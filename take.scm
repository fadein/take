#!/bin/env -S csi -ss

(define *VERSION* "3.0")

(import (chicken base))
(import (chicken format))
(import (chicken io))
(import (chicken irregex))
(import (chicken port))
(import (chicken process signal))
(import (chicken process-context))
(import (chicken random))
(import (chicken string))
(import (chicken time))

(import ansi-escape-sequences)
(import miscmacros)
(import srfi-1)
(import srfi-13)
(import srfi-14)
(import stty)

(include "words-to-numbers.scm") (import words-to-numbers)


(define (usage)
  (print "take v" *VERSION*
         "\n\nUsage: take five minutes 20 seconds to ... then take thirty seconds to ..."
           "\n       take a budget of 7 minutes then take 30% to ... then take 70% to ...")
  (exit 1))


;; Global variable to store the budget
(define *budget* #f)


;; Detect if a timespec is a percentage value
;; Returns percentage-value or #f
(define (detect-percentage word)
  (let ((word (string-downcase word)))
    (cond
      ;; Handle numeric percentages like "50%"
      ((irregex-search "^(\\d+(?:\\.\\d+)?)%$" word)
       => (lambda (match)
            (string->number (irregex-match-substring match 1))))
      ;; Handle word-based percentages like "fifty%" or "twenty three%"
      ((irregex-search "^(.*)%$" word)
       => (lambda (match)
            (let ((num-words (string-split (irregex-match-substring match 1) " -,")))
              (words->numbers num-words))))
      (else
        #f))))


;; Convert a human time specification into seconds.
;; Input numbers can be any mixture of integers, floats, or words.
;; Unrecognized words are silently ignored, allowing for the use of filler words like "and"
;; Examples include:
;;   2.5 days
;;   3.14159 hours
;;   four minutes thirty seconds
;;   3 days 12.5 hours and fifteen seconds
;;   50% (proportional timespec)
;;   twenty three% (proportional timespec)
(define (timespec->seconds timespec)
  (define (ci-ts str)
    ;; massage input for the enclosing function
    (flatten (map (lambda (s) (string-split (string-downcase s) " -,")) str)))

  (let helper ((timespec (ci-ts timespec)) (accum '()) (total-seconds 0))
    (cond
      ((null? timespec)
       total-seconds)

      ;; Check for percentage value
      ((detect-percentage (car timespec))
       => (lambda (percent) `(proportional ,percent)))

      ; if (car timespec) matches HH:MM:SS or MM:SS, convert it to seconds and
      ;   immediately return its value, discarding the remaining timespec
      ;   (other timespec info after an absolute and complete HH:MM:SS doesn't
      ;   really make sense)
      ((irregex-search "(\\d?\\d):(\\d\\d)(:(\\d\\d))?" (car timespec))
       => (lambda (match)
            (if (irregex-match-substring match 4)
              ; have all three of HH:MM:SS
              (+ (* 3600 (string->number (irregex-match-substring match 1)))
                 (*   60 (string->number (irregex-match-substring match 2))
                         (string->number (irregex-match-substring match 4))))
              ; else, timespec is MM:SS
              (+ (* 60 (string->number (irregex-match-substring match 1))
                       (string->number (irregex-match-substring match 2)))))))

      ; if (car timespec) is one of "seconds" "minutes" "hours" "days", etc.,
      ;   process (reverse accum) with symbols->numbers, then multiply by the time type,
      ;   then add to total-seconds & loop
      ((assoc (car timespec) '(("days" . 86400) ("day" . 86400) ("d" . 86400)
                               ("hours" . 3600) ("hour" . 3600) ("hr" . 3600) ("hrs" . 3600) ("h" . 3600)
                               ("minutes" . 60) ("minute" . 60) ("min" . 60) ("mins" . 60) ("m" . 60)
                               ("seconds" . 1) ("second" . 1) ("sec" . 1) ("secs" . 1) ("s" . 1))
              string-ci=)
       => (lambda (multiplier)
            (let* ((number (words->numbers (reverse accum)))
                   (seconds (inexact->exact (round (* number (cdr multiplier))))))
              (helper (cdr timespec) '() (+ total-seconds seconds)))))

      ; else, append (car timespec) to accum & loop
      (else
        (helper (cdr timespec) (cons (car timespec) accum) total-seconds)))))


; predicate for use with (srfi-1 break)
;   break a list into a timespec & everything following
;   a timespec ends at the words "to" or "for"
(define (recognize-timespec? item)
  (or (string-ci=? item "to")
      (string-ci=? item "for")))

(define (process-timespec words)
  (let-values (((timespec rest) (break recognize-timespec? words)))
    (let ((seconds (timespec->seconds timespec)))
      (values seconds rest))))


; predicate for use with (srfi-1 break)
;   break a list into an action & everything following
;   an action ends at the words "take" or "then"
(define (recognize-action? item)
  (or (string-ci=? item "take")
      (string-ci=? item "then")))

(define (process-action words)
  ; this is pure vanity - strip any trailing comma
  (define (list-strip-trailing-comma! head)
    (let loop ((lst head))
      (cond
        ((null? lst) head)
        ((null? (cdr lst))
         (set-car! lst (string-trim-right (car lst) (char-set #\: #\,)))
         head)
        (else
          (loop (cdr lst))))))
  (let-values (((action rest) (break recognize-action? words)))
    (values
      (if (null? action)
        '("to" "")
        (list-strip-trailing-comma! action))
      rest)))


;; Detect if a timespec is a budget specification
;; Returns (values is-budget? budget-seconds rest-words)
(define (detect-budget words)
  (let loop ((words words) (accum '()))
    (cond
      ((null? words)
       (values #f 0 accum))
      ((and (> (length words) 1)
            (string-ci=? (car words) "budget"))
       (let-values (((budget rest) (break recognize-action? (cdr words))))
         (let-values (((timespec _) (break recognize-timespec? budget)))
           (values #t (timespec->seconds timespec) (append (reverse accum) rest)))))
      (else
        (loop (cdr words) (cons (car words) accum))))))


;; Calculate remaining budget after fixed timespecs
;; Returns (values remaining-budget fixed-total)
(define (calculate-fixed-budget directives)
  (let loop ((directives directives) (fixed-total 0))
    (if (null? directives)
      (values (if *budget* (- *budget* fixed-total) #f) fixed-total)
      (begin
        (let* ((this (car directives))
               (fixed-time (assq 'time this)))
          (if fixed-time
            (loop (cdr directives) (+ fixed-total (cadr fixed-time)))
            (loop (cdr directives) fixed-total)))))))


;; Apply proportional timespecs to original budget, capping at remaining budget
;; Returns list of directives with calculated seconds
(define (apply-proportional-timespecs directives remaining-budget)
  (let loop ((directives directives) (remaining remaining-budget) (fixed-total 0))
    (cond
      ((null? directives)
       (emit-budget-warnings fixed-total (or remaining 0))
       '())
      (else
        (let* ((this (car directives))
               (rest (cdr directives))
               (proportional (assq 'proportional this)))
          (if proportional
            (let* ((percent (cadr proportional))
                   (requested-seconds (inexact->exact (round (* *budget* (/ percent 100)))))
                   (actual-seconds (min requested-seconds remaining))
                   (to-do (string-join (cdr (or (assoc "to" this)
                                                (assoc "for" this))))))
              (cond
                ((and (< actual-seconds requested-seconds) (> actual-seconds 0))
                 (fprintf (current-error-port)
                          "Warning: '~a' shortened from ~a to ~a due to budget constraints\n"
                          to-do
                          (seconds->timestamp requested-seconds #f)
                          (seconds->timestamp actual-seconds #f))
                 (cons `((time ,actual-seconds) ,@(cdr this))
                       (loop rest (- remaining actual-seconds) (+ fixed-total actual-seconds))))
                ((<= actual-seconds 0)
                 (fprintf (current-error-port) "Warning: No time left for '~a'\n" to-do)
                 (cons `((time 0) ,@(cdr this))
                       (loop rest (- remaining actual-seconds) (+ fixed-total actual-seconds))))
                (else
                  (cons `((time ,actual-seconds) ,@(cdr this))
                        (loop rest (- remaining actual-seconds) (+ fixed-total actual-seconds))))))
            (cons this (loop rest remaining (+ (cadr (assq 'time this)) fixed-total)))))))))


;; Emit budget-related warnings to stderr
(define (emit-budget-warnings fixed-total remaining-budget)
  (when *budget*
    (let ((port (current-error-port)))
      (cond
        ((< remaining-budget 0)
         (fprintf port "Warning: Fixed timespecs exceed budget by ~a\n"
                  (seconds->timestamp (abs remaining-budget) #f)))
        ((> remaining-budget 0)
         (fprintf port "Warning: ~a of budget remains unused\n"
                  (seconds->timestamp remaining-budget #f)))))))


(define (argv->directives argv)
  ;; First pass: look for budget specification
  (let-values (((is-budget? budget-seconds rest) (detect-budget argv)))
    (when is-budget?
      (set! *budget* budget-seconds)
      (set! argv rest))

    (if (null? argv)
      '()
      (let-values (((seconds rest) (process-timespec argv)))
        (let-values (((action rest) (process-action rest)))
          (cond
            ((and (number? seconds) (zero? seconds))
             (argv->directives rest))
            ((and (pair? seconds) (eq? (car seconds) 'proportional))
             (cons `(,seconds ,action) (argv->directives rest)))
            (else
              (cons `((time ,seconds) ,action) (argv->directives rest)))))))))


;; Convert int seconds into string timestamp in the form of M:S or H:M:S with
;; blinking colons.  Uses helper function zero-pad
(define (seconds->timestamp sec #!optional (blink? #t))
  (define (zero-pad n)
    (if (<= 0 n 9)
      (sprintf "0~a" (number->string n))
      (number->string n)))
  (let* ((sec (inexact->exact (truncate sec)))
         (hours (quotient sec 3600))
         (rem (remainder sec 3600))
         (mins (quotient rem 60))
         (sec (remainder rem 60))
         (h?ms (if (zero? hours)
                 (list mins sec)
                 (list hours mins sec))))
    (string-intersperse (map zero-pad h?ms) (if (and blink? (odd? sec)) "." ":"))))


;; Given a list of directives alists, print the message and countdown for the given time
(define (process directives)

  (define counters
    (map (lambda (color) (lambda (seconds to-do) (countdown seconds to-do color)))
         '(fg-red fg-green fg-yellow fg-blue fg-magenta fg-cyan fg-white)))

  ;; pick an element from lst at random
  (define (random-choice lst)
    (list-ref lst (pseudo-random-integer (length lst))))


  ;; Calculate fixed budget and apply proportional timespecs
  (let-values (((remaining-budget fixed-total) (calculate-fixed-budget directives)))
    (let ((processed-directives
            (if *budget*
              (apply-proportional-timespecs directives remaining-budget)
              directives)))

      (define (process* directives)
        (when (not (null? directives))
          (let* ((this (car directives))
                 (seconds (cadr (assq 'time this)))
                 (to-do (string-join (cdr (or (assoc "to" this)
                                              (assoc "for" this))))))
            (when (> seconds 0)  ; Skip 0-second tasks
              (print* (set-title to-do))
              (set! *cancel-countdown* #f)

              ; run a countdown display function chosen at random
              ((random-choice counters) seconds to-do)

              ; ring the bell thrice if this countdown wasn't cancelled
              (unless *cancel-countdown*
                (do ((i 3 (sub1 i)))
                  ((zero? i))  ; quit when i=0
                  (print* "\a") (sleep 1)))
              (newline))

            (process* (cdr directives)))))

      (print* (hide-cursor))
      (process* processed-directives)
      (cleanup! 'normal-exit))))


(define (countdown seconds to-do #!optional (color 'fg-white))
  (define restart #f)
  (define paused? #f)

  ;; The lambda invoked by call/cc sets time-left to the initial value of 'seconds'
  ;; and assigns this continuation to the name 'restart'
  (let loop ((time-left (call/cc (lambda (k) (set! restart k) seconds))))
    (when (and (>= time-left 0) (not *cancel-countdown*))

      (while (char-ready? (current-input-port))
             (case (read-char)
               ((#\return #\newline #\r #\R)
                (while (char-ready? (current-input-port)) (read-char))  ; drain STDIN
                (restart seconds))

               ((#\z #\Z #\0)
                (set! *cancel-countdown* #t)
                (set! time-left 0))

               ; add 5% to the timer (+ 1 second to make up for this keypress)
               ((#\+ #\=)
                (set! time-left
                  (min seconds (+ 1 time-left (inexact->exact (round (* seconds 0.05)))))))

               ; subtract 5% from the timer
               ((#\- #\_)
                (set! time-left
                  (max 0 (- time-left (inexact->exact (round (* seconds 0.05)))))))

               ((#\p #\P #\space)
                (set! paused? (not paused?)))

               ((#\q #\Q)
                (cleanup! 'quit))))

      (let* (; re-calculate the screen width on each update
             (secs-per-col (/ *cols* seconds))
             ; form the message, padded with spaces, putting the reverse attr in the right place
             (timestamp (seconds->timestamp time-left))
             (msg (string-pad-right
                    (if paused? (conc "[PAUSED] " to-do) to-do)
                    (- *cols* (add1 (string-length timestamp)))))
             (line (string-append timestamp " " msg))
             (bar-width (truncate (* time-left secs-per-col)))
             (reversed (set-text `(reverse-video bold ,color) (string-take line bar-width)))
             (regular  (set-text `(bold ,color)               (string-drop line bar-width))))
        (print* "\r" (erase-line) reversed regular))
      (sleep 1)
      (if *winched*
        (begin
          (set! *winched* #f)
          (loop time-left))
        (loop (if paused? time-left (sub1 time-left)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAIN BODY OF CODE ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; signal handlers
(define *cancel-countdown* #f)
(define *last-interrupt* 0)

; skip the current timer; exit if this is invoked twice within 1 second
(define (skip-countdown signal)
  (let ((now (current-seconds)))
    (cond
      ((zero? (- now *last-interrupt*))
       (cleanup! 'skipped-timer))
      (else
        (set! *cancel-countdown* #t)
        (set! *last-interrupt* now)))))
(set-signal-handler! signal/int skip-countdown)


;; restore the cursor when this program is interrupted
(define (cleanup! unused)
  (stty '(icanon echo))
  (print* (show-cursor))
  (exit))

(for-each (lambda (s) (set-signal-handler! s cleanup!))
          (list signal/term signal/pipe signal/quit))


;; Update the dimensions of the terminal upon receipt of SIGWINCH
;;
;; SIGWINCH can interrut the sleep function, and is continually fired as the
;; terminal window is resized which quickly drains the timer.
;;
;; When *winched* has a truthy value the timer doesn't decrement
(define *winched* #f)
(define *rows*)
(define *cols*)
(define (window-size-changed! sig)
  (let-values (((rows cols) (terminal-size (current-output-port))))
    (set! *rows* rows)
    (set! *cols* cols))
  (set! *winched* sig))
(set-signal-handler! signal/winch window-size-changed!)
(window-size-changed! #f)

(define (main argv)
  ; -icanon disables the terminal's line-buffering
  (with-stty '(not icanon echo)
             (lambda ()
               ; Parse the command-line arguments into a list of alists
               (let ((directives (argv->directives argv)))
                 (if (null? directives)
                   (usage)
                   (process directives)))
               (print* (show-cursor)))))

(cond-expand
  (compiling (main (command-line-arguments)))
  (else #f))

; vim: set expandtab:
