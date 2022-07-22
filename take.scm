#!/usr/bin/csi -s

(import (chicken base))
(import (chicken format))  ; DELETE ME
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


(define *VERSION* "2.0")
(define (usage)
  (print "take v" *VERSION*
         "\n\nUsage: take 5 minutes 20 seconds to ... then take 30 seconds to ...")
  (exit 1))


(define (symbols->numbers xs)
  (define name->value '((zero 0)
                        (one 1)
                        (two 2)
                        (three 3)
                        (four 4)
                        (five 5)
                        (six 6)
                        (seven 7)
                        (eight 8)
                        (nine 9)
                        (ten 10)
                        (eleven 1 10 1)
                        (twelve 1 10 2)
                        (thirteen 1 10 3)
                        (fourteen 1 10 4)
                        (fifteen 1 10 5)
                        (sixteen 1 10 6)
                        (seventeen 1 10 7)
                        (eightteen 1 10 8)
                        (nineteen 1 10 9)
                        (twenty 2 10)
                        (thirty 3 10)
                        (forty 4 10)
                        (fifty 5 10)
                        (sixty 6 10)
                        (seventy 7 10)
                        (eighty 8 10)
                        (ninety 9 10)
                        (hundred 100)
                        (thousand 1000)
                        (million 1000000)
                        (billion 1000000000)
                        (trillion 1000000000000)))

  (define (helper xs)
    (cond
      ((null? xs)
       '())

      ((string->number (car xs))
       => (lambda (num)
            (cons num (helper (cdr xs)))))

      ((assq (string->symbol (car xs)) name->value)
       => (lambda (p)
            (cons (cdr p) (helper (cdr xs)))))

      (else
        (helper (cdr xs)))))

  (flatten (helper xs)))


(define (words->numbers words)
  ; TODO - there's got to be a better way to do this
  (define (pow10? x)
    (memq x '(10 100 1000 10000 1000000 10000000 100000000 1000000000 10000000000 100000000000 1000000000000)))

  ; TODO - there's got to be a better way to do this
  (define (pow1000? x)
    (memq x '(1000 1000000 1000000000 1000000000000)))

  (let helper ((lst (symbols->numbers words)) (accum 0) (tot 0))
    (cond
      ((null? lst) ; case 1
       (+ tot accum))
      ((= 1 (length lst))
       (let ((no1 (car lst)))
         (if (pow1000? no1)
           (begin
             (+ tot (* accum no1)))  ; case 2
           (begin
             (+ tot accum no1))))) ; case 3 - not sure why I considered these two cases separately on paper...
      (else
        (let ((no1 (car lst))
              (no2 (cadr lst)))
          (cond
            ((pow1000? no1)  ; case 4
             (helper (cdr lst) 0 (+ tot (* no1 accum))))

            ((and (<= 1 no1 9) (pow1000? no2))  ; case 6 
             (helper (cddr lst) 0 
                     (+ tot (* (+ accum no1) no2))))

            ((and (<= 1 no1 9) (pow10? no2))  ; case 5
             (helper (cddr lst) (+ accum (* no1 no2)) tot))

            (else
              (printf "(helper no1:~a no2:~a lst:~a accum:~a tot:~a~n" no1 no2 lst accum tot)
              (error "how did this happen?"))))))))

(define (timespec->seconds timespec)
  ; condition input for the timespec->seconds function
  (define (ci-ts str)
    (flatten (map (lambda (s) (string-split (string-downcase s) " -,.")) str)))

  (let helper ((timespec (ci-ts timespec)) (accum '()) (total-seconds 0))
    (cond
      ((null? timespec)
       total-seconds)

      ; if (car timespec) matches HH:MM:SS or MM:SS, convert it to seconds and
      ;   immediately return its value, discarding the remaining timespec
      ;   (other timespec info after an absolute and complete HH:MM:SS doesn't
      ;   really make sense)
      ((irregex-search "(\\d\\d):(\\d\\d)(:(\\d\\d))" (car timespec))
       => (lambda (match)
            (if (irregex-match-substring match 4)
              ; have all three of HH:MM:SS
              (+ (* 3600 (string->number (irregex-match-substring match 1)))
                 (*   60 (string->number (irregex-match-substring match 2)))
                         (string->number (irregex-match-substring match 4)))
              ; else, timespec is MM:SS
              (+ (* 60 (string->number (irregex-match-substring match 1)))
                       (string->number (irregex-match-substring match 2))))))

      ; if (car timespec) is one of "seconds" "minutes" "hours" "days", etc.,
      ;   process (reverse accum) with symbols->numbers, then multiply by the time type,
      ;   then add to total-seconds & loop
      ((assoc (car timespec) '(("days" . 86400) ("day" . 86400) ("d" . 86400)
                               ("hours" . 3600) ("hour" . 3600) ("hr" . 3600) ("hrs" . 3600) ("h" . 3600)
                               ("minutes" . 60) ("minute" . 60) ("min" . 60) ("mins" . 60) ("m" . 60)
                               ("seconds" . 1) ("second" . 1) ("sec" . 1) ("secs" . 1) ("s" . 1))
              string-ci=) =>
       (lambda (multiplier)
         (let* ((number (words->numbers (reverse accum)))
                (seconds (* number (cdr multiplier))))
           (helper (cdr timespec) '() (+ total-seconds seconds)))))

      ; else, append (car timespec) to accum & loop
      (else
        (helper (cdr timespec) (cons (car timespec) accum) total-seconds)))))

; predicate for use with (srfi-1 break)
;   break a list into a timespec & everything following
;   a timespec ends at the words "to"
(define (recognize-timespec? item)
  (string-ci=? item "to"))

; predicate for use with (srfi-1 break)
;   break a list into an action & everything following
;   an action ends at the words "take" or "then"
(define (recognize-action? item)
  (or (string-ci=? item "take")
      (string-ci=? item "then")))


(define (process-timespec words)
  (let-values (((timespec rest) (break recognize-timespec? words)))
    (let ((seconds (timespec->seconds timespec)))
      (values seconds rest))))

(define (process-action words)
  (break recognize-action? words))

(define (process-args args)
  (if (null? args)
    '()
    (let-values (((seconds rest) (process-timespec args)))
      (let-values (((action rest) (process-action rest)))
        (cons (list (list 'time seconds) action) (process-args rest))))))


; Parse the command-line arguments into a list of alists
(define (parse-command-line args)
  (let ((directives (process-args args)))
    (if (null? directives)
        (usage)
          directives)))


;; Convert int seconds into string timestamp in the form of M:S or H:M:S with
;; blinking colons
(define (seconds->timestamp sec)
  (define zero-pad
    (lambda (n)
      (if (< -1 n 10)
        (string-concatenate `("0" ,(number->string n)))
        (number->string n))))
  (let ((sec (inexact->exact (truncate sec))))
    (let* ((hours (quotient sec 3600))
           (rem (remainder sec 3600))
           (mins (quotient rem 60))
           (sec (remainder rem 60))
           (h?ms (if (zero? hours)
                     (list mins sec)
                     (list hours mins sec))))
      (string-concatenate
        (let build ((l h?ms))
          (if (null? (cdr l))
            (cons (zero-pad (car l)) '())
            (cons (zero-pad (car l)) (cons (if (even? sec) ":" ".") (build (cdr l))))))))))


;; Given a list of directives alists, print the message and countdown for the given time
(define (process directives)
  (define counters
    (map (lambda (color) (lambda (seconds to-do) (countdown seconds to-do color)))
         '(fg-red fg-green fg-yellow fg-blue fg-magenta fg-cyan fg-white)))

  (define (process-h directives)
    (when (not (null? directives))
      (let* ((this (car directives))
             (seconds (cadr (assq 'time this)))
             (to-do (string-join (cdr (assq 'to this)))))
        (print* (set-title to-do))
        ; choose a counting display function at random
        (set! *cancel-countdown* #f)
        ((list-ref counters (pseudo-random-integer (length counters))) seconds to-do)

        ; ring the bell thrice if this countdown wasn't cancelled
        (unless *cancel-countdown*
          (do ((i 3 (sub1 i)))
            ((zero? i) (newline))
            (print* "\a") (sleep 1)))

      (process-h (cdr directives)))))

  (print* (hide-cursor))
  (process-h directives)
  (cleanup! 'normal-exit))


(define (countdown seconds to-do #!optional (color 'fg-white))
  (define restart #f)

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

                 ; add or subtract 5% of the timer
                 ((#\+ #\=)
                  (set! time-left
                    (min seconds (+ time-left (inexact->exact (round (* seconds 0.05)))))))

                 ((#\- #\_)
                  (set! time-left
                    (max 0 (- time-left (inexact->exact (round (* seconds 0.05)))))))

                 ((#\q #\Q)
                  (cleanup! 'quit))))

        (let* (; re-calculate the screen width on each update
               (secs-per-col (/ *cols* seconds))
               ; form the message, padded with spaces, putting the reverse attr in the right place
               (msg (string-pad-right to-do (- *cols* 6)))
               (line (string-concatenate (list (seconds->timestamp time-left) " " msg)))
               (bar-width (truncate (* time-left secs-per-col)))
               (reversed (set-text `(reverse-video bold ,color) (string-take line bar-width)))
               (regular  (set-text `(bold ,color)               (string-drop line bar-width))))
          (print* "\r" (erase-line) reversed regular))
        (sleep 1)
        (if *winched*
          (begin
            (set! *winched* #f)
            (loop time-left))
          (loop (sub1 time-left))))))



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


; -icanon disables the terminal's line-buffering
(with-stty '(not icanon echo)
           (lambda ()

             (let ((directives (parse-command-line (command-line-arguments))))
               (print "directives:" directives)  ; DELETE ME
               (print "(caadar directives) => " (caadar directives) " is a "
                      (cond
                        ((string? (caadar directives)) "string")
                        ((symbol? (caadar directives)) "symbol")
                        (else "...something else")))
               (process directives))
             (print* (show-cursor))))

; vim: set expandtab:
