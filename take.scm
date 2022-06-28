#!/usr/bin/csi -s

(import (chicken io))
(import (chicken port))
(import (chicken process signal))
(import (chicken process-context))
(import (chicken random))
(import (chicken time))

(import ansi-escape-sequences)
(import miscmacros)
(import srfi-1)
(import srfi-13)
(import srfi-14)
(import stty)


(define *VERSION* "1.1")
(define (usage)
  (print "take v" *VERSION*
         "\n\nUsage: take 5 minutes 20 seconds to ... then take 30 seconds to ...")
  (exit 1))



; Parse the command-line arguments into a list of alists
(define (parse-command-line args)
  ; Prepare the command-line arguments by removing (most) punctuation
  (define cs:punct-minus- (char-set-delete char-set:punctuation #\[ #\] #\( #\) #\/ #\' #\& #\- #\:))

  (let* ((args (map (lambda (s) (string-delete cs:punct-minus- s)) args))
         (directives (state0 args)))
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


; parse command-line arguments given in the form of "take 5 minutes to ... then take 30 seconds to ..."
;
; '(((time 300) (to ...))
;   ((time 30) (to ...)))
; TODO - these functions need better names
(define (state0 args)
  ;(print "state0:" args)   ; DELETE ME
  ;; look for a number followed by a time quantifier, skip over (filler) words like "then", "take"
  (if (null? args)
      '()
      (let* ((this-arg (car args))
             (this-num (string->number this-arg)))
        (cond
          ;; keep numeric quantities, look for a time specifier following the
          ;; qunantity and convert into seconds
          ((and (number? this-num) (>= this-num 0) (not (null? (cdr args))))
           (let-values (((time-spec)
                         (* this-num
                            (case (string->symbol (cadr args))
                              ((minute minutes)   60)
                              ((second seconds)    1)
                              ((hour   hours)   3600)
                              ((day    days)   86400)
                              (else                1))))
                        ((action consumed) (state1 (cddr args))))

             ;(print "from state1 I got (" action " " consumed ")" action consumed)  ; DELETE ME
             ;(print "and args is " args) ; DELETE ME

             (if (and (null? action) (zero? consumed) (not (null? (cddr args))))
                 ;; when more than one time specification are given in a row without
                 ;; an intervening action we add them up
                 (begin
                   ;(print "an incomplete timespec, parsing further") (sleep 1) ; DELETE ME
                        (let* ((the-rest (state0 (cddr args)))
                               (this-one (car the-rest))
                               (i-time-spec (assq 'time this-one)))
                          ;(print "the-rest:" the-rest)  ; DELETE ME
                          ;(print "this-one:" this-one)  ; DELETE ME
                          ;(print "i-time-spec:" i-time-spec)  ; DELETE ME
                          (set-car! (cdr i-time-spec) (+ (cadr i-time-spec) time-spec))
                          the-rest))
             (cons `((time ,time-spec) (to ,@action))
                   (state0 (drop (cddr args) consumed))))))

          (else
            ;(print "state0: else")  ; DELETE ME
            (state0 (cdr args)))))))


(define (state1 args)
  (define (state1-helper args consumed)
    ;(print "  state1:" args)   ; DELETE ME

    ; remove an initial "to"
    (if (and (not (null? args)) (string-ci= (car args) "to"))
        (state1-helper (cdr args) (add1 consumed))

        (let loop ((args args) (action '()) (consumed consumed))
          (if (null? args)
              (values (reverse action) consumed)

              (let* ((this-arg (car args))
                     (this-num (string->number this-arg))
                     (this-sym (string->symbol this-arg)))

                (cond
                  ;; if I see a number, transition back to state0 without consuming it
                  ((number? this-num)
                   (values (reverse action) consumed))

                  ;; skip over filler words like "take", "then", "to"
                  ((or (string-ci= this-arg "take")
                       (string-ci= this-arg "then"))
                   (loop (cdr args) action (add1 consumed)))

                  ;; else, accumulate into a list
                  (else
                    (loop (cdr args) (cons this-arg action) (add1 consumed)))))))))

  (state1-helper args 0))



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
             (process (parse-command-line (command-line-arguments)))
             (print* (show-cursor))))

; vim: set expandtab:
