#!/usr/bin/csi -s

(import ansi-escape-sequences
        srfi-1
        srfi-13
        srfi-14
        (chicken port)
        (chicken process signal)
        (chicken process-context)
        (chicken random)
        unicode-utils)


(define *VERSION* "0.3")
(define (usage)
  (print "take v" *VERSION*
         "\nUsage: take 5 minutes 20 seconds to ... then take 30 seconds to ...")
  (exit 1))



; Dimensions of the terminal
(define *rows*)
(define *cols*)
(define (window-size-changed! signal)
  (let-values (((rows cols) (terminal-size (current-output-port))))
	(set! *rows* rows)
	(set! *cols* cols)))


; Parse the command-line arguments into a list of alists
(define (parse-command-line args)
  ; Prepare the command-line arguments by removing punctuation
  (define cs:punct-minus- (char-set-delete char-set:punctuation #\-))

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
    (map (lambda (color) (lambda (seconds to-do) (reverse-countdown seconds to-do color)))
         '(fg-red fg-green fg-yellow fg-blue fg-magenta fg-cyan fg-white)))

  (define (process-h directives)
    (when (not (null? directives))
      (let* ((this (car directives))
             (seconds (cadr (assq 'time this)))
             (to-do (string-join (cdr (assq 'to this)))))
        (print* (set-title to-do))
        ; choose a counting display function at random
        ((list-ref counters (pseudo-random-integer (length counters))) seconds to-do)

        ; ring the bell thrice
        (do ((i 3 (sub1 i)))
          ((zero? i) (newline))
          (print* "\a") (sleep 1))

      (process-h (cdr directives)))))

  (print* (hide-cursor))
  (process-h directives)
  (print* (show-cursor)))


(define (simple-countdown seconds to-do)
  (let loop ((seconds seconds))
    (when (>= seconds 0)
      (print*
        "\r"
        (erase-line)
        (seconds->timestamp seconds)
        " " to-do)
      (sleep 1)
      (loop (sub1 seconds)))))


(define (bar-countup seconds to-do)
  (let* ((msg-width (+ 6 (string-length to-do)))
         (free-spaces (- *cols* msg-width))
         (secs-per-col (/ free-spaces seconds )))
  (let loop ((seconds seconds))
    (when (>= seconds 0)
      (print*
        "\r"
        (erase-line)
        (seconds->timestamp seconds)
        " " to-do
        (unicode-make-string (- free-spaces (truncate (* seconds secs-per-col))) #\u2588))
      (sleep 1)
      (loop (sub1 seconds))))))


(define (bar-countdown seconds to-do)
  (let* ((msg-width (+ 6 (string-length to-do)))
         (free-spaces (- *cols* msg-width))
         (secs-per-col (/ free-spaces seconds )))

  (let loop ((seconds seconds))
    (when (>= seconds 0)
      (print*
        "\r"
        (erase-line)
        (seconds->timestamp seconds)
        " " to-do
        (unicode-make-string (truncate (* seconds secs-per-col)) #\u2588))
      (sleep 1)
      (loop (sub1 seconds))))))


(define (reverse-countdown seconds to-do #!optional (color 'fg-white))
  (let* ((secs-per-col (/ *cols* seconds))
         (msg (string-pad-right to-do (- *cols* 6))))
    (let loop ((seconds seconds))
      (when (>= seconds 0)
        ; form the string, padded with spaces, putting the reverse attr in the right place
        (let* ((line (string-concatenate (list (seconds->timestamp seconds) " " msg)))
               (bar-width (truncate (* seconds secs-per-col)))
               (reversed (set-text `(reverse-video bold ,color) (string-take line bar-width)))
               (regular  (set-text `(bold ,color)               (string-drop line bar-width))))
          (print* "\r" (erase-line) reversed regular))
        (sleep 1)
        (loop (sub1 seconds))))))


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


;;; main body of code

;; restore the cursor when this program is interrupted
(define (cleanup signal)
  (print (show-cursor))
  (exit))
(for-each (lambda (s) (set-signal-handler! s cleanup))
          (list signal/term signal/int signal/pipe signal/quit))
(set-signal-handler! signal/winch window-size-changed!)
(window-size-changed! #f)

;(print "The window is " *cols* "x" *rows*)  ; DELETE ME


;; do your thing
;(import (chicken pretty-print))  ; DELETE ME
;(pretty-print (parse-command-line (command-line-arguments)))  ; DELETE ME
(process (parse-command-line (command-line-arguments)))
