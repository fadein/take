(import (chicken base))
(import (chicken format))
(import (chicken irregex))
(import (chicken string))
(import (only srfi-1 assoc break))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CODE to be deleted


; this string should be broken into words on spaces and punctuation,
; converted to lower case and made into symbols
; "take ten minutes and thirty-three seconds to cruise with wifey then take seven minutes fifty-five seconds to eat yummy food"
;  => (((time 633) (to cruise with wifey)) ((time 475) (to eat yummy food)))
;
;  First, I need to identify which tokens are time words
;  either find the last time word, or look for a special token such as "to" or "and"
(define test0 (string-split "Take TEN minutes AND THIRTY-THREE seconds to cruise with wifey then take seven minutes fifty-five seconds to eat yummy food"))

(define test1 (string-split "Take TEN minutes, THIRTY-THREE seconds to cruise with wifey then take seven min, 55 secs to eat yummy food"))

(define test2 (string-split "TEN m, THIRTY THREE seconds to cruise with wifey then take seven min, 55 secs to eat yummy food"))
