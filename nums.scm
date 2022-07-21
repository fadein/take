(import (chicken base))
(import (chicken string))


(define (string?->symbol str?)
  (cond
    ((string->number str?))
    ((string->symbol str?))))

; this string should be broken into words on spaces and punctuation,
; converted to lower case and made into symbols
; "seven hundred fifty four thousand six hundred thirty three"
(define (condition-input str)
  (map string?->symbol (string-split (string-downcase str) " -,.")))

(define (c str)
  (condition-input str))

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

      ((number? (car xs))
       (cons (car xs) (helper (cdr xs))))

      ((assq (car xs) name->value) => (lambda (p)
                                        (cons (cdr p) (helper (cdr xs)))))

      (else (helper (cdr xs)))))
  (flatten (helper xs)))


(define (pow10? x)
  (memq x '(10 100 1000 10000 1000000 10000000 100000000 1000000000 10000000000 100000000000 1000000000000)))

(define (pow1000? x)
  (memq x '(1000 1000000 1000000000 1000000000000)))

(define (within x lo hi)
  (<= lo x hi))

(define (parse str)
  (let helper ((lst (symbols->numbers (condition-input str))) (accum 0) (tot 0))
    (cond
      ((null? lst) ; case 1
       (print "case 1")  ; DELETE ME
       (+ tot accum))
      ((= 1 (length lst))
       (let ((no1 (car lst)))
         (if (pow1000? no1)
           (begin
             (printf "case 2 no1:~a lst:~a accum:~a tot:~a~n" no1 lst accum tot)  ; DELETE ME
             (+ tot (* accum no1)))  ; case 2
           (begin
             (printf "case 3 no1:~a lst:~a accum:~a tot:~a~n" no1 lst accum tot)  ; DELETE ME
             (+ tot accum no1))))) ; case 3 - not sure why I considered these two cases separately on paper...
      (else
        (let ((no1 (car lst))
              (no2 (cadr lst)))
          (cond
            ((pow1000? no1)  ; case 4
             (printf "case 4 no1:~a no2:~a lst:~a accum:~a tot:~a~n" no1 no2 lst accum tot)  ; DELETE ME
             (helper (cdr lst) 0 (+ tot (* no1 accum))))

            ((and (<= 1 no1 9) (pow1000? no2))  ; case 6 
             (printf "case 6 no1:~a no2:~a lst:~a accum:~a tot:~a~n" no1 no2 lst accum tot)  ; DELETE ME
             (helper (cddr lst) 0 
                     (+ tot (* (+ accum no1) no2))))

            ((and (<= 1 no1 9) (pow10? no2))  ; case 5
             (printf "case 5 no1:~a no2:~a lst:~a accum:~a tot:~a~n" no1 no2 lst accum tot)  ; DELETE ME
             (helper (cddr lst) (+ accum (* no1 no2)) tot))

            (else
              (printf "(helper no1:~a no2:~a lst:~a accum:~a tot:~a~n" no1 no2 lst accum tot)
              (error "how did this happen?"))))))))

