(module words-to-numbers
        (words->numbers symbols->numbers)

(import srfi-1)
(import (chicken base))
(import (chicken io))
(import (chicken random))
(import scheme)


;; manually randomize the PRNG from /dev/urandom otherwise the current $SECONDS
;; are used, resulting the stable outputs when the program is run quickly
;; back-to-back
(define (seed-prng-from-urandom!)
  (set-pseudo-random-seed!
    (with-input-from-file "/dev/urandom" (lambda () (read-string 8)))))

;; Return a number in the range [i, j)
(define (a-number-between i j)
  (let ((spread (add1 (abs (- j i)))))
    (+ (min i j) (pseudo-random-integer spread))))


(define (symbols->numbers xs)
  (let ((precise-name->value
          '((zero 0)
            (one 1)
            (a 1)
            (an 1)
            (two 2)
            (couple 2)
            (pair 2)
            (three 3)
            (few 3)
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
            (gross 144)
            (thousand 1000)
            (million 1000000)
            (billion 1000000000)
            (trillion 1000000000000)))

        (fuzzy-name->value 
          `((few . ,(lambda () (a-number-between 3 5)))
            (several . ,(lambda () (a-number-between 4 7)))
            (some . ,(lambda () (a-number-between 4 7)))
            (pack . ,(lambda () (a-number-between 5 21)))
            (bunch . ,(lambda () (a-number-between 8 15)))
            (many . ,(lambda () (a-number-between 8 15)))
            (lots . ,(lambda () (a-number-between 8 15)))
            (mess . ,(lambda () (a-number-between 20 55)))
            (grundle . ,(lambda () (a-number-between 20 55))))))

    (define (helper xs)
      (cond
        ((null? xs)
         '())

        ((string->number (car xs))
         => (lambda (num)
              (cons num (helper (cdr xs)))))

        ((assq (string->symbol (car xs)) precise-name->value)
         => (lambda (p)
              (cons (cdr p) (helper (cdr xs)))))

        ((assq (string->symbol (car xs)) fuzzy-name->value)
         => (lambda (p)
              (cons ((cdr p)) (helper (cdr xs)))))

        (else
          (helper (cdr xs)))))

    (flatten (helper xs))))


; TODO - there's got to be a better way to do this
(define (pow10? x)
  (memq x '(10 100 1000 10000 1000000 10000000 100000000 1000000000 10000000000 100000000000 1000000000000)))


; TODO - there's got to be a better way to do this
(define (pow1000? x)
  (memq x '(1000 1000000 1000000000 1000000000000)))


(define (words->numbers words)
  (let helper ((lst (symbols->numbers words)) (accum 0) (tot 0))
    (cond
      ((null? lst)
       (+ tot accum))
      ((= 1 (length lst))
       (let ((no1 (car lst)))
         (if (pow1000? no1)
           (+ tot (* (if (zero? accum) 1 accum) no1))
           (+ tot accum no1))))
      (else
        (let ((no1 (car lst))
              (no2 (cadr lst)))
          (cond
            ((pow1000? no1)
             (helper (cdr lst) 0 (+ tot (* no1 accum))))

            ((and (<= 1 no1 10) (pow1000? no2))
             (helper (cddr lst) 0
                     (+ tot (* (+ accum no1) no2))))

            ((and (<= 1 no1 10) (pow10? no2))
             (helper (cddr lst) (+ accum (* no1 no2)) tot))

            ((= 1 no1)
             (helper (cddr lst) (+ accum no2) tot))

            (else
              (import (chicken format))  ; DELETE ME
              (printf "(helper no1:~a no2:~a lst:~a accum:~a tot:~a~n" no1 no2 lst accum tot)  ; DELETE ME
              (error "Now sure how this could happen"))))))))

)
