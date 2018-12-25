#!/usr/bin/csi -s

(import srfi-1
        srfi-13
        srfi-14
        (chicken string)
        (chicken file)
        (chicken io)
        (chicken random)
        (chicken pathname)
        (chicken process-context))


; Parse the command-line arguments into a list of alists

(define (parse-command-line args)
  ; Prepare the command-line arguments by removing punctuation
  (define cs:punct-minus- (char-set-delete char-set:punctuation #\-))

  (let* ((args (map (lambda (s) (string-delete cs:punct-minus- s)) args))
         (directives (state0 args)))
    (import (chicken pretty-print))
    (pretty-print directives)))




; take 5 minutes to ... then take 30 seconds to ...
;
; '(((time 300) (to ...))
;   ((time 30) (to ...)))

(define (state0 args)
  ;; look for a number, skip over words like "then" "take"
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
             (cons `((time ,time-spec) (to ,@action))
                   (state0 (drop args consumed)))))

          (else
            (state0 (cdr args)))))))

(define (state1 args)
  (let loop ((args args) (action '()) (consumed 0))
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
              (loop (cdr args) (cons this-arg action) (add1 consumed))))))))




(parse-command-line (command-line-arguments))
