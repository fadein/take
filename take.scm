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
                     (string-ci= this-arg "then")
                     (string-ci= this-arg "to"))
                 (loop (cdr args) action (add1 consumed)))

                ;; else, accumulate into a list
                (else
                  (loop (cdr args) (cons this-arg action) (add1 consumed))))))))




(parse-command-line (command-line-arguments))


(define (parse-command-line+ args)
  ;;; strip punctuation from words, except for '-'
  (define cs:punct-minus- (char-set-delete char-set:punctuation #\-))
  (let loop ((args (map (lambda (s) (string-delete cs:punct-minus- s)) args))
               (quant 1) (item #f))
    (if (null? args)
        '()

        (let* ((this-arg (car args))
               (this-num (string->number this-arg)))
          (cond

            ;; numeric quantities
            ((and (number? this-num) (> this-num 0))
             (loop (cdr args) this-num item))

            (else
              (case (string->symbol this-arg)

                ;; types of items this program can return
                ((number numbers)
                 (cons (list 'number quant
                             (if (and
                                   (>= (length args) 5)
                                   (or
                                     (string= "from" (cadr args))
                                     (string= "between" (cadr args)))
                                   (or
                                     (string= "to" (cadddr args))
                                     (string= "and" (cadddr args))))
                                 (list (caddr args) (car (cddddr args)))
                                 '()))
                       (loop (cdr args) 1 #f)))

                ((noun nouns)
                 (cons (cons 'noun quant) (loop (cdr args) 1 #f)))

                ((word words)
                 (cons (cons 'word quant) (loop (cdr args) 1 #f)))

                ((verb verbs)
                 (cons (cons 'verb quant) (loop (cdr args) 1 #f)))

                ((name names)
                 (cons (cons 'name quant) (loop (cdr args) 1 #f)))

                ((color colors)
                 (cons (cons 'color quant) (loop (cdr args) 1 #f)))

                ((fancycolor fancycolors)
                 (cons (cons 'fancycolor quant) (loop (cdr args) 1 #f)))

                ((csscolor csscolors)
                 (cons (cons 'csscolor quant) (loop (cdr args) 1 #f)))

                ;; quantities
                ((a an one)
                 (loop (cdr args) 1 item))

                ((brace pair couple two)
                 (loop (cdr args) 2 item))

                ((three)
                 (loop (cdr args) 3 item))

                ((few)
                 (loop (cdr args) (a-number-between 3 5) item))

                ((four)
                 (loop (cdr args) 4 item))

                ((handful five)
                 (loop (cdr args) 5 item))

                ((six)
                 (loop (cdr args) 6 item))

                ((seven)
                 (loop (cdr args) 7 item))

                ((some several)
                 (loop (cdr args) (a-number-between 4 7) item))

                ((eight)
                 (loop (cdr args) 8 item))

                ((nine)
                 (loop (cdr args) 9 item))

                ((ten)
                 (loop (cdr args) 10 item))

                ((bunch many lots)
                 (loop (cdr args) (a-number-between 8 15) item))

                ((dozen)
                 (loop (cdr args) 12 item))

                ((mess oodles multitude ton crapton grundle swarm crowd flock)
                 (loop (cdr args) (a-number-between 20 55) item))

                ((hundred)
                 (loop (cdr args) 100 item))

                ((gross)
                 (loop (cdr args) 144 item))

                ((bajillion gazillion)
                 (loop (cdr args) (a-number-between 100 10000) item))

                ((myriad)
                 (loop (cdr args) 10000 item))

                (else
                  (loop (cdr args) quant item)))))))))
