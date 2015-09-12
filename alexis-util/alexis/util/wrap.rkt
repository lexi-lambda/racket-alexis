#lang at-exp racket/base

(require scribble/srcdoc
         syntax/parse/define
         (for-doc racket/base
                  racket/format
                  scribble/eval
                  scribble/manual)
         (for-label racket/list))

(provide @form-doc[(wrap-keyword-procedure [result-id original-proc-expr]
                     body-expr ...+)
                   #:contracts ([original-proc-expr procedure?])
                   @{Creates a new procedure that wraps @racket[original-proc-expr]. Each application
                     of the resulting procedure calls @racket[original-proc-expr] with the provided
                     arguments and binds @racket[result-id] to the result, which is available to the
                     @racket[body-expr]s. The return value of the resulting procedure is the result of
                     the final @racket[body-expr].
                     
                     @(examples
                       #:eval ((make-eval-factory '(racket/list alexis/util/wrap)))
                       (define check-duplicates+add1
                         (wrap-keyword-procedure [v check-duplicates]
                           (add1 v)))
                       (check-duplicates+add1 #:key positive? '(1 1)))}]
         
         @form-doc[(define/wrapped name-id [result-id original-proc-expr]
                     body-expr ...+)
                   #:contracts ([original-proc-expr procedure?])
                   @{An abbreviation for defining functions using @racket[wrap-keyword-procedure].
                     Equivalent to:
                     
                     @(racketblock
                       (define name-id
                         (wrap-keyword-procedure [result-id original-proc-expr]
                           body ...)))}])

(define-simple-macro (wrap-keyword-procedure [result:id proc:expr] body ...+)
  (make-keyword-procedure
   (λ (kws kw-vals . rest)
     (let ([result (keyword-apply proc kws kw-vals rest)])
       body ...))))

(define-simple-macro (define/wrapped new-id [result:id original-proc:expr] body ...+)
  (define new-id (wrap-keyword-procedure [result original-proc] body ...)))

(module+ test
  (require racket/list
           rackunit)

  (define check-duplicates+add1
    (wrap-keyword-procedure
     [v check-duplicates]
     (add1 v)))

  (define/wrapped check-duplicates+add1* [v check-duplicates]
    (add1 v))

  (check-equal? (check-duplicates+add1 #:key positive? '(1 1)) 2)
  (check-equal? (check-duplicates+add1* #:key positive? '(1 1)) 2))
