#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide ~> ~>> _)

(begin-for-syntax
  (define-syntax-class clause
    #:literals (_)
    #:attributes (call insertion-point)
    (pattern
     id:id
     #:with call #'(id)
     #:attr insertion-point 0)
    (pattern
     (head:expr pre ... _ post ...)
     #:with call #'(head pre ... post ...)
     #:attr insertion-point (length (syntax->list #'(pre ...))))
    (pattern
     (head:expr arg ...)
     #:with call #'(head arg ...)
     #:attr insertion-point #f)))

(define-syntaxes (~> ~>>)
  (values
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      (define call (syntax->list #'cl.call))
      (define-values (pre post)
        (split-at call (add1 (or (attribute cl.insertion-point) 0))))
      (with-syntax ([(pre ...) pre]
                    [(post ...) post])
        #'(~> (pre ... ex post ...) remaining ...))])
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      (define call (syntax->list #'cl.call))
      (define-values (pre post)
        (split-at call (add1 (or (attribute cl.insertion-point)
                                 (sub1 (length call))))))
      (with-syntax ([(pre ...) pre]
                    [(post ...) post])
        #'(~>> (pre ... ex post ...) remaining ...))])))
