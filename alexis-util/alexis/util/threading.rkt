#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide ~> _)

(define-syntax (~> stx)
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
     #:attr insertion-point 0))
  (syntax-parse stx
    [(_ ex:expr) #'ex]
    [(_ ex:expr cl:clause remaining:clause ...)
     (define-values (pre post)
       (split-at (syntax->list #'cl.call)
                 (add1 (attribute cl.insertion-point))))
     (with-syntax ([(pre ...) pre]
                   [(post ...) post])
       #'(~> (pre ... ex post ...) remaining ...))]))
