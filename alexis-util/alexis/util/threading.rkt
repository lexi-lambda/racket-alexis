#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide ~> <> (rename-out [<> ♢]))

(define-syntax (<> stx)
  (raise-syntax-error #f "hole marker not allowed as an expression" stx))

(define-syntax (~> stx)
  (define-syntax-class clause
    #:literals (<>)
    #:attributes (call insertion-point)
    (pattern
     id:id
     #:with call #'(id)
     #:attr insertion-point 0)
    (pattern
     (head:expr pre ... <> post ...)
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
