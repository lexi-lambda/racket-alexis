#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide define/renamed)

(define-syntax (define/renamed stx)
  (define-syntax-class function-header
    #:attributes (name)
    (pattern (name:id . args))
    (pattern (header:function-header . args)
             #:attr name #'header.name))
  (define (replace-function-name name header)
    (syntax-parse header
      [(_:id . args) #`(#,name . args)]
      [(header . args) #`(#,(replace-function-name name #'header) . args)]))
  (syntax-parse stx
    [(_ name:id header:function-header . rest)
     #`(define header.name
         (let ()
           (define #,(replace-function-name #'name #'header) . rest)
           name))]))
