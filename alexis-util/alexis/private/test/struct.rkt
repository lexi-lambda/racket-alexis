#lang racket/base

(module+ test
  
  (module untyped racket/base
    (require alexis/util/struct)
    (provide (struct+updaters-out point))
    (struct point (x y) #:transparent)
    (define-struct-updaters point))
  
  (require rackunit
           'untyped)
  
  (check-equal? (point-x-set (point 0 0) 5) (point 5 0))
  (check-equal? (point-y-update (point 3 1) add1) (point 3 2))
  
  )
