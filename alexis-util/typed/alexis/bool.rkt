#lang typed/racket/base

(provide true?)

(: true? (Any -> Boolean : #:+ (! #f) #:- #f))
(define (true? x)
  (if x #t #f))
