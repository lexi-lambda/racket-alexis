#lang racket/base

(provide true?)

(define (true? x)
  (if x #t #f))
