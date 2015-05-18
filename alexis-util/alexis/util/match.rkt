#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/match)

(provide define/match*)

(define-syntax (define/match* stx)
  (syntax-case stx ()
    [(_ (head arg ... . rest) . body)
     (begin
       (define/with-syntax (arg* ...) (generate-temporaries #'(arg ...)))
       (define rest-arg? (not (null? (syntax-e #'rest))))
       (define/with-syntax rest* (if rest-arg? (generate-temporary #'rest) null))
       (define/with-syntax match*-clause (if rest-arg? #'(arg ... rest) #'(arg ...)))
       (define/with-syntax match*-list (if rest-arg? #'(arg* ... rest*) #'(arg* ...)))
       #'(define (head arg* ... . rest*)
           (match* match*-list
             [match*-clause . body])))]))
