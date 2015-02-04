#lang typed/racket/base

(require racket/function
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         (only-in alexis/util/comparator
                  comparison-predicates-out))

(provide define-comparison-predicates
         comparison-predicates-out)

; utility for formating identifiers
(begin-for-syntax
  (define-syntax-rule (format-identifier format id)
    (format-id id #:source id format id)))

(define-syntax (define-comparison-predicates stx)
  (syntax-parse stx #:literals (:)
    [(_ out-name:id : type:expr comparison-fn:expr
        (~optional (~seq #:adapter adapter-fn:expr : adapted-type:expr)
                   #:defaults ([adapter-fn #'identity] [adapted-type #'type])))
     (define/with-syntax out-name=? (format-identifier "~a=?" #'out-name))
     (define/with-syntax out-name>? (format-identifier "~a>?" #'out-name))
     (define/with-syntax out-name<? (format-identifier "~a<?" #'out-name))
     (define/with-syntax out-name<>? (format-identifier "~a<>?" #'out-name))
     (define/with-syntax out-name>=? (format-identifier "~a>=?" #'out-name))
     (define/with-syntax out-name<=? (format-identifier "~a<=?" #'out-name))
     #'(begin
         (define compare : (adapted-type adapted-type -> (U -1 0 1)) comparison-fn)
         (define adapt : (type -> adapted-type) adapter-fn)
         (define (out-name=? [a : type] [b : type])
           (= (compare (adapt a) (adapt b)) 0))
         (define (out-name>? [a : type] [b : type])
           (= (compare (adapt a) (adapt b)) 1))
         (define (out-name<? [a : type] [b : type])
           (= (compare (adapt a) (adapt b)) -1))
         (define (out-name<>? [a : type] [b : type])
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v -1) (= v 1))))
         (define (out-name>=? [a : type] [b : type])
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v 0) (= v 1))))
         (define (out-name<=? [a : type] [b : type])
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v 0) (= v -1))))
         )]))
