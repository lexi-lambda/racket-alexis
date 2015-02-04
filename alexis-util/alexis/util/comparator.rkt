#lang racket/base

(require racket/function
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/provide-transform))

(provide define-comparison-predicates
         comparison-predicates-out)

; utility for formating identifiers
(begin-for-syntax
  (define-syntax-rule (format-identifier format id)
    (format-id id #:source id format id)))

; generates predicates from comparator functions
(define-syntax (define-comparison-predicates stx)
  (syntax-parse stx
    [(_ out-name:id comparison-fn:expr
        (~optional (~seq #:adapter adapter-fn:expr) #:defaults ([adapter-fn #'identity])))
     (define/with-syntax out-name=? (format-identifier "~a=?" #'out-name))
     (define/with-syntax out-name>? (format-identifier "~a>?" #'out-name))
     (define/with-syntax out-name<? (format-identifier "~a<?" #'out-name))
     (define/with-syntax out-name<>? (format-identifier "~a<>?" #'out-name))
     (define/with-syntax out-name>=? (format-identifier "~a>=?" #'out-name))
     (define/with-syntax out-name<=? (format-identifier "~a<=?" #'out-name))
     #'(begin
         (define compare comparison-fn)
         (define adapt adapter-fn)
         (define (out-name=? a b)
           (= (compare (adapt a) (adapt b)) 0))
         (define (out-name>? a b)
           (= (compare (adapt a) (adapt b)) 1))
         (define (out-name<? a b)
           (= (compare (adapt a) (adapt b)) -1))
         (define (out-name<>? a b)
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v -1) (= v 1))))
         (define (out-name>=? a b)
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v 0) (= v 1))))
         (define (out-name<=? a b)
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v 0) (= v -1))))
         )]))

; adds provide transformer for easily exporting generated predicates
(define-syntax comparison-predicates-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ (~and (~or out-name:id
                      [original-name:id renamed-out:id])
                 out-spec) ...)
        ; normalize provide specs
        (define/with-syntax ([original rename] ...)
          (for/list ([spec (syntax->list #'(out-spec ...))])
            (syntax-case spec ()
              [[original rename] #'[original rename]]
              [out               #'[out out]])))
        ; produce all predicate names
        (define/with-syntax (([original? rename?] ...) ...)
          (for/list ([spec (syntax->list #'([original rename] ...))])
            (syntax-case spec ()
              [[original rename]
               (with-syntax ([original=? (format-identifier "~a=?" #'original)]
                             [original>? (format-identifier "~a>?" #'original)]
                             [original<? (format-identifier "~a<?" #'original)]
                             [original<>? (format-identifier "~a<>?" #'original)]
                             [original>=? (format-identifier "~a>=?" #'original)]
                             [original<=? (format-identifier "~a<=?" #'original)]
                             [rename=? (format-identifier "~a=?" #'rename)]
                             [rename>? (format-identifier "~a>?" #'rename)]
                             [rename<? (format-identifier "~a<?" #'rename)]
                             [rename<>? (format-identifier "~a<>?" #'rename)]
                             [rename>=? (format-identifier "~a>=?" #'rename)]
                             [rename<=? (format-identifier "~a<=?" #'rename)])
                 #'([original=? rename=?]
                    [original>? rename>?]
                    [original<? rename<?]
                    [original<>? rename<>?]
                    [original>=? rename>=?]
                    [original<=? rename<=?]))])))
        ; output the rename-out clause
        (expand-export #'(rename-out [original? rename?] ... ...) modes)]))))
