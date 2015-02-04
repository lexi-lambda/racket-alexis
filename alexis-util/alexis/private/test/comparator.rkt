#lang racket/base

(module+ test
  
  (module untyped racket/base
    (require alexis/util/comparator)
    (provide (comparison-predicates-out num-str))
    (struct num-str (num str))
    (define (num-str-compare a b)
      (if (= (num-str-num a) (num-str-num b))
          (let ([a (num-str-str a)]
                [b (num-str-str b)])
            (cond
              [(string>? a b)  1]
              [(string<? a b) -1]
              [else            0]))
          (let ([a (num-str-num a)]
                [b (num-str-num b)])
            (cond
              [(> a b)  1]
              [(< a b) -1]
              [else     0]))))
    (define-comparison-predicates num-str num-str-compare
      #:adapter (λ (p) (num-str (car p) (cdr p)))))
  
  (module typed typed/racket/base
    (require typed/alexis/util/comparator)
    (provide (comparison-predicates-out num-str))
    (struct num-str ([num : Real] [str : String]))
    (: num-str-compare (num-str num-str -> (U -1 0 1)))
    (define (num-str-compare a b)
      (if (= (num-str-num a) (num-str-num b))
          (let ([a (num-str-str a)]
                [b (num-str-str b)])
            (cond
              [(string>? a b)  1]
              [(string<? a b) -1]
              [else            0]))
          (let ([a (num-str-num a)]
                [b (num-str-num b)])
            (cond
              [(> a b)  1]
              [(< a b) -1]
              [else     0]))))
    (define-comparison-predicates
      num-str : (Pairof Real String) num-str-compare
      #:adapter (λ (p) (num-str (car p) (cdr p))) : num-str))
  
  (require rackunit
           (prefix-in u: 'untyped)
           (prefix-in t: 'typed))
  
  (test-case
   "basic comparison generation tests"
   ; =?
   (check-eq? (u:num-str=? '(123 . "abc") '(123 . "abc")) #t)
   (check-eq? (u:num-str=? '(123 . "abc") '(321 . "abc")) #f)
   (check-eq? (u:num-str=? '(123 . "abc") '(123 . "cba")) #f)
   ; >?
   (check-eq? (u:num-str>? '(200 . "aaa") '(100 . "aaa")) #t)
   (check-eq? (u:num-str>? '(200 . "bbb") '(200 . "aaa")) #t)
   (check-eq? (u:num-str>? '(200 . "aaa") '(200 . "aaa")) #f)
   (check-eq? (u:num-str>? '(200 . "aaa") '(200 . "bbb")) #f)
   ; <?
   (check-eq? (u:num-str<? '(100 . "aaa") '(200 . "aaa")) #t)
   (check-eq? (u:num-str<? '(200 . "aaa") '(200 . "bbb")) #t)
   (check-eq? (u:num-str<? '(200 . "aaa") '(200 . "aaa")) #f)
   (check-eq? (u:num-str<? '(200 . "bbb") '(200 . "aaa")) #f))
  
  (test-case
   "untyped/typed comparison integrity tests"
   (define-syntax-rule (check-pred-eq? a? b? v ...)
     (check-eq? (a? v ...) (b? v ...)))
   ; =?
   (check-pred-eq? u:num-str=? t:num-str=? '(123 . "abc") '(123 . "abc"))
   (check-pred-eq? u:num-str=? t:num-str=? '(123 . "abc") '(321 . "abc"))
   (check-pred-eq? u:num-str=? t:num-str=? '(123 . "abc") '(123 . "cba"))
   ; >?
   (check-pred-eq? u:num-str>? t:num-str>? '(200 . "aaa") '(100 . "aaa"))
   (check-pred-eq? u:num-str>? t:num-str>? '(200 . "bbb") '(200 . "aaa"))
   (check-pred-eq? u:num-str>? t:num-str>? '(200 . "aaa") '(200 . "aaa"))
   (check-pred-eq? u:num-str>? t:num-str>? '(200 . "aaa") '(200 . "bbb"))
   ; <?
   (check-pred-eq? u:num-str<? t:num-str<? '(200 . "aaa") '(100 . "aaa"))
   (check-pred-eq? u:num-str<? t:num-str<? '(200 . "aaa") '(200 . "bbb"))
   (check-pred-eq? u:num-str<? t:num-str<? '(200 . "aaa") '(200 . "aaa"))
   (check-pred-eq? u:num-str<? t:num-str<? '(200 . "bbb") '(200 . "aaa")))
  
  )
