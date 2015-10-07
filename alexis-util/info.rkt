#lang info

(define collection 'multi)

(define name "alexis-util")
(define version "0.1.0")

(define deps
  '("base"
    "typed-racket-lib"
    "scribble-lib"))
(define build-deps
  '("rackunit-lib"
    "at-exp-lib"
    "racket-doc"
    "typed-racket-doc"
    "sandbox-lib"
    "cover"
    "cover-coveralls"))
