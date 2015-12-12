#lang info

(define collection 'multi)

(define name "alexis-util")
(define version "0.1.0")

(define deps
  '("base"
    "match-plus"
    "scribble-lib"
    "static-rename"
    "threading"
    "typed-racket-lib"))
(define build-deps
  '("rackunit-lib"
    "at-exp-lib"
    "racket-doc"
    "typed-racket-doc"
    "sandbox-lib"
    "cover"
    "cover-coveralls"))
