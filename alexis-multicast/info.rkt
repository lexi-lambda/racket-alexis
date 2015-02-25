#lang info

(define collection 'multi)

(define name "alexis-multicast")
(define version "0.1.0")

(define deps
  '("base"
    "alexis-util"))
(define build-deps
  '("rackunit-lib"
    "cover"
    "racket-doc"
    "scribble-lib"))
