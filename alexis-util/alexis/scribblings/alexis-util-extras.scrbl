#lang scribble/manual

@(require (for-label racket/base
                     racket/list
                     racket/function
                     racket/contract
                     racket/match
                     racket/math
                     (only-in typed/racket/base
                              Any Boolean)
                     alexis/bool
                     alexis/util/abbreviations
                     alexis/util/match
                     threading)
          scribble/eval)

@title{Extra Utilities}

@section{The @code{true?} Predicate}

@defmodule*[(alexis/bool
             typed/alexis/bool)]

@defproc*[([(true? [v any/c]) boolean?]
           [(true? [v Any]) Boolean])]{
Equivalent to @racket[(if v #t #f)]. Useful for casting values to booleans.}

@section{Pattern Matching in Simple Functions}

@defmodule[alexis/util/match]

@defform[(define/match* (head-id args) body)
         #:grammar
         ([args (code:line match-expr ...)
                (code:line match-expr ... @#,racketparenfont{.} rest-expr)])]{
Equivalent to:
@(racketblock
  (define (head-id args*)
    (match* (args*)
      [(args) body])))
where @racket[args*] is a list of unique identifiers generated corresponding to each @racket[_arg].}

@section{Abbreviation Macros}

@defmodule[alexis/util/abbreviations]

This module provides various simple abbreviations to make writing Racket code clearer and easier.
These forms should work in both Racket and Typed Racket.

@defform[(loop-forever body)]{
Equivalent to:
@(racketblock
  (let loop ()
    body
    (loop)))}

@defform[(async body)]{
Equivalent to:
@(racketblock
  (thread (thunk body)))}

@defform[(async-loop body)]{
Equivalent to:
@(racketblock
  (thread (thunk (loop-forever body))))}

@defform[(with-semaphore semaphore-expr body)]{
Equivalent to:
@(racketblock
  (call-with-semaphore semaphore-expr
    (thunk body)))}

@section{Threading Macros}

@defmodule[alexis/util/threading]

@deprecated[@racketmodname[threading]]{
 This module re-exports @racket[~>], @racket[~>>], @racket[and~>], @racket[and~>>], @racket[lambda~>],
 @racket[λ~>], @racket[lambda~>*], @racket[λ~>*], @racket[lambda~>>], @racket[λ~>>],
 @racket[lambda~>>*], @racket[λ~>>*], @racket[lambda-and~>], @racket[λ-and~>], @racket[lambda-and~>*],
 @racket[λ-and~>*], @racket[lambda-and~>>], @racket[λ-and~>>], @racket[lambda-and~>>*], and
 @racket[λ-and~>>*] from @racketmodname[threading] for backwards-compatibility.}
