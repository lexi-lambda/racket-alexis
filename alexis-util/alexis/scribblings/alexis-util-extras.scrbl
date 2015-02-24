#lang scribble/manual

@(require (for-label racket/base
                     racket/function
                     alexis/util/abbreviations))

@title{Extra Utilities}

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
