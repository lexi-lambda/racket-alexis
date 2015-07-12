#lang scribble/manual

@(require (for-label racket/base
                     racket/list
                     racket/function
                     racket/contract
                     racket/match
                     (only-in typed/racket/base
                              Any Boolean)
                     alexis/bool
                     alexis/util/abbreviations
                     alexis/util/match
                     alexis/util/threading)
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

This provides a Clojure-inspired threading macro, but it allows the insertion point to be explicitly
specified in the case that the first argument is not the proper threading point.

@defform[#:literals (_)
         (~> expr clause ...)
         #:grammar
         ([clause bare-id
                  (fn-expr arg-expr ...)
                  (fn-expr pre-expr ... hole-marker post-expr ...)]
          [hole-marker _])]{
"Threads" the @racket[expr] through the @racket[clause] expressions, from top to bottom. If a
@racket[clause] is a @racket[bare-id], then the clause is transformed into the form
@racket[(bare-id)] before threading. If the clause is a function application without a
@racket[hole-marker], it is transformed into a function application with the @racket[hole-marker]
placed immediately after the @racket[fn-expr].

Once the initial transformation has been completed, the @racket[expr] is threaded through the clauses
by nesting it within each clause, replacing the hole marker.

@(examples
  #:eval ((make-eval-factory '(racket/list
                               racket/function
                               alexis/util/threading)))
  (~> '(1 2 3)
      (map add1 _)
      second
      (* 2))
  (~> "foo"
      string->bytes/utf-8
      bytes->list
      (map (curry * 2) _)
      list->bytes))}

@defform[#:literals (_)
         (~>> expr clause ...)
         #:grammar
         ([clause bare-id
                  (fn-expr arg-expr ...)
                  (fn-expr pre-expr ... hole-marker post-expr ...)]
          [hole-marker _])]{
Works equivalently to @racket[~>] except that when no @racket[hole-marker] is provided, the insertion
point is at the @emph{end}, just after the final @racket[arg-expr].

@(examples
  #:eval ((make-eval-factory '(racket/list
                               racket/function
                               alexis/util/threading)))
  (~>> '(1 2 3)
       (map add1)
       second
       (* 2))
  (~>> "foo"
       string->bytes/utf-8
       bytes->list
       (map (curry * 2))
       list->bytes))}
