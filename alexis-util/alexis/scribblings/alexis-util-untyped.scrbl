#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     alexis/util/comparator
                     alexis/util/struct
                     (submod alexis/util/struct get-struct-accessors)
                     racket/struct-info)
          scribble/eval
          scribble/extract)

@title{Untyped Utilities}

@section{Comparison Predicate Generation}

@defmodule[alexis/util/comparator]

@(define comparison-predicates-evaluator
   ((make-eval-factory #:lang 'racket/base
                       '(alexis/util/comparator))))

See also @racketmodname[typed/alexis/util/comparator] for Typed Racket-compatible forms.

@defform[(define-comparison-predicates predicate-base-id comparator-expr
           maybe-adapter)
         #:grammar
         [(maybe-adapter (code:line)
                         (code:line #:adapter adapter-expr))]]{
This provides a convenient macro for generating comparison predicates based on a "comparator"
function. The provided @racket[comparator-expr] must evaluate to a function which takes two values and
produces either @racket[0], @racket[1], or @racket[-1]. These values correspond to both parameters
being equal, the first parameter being greater, or the second parameter being greater, respectively.

Using this function, a set of functions is generated using @racket[predicate-base-id] as a base
identifer to determine the names of the resulting definitions. This macro produces six functions,
their names acquired by appending @racket[=?], @racket[>?], @racket[<?], @racket[<>?], @racket[>=?],
and @racket[<=?] to @racket[predicate-base-id].

If @racket[adapter-expr] is provided, then it must evaluate to a function that takes a single
parameter and returns a single value. If specified, values will be threaded through
@racket[adapter-expr] @italic{before} being passed to @racket[comparator-expr]. This allows values to
be mapped to other values before being provided to the comparator for additional processing or
parsing.

@(examples
  #:eval comparison-predicates-evaluator
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
    #:adapter (λ (p) (num-str (car p) (cdr p))))
  (num-str=? '(123 . "abc") '(123 . "abc"))
  (num-str<? '(200 . "aaa") '(100 . "aaa")))}

@defform[(comparison-predicates-out maybe-renamed)
         #:grammar
         [(maybe-renamed predicate-base-id
                         [original-base-id renamed-base-id])]]{
This is a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{provide transformer} that
provides a convenient shorthand for providing predicates generated via
@racket[define-comparison-predicates]. Predicate suffixes are prepended the base identifiers in the
same manner as in @racket[define-comparison-predicates]. If @racket[renamed-base-id] is provided, the
generated provides use the renamed base identifier.}

@section{Struct Utilities}

@defmodule[alexis/util/struct]

@deprecated[@racketmodname[struct-updaters]]{
 This module re-exports @racket[define-struct-updaters], @racket[struct-updaters-out],
 and @racket[struct+updaters-out] from @racketmodname[struct-updaters] for
 backwards-compatibility.}

@section{Wrapping keyword procedures}

@defmodule[alexis/util/wrap]

It's easy to "wrap" existing procedures in order to extend them with additional functionality. For
example, the following wraps @racket[+] to additionally call @racket[add1] on the result:

@(racketblock
  (define (+/add1 . args)
    (add1 (apply + args))))

This is much harder to do, however, if the procedure being wrapped can accept keyword arguments, since
they are not accepted in the "rest argument" syntax. Instead, @racket[make-keyword-procedure] and
@racket[keyword-apply] must be used. This module provides some macros to make wrapping these
procedures easier.

@include-extracted[alexis/util/wrap]
