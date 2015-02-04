#lang scribble/manual

@(require (for-label typed/racket/base
                     typed/alexis/util/comparator
                     (only-in alexis/util/comparator comparison-predicates-out))
          racket/sandbox
          scribble/eval)

@title{@code{typed/alexis/util}: Filling in the Gaps}

@section{Comparison Predicate Generation}

@defmodule[typed/alexis/util/comparator]

@(define comparison-predicates-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f])
     (make-evaluator 'typed/racket/base)))
@(comparison-predicates-evaluator
  '(require typed/alexis/util/comparator))

See also @racketmodname[alexis/util/comparator] for untyped Racket forms.

@defform[#:literals (:)
         (define-comparison-predicates
           predicate-base-id : predicate-args-type
           comparator-expr maybe-adapter)
         #:grammar
         [(maybe-adapter (code:line)
                         (code:line #:adapter adapter-expr : adapter-args-type))]]{
This provides a convenient macro for generating comparison predicates based on a "comparator"
function. The provided @racket[comparator-expr] must evaluate to a function which takes two values and
produces either @racket[0], @racket[1], or @racket[-1]. These values correspond to both parameters
being equal, the first parameter being greater, or the second parameter being greater, respectively.
The @racket[predicate-args-type] must also be explicitly provided, which describes the types of values
that are permitted to be passed to the predicates.

Using this function, a set of functions is generated using @racket[predicate-base-id] as a base
identifer to determine the names of the resulting definitions. This macro produces six functions,
their names acquired by appending @racket[=?], @racket[>?], @racket[<?], @racket[<>?], @racket[>=?],
and @racket[<=?] to @racket[predicate-base-id].

If @racket[adapter-expr] is provided, then it must evaluate to a function that takes a single
parameter and returns a single value. If specified, values will be threaded through
@racket[adapter-expr] @italic{before} being passed to @racket[comparator-expr]. This allows values to
be mapped to other values before being provided to the comparator for additional processing or
parsing. The @racket[adapter-args-type] expression must specify a type that describes the values that
may be passed to the adapter function.

@(examples
  #:eval comparison-predicates-evaluator
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
    #:adapter (λ (p) (num-str (car p) (cdr p))) : num-str)
  (num-str=? '(123 . "abc") '(123 . "abc"))
  (num-str<? '(200 . "aaa") '(100 . "aaa")))}

The @racketmodname[typed/alexis/util/comparator] module also exports
@racket[comparison-predicates-out] from @racketmodname[alexis/util/comparator], which is compatible
with Typed Racket.
