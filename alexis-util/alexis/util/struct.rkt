#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info
                     racket/provide-transform
                     racket/syntax
                     syntax/parse)
         racket/contract/base
         racket/contract/region)

(provide define-struct-updaters
         struct-updaters-out
         struct+updaters-out)

; Extracts the fields belonging to a struct, not including its supertypes. The first value returned
; includes all accessors, the second value is just the struct's fields.
; struct-info? syntax? -> (listof identifier?) (listof identifier?)
(define-for-syntax (get-struct-accessors struct-info failure-context)
  (define accessors (reverse (fourth struct-info)))
  ; if not all the accessors are available, fail
  (unless (first accessors)
    (wrong-syntax failure-context "not all structure fields are available from the structure type"))
  (define accessor-count (length accessors))
  (define super-type (sixth struct-info))
  ; calculate how many fields actually belong to the given type
  (define field-count
    (cond [(identifier? super-type)
           (define super-info (extract-struct-info (syntax-local-value super-type)))
           (define super-accessors (fourth super-info))
           (- accessor-count (length (filter identifier? super-accessors)))]
          [else accessor-count]))
  ; return the results
  (values accessors (drop accessors (- accessor-count field-count))))

; Gets the derived setter & updater names from a list of accessors. The ‘context’ argument is used
; for the new identifiers' lexical scope. Additionally, returns the field accessors that occur before
; and after each accessor in the field list.
; (listof identifier?) (listof identifier?) syntax? -> (listof (list/c syntax? syntax? syntax? syntax?))
(define-for-syntax (get-setters+updaters all-accessors accessors context)
  (define super-field-count (- (length all-accessors) (length accessors)))
  (for/list ([accessor (in-list accessors)]
             [index (in-naturals)])
    (define setter (format-id context "~a-set" accessor #:source context))
    (define updater (format-id context "~a-update" accessor #:source context))
    (define-values (pre current+post) (split-at all-accessors (+ super-field-count index)))
    (list setter updater pre (rest current+post))))

; makes functional setters and updaters given a structure type
(define-syntax (define-struct-updaters stx)
  (syntax-parse stx
    [(_ name:id)
     ; this gets compile-time information about the struct
     (define struct-info (extract-struct-info (syntax-local-value #'name)))
     ; we can use it to get the constructor, predicate, and accessor functions
     (define/with-syntax make-name (second struct-info))
     (define/with-syntax name? (third struct-info))
     (define-values (all-accessors accessors) (get-struct-accessors struct-info #'name))
     (define/with-syntax (name-field ...) accessors)
     ; get the required identifiers and information to generate setter/updater functions
     (define/with-syntax ([name-field-set name-field-update
                           (name-field-pre ...) (name-field-post ...)]
                          ...)
       (get-setters+updaters all-accessors accessors stx))
     ; now we just need to generate the actual function code
     #'(begin
         (define/contract (name-field-set instance value)
           (-> name? any/c name?)
           (make-name (name-field-pre instance) ...
                      value
                      (name-field-post instance) ...))
         ...
         (define/contract (name-field-update instance updater)
           (-> name? (-> any/c any/c) name?)
           (make-name (name-field-pre instance) ...
                      (updater (name-field instance))
                      (name-field-post instance) ...))
         ...)]))

; provide transformer for easily exporting updaters
(define-syntax struct-updaters-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ struct-type:id)
        (define struct-info (extract-struct-info (syntax-local-value #'struct-type)))
        (define-values (all-accessors accessors) (get-struct-accessors struct-info #'struct-type))
        (define/with-syntax ([setter updater _ _] ...)
          (get-setters+updaters all-accessors accessors stx))
        (expand-export #'(combine-out setter ... updater ...) modes)]))))

; utility for providing both struct-out and struct-updaters-out
(define-syntax struct+updaters-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ struct-type:id)
        (expand-export #'(combine-out (struct-out struct-type)
                                      (struct-updaters-out struct-type))
                       modes)]))))
