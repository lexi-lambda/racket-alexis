#lang scribble/manual

@title{@code{alexis/util}: Filling in the Gaps}

This provides various utilities that are not provided in the main Racket distribution, but may be
helpful. All utilities in this library are designed to remain as "Racket-y" as possible to fit in with
the core libraries.

This collection provides both untyped and typed variants of its utilities. Typed versions are prefixed
with the usual @racket[typed/] collection.

@include-section["alexis-util-untyped.scrbl"]
@include-section["alexis-util-typed.scrbl"]
