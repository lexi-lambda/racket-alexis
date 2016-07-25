#lang racket/base

(require struct-updaters)

(provide define-struct-updaters
         struct-updaters-out
         struct+updaters-out)

(module+ get-struct-accessors
  (require (submod struct-updaters get-struct-accessors))
  (provide (for-syntax get-struct-accessors)))
