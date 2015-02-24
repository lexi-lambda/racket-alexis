#lang racket/base

(provide loop-forever
         async
         async-loop
         with-semaphore)

(require racket/function)

(define-syntax-rule (loop-forever expr ...)
  (let loop ()
    expr ...
    (loop)))

(define-syntax-rule (async expr ...)
  (thread (thunk expr ...)))

(define-syntax-rule (async-loop expr ...)
  (async (loop-forever expr ...)))

(define-syntax-rule (with-semaphore sem expr ...)
  (call-with-semaphore sem (thunk expr ...)))
