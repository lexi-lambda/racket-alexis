#lang racket/base

(require racket/async-channel
         racket/set
         racket/function
         racket/contract
         alexis/util/abbreviations)

(provide
 (contract-out
  [multicast-channel? (any/c . -> . boolean?)]
  [make-multicast-channel (-> multicast-channel?)]
  [make-multicast-receiver (multicast-channel? . -> . async-channel?)]
  [multicast-channel-put (multicast-channel? any/c . -> . void?)]))

; internal representation of a multicast channel
; (entirely opaque)
(struct multicast-channel (internal-channel
                           receiver-set
                           lock))

; contructs a new multicast channel
(define (make-multicast-channel)
  ; initialize the required members
  (define channel (make-async-channel))
  (define receivers (mutable-seteq))
  (define lock (make-semaphore 1))
  
  ; spawn the worker thread
  (define worker-thread
    (async-loop
     (define ac-value (async-channel-get channel))
     (with-semaphore lock
       (set-subtract!
        receivers
        (for/seteq ([receiver-box (in-set receivers)])
          (define receiver (weak-box-value receiver-box))
          (when receiver
            (async-channel-put receiver ac-value))
          ; if the receiver channel has been garbage-collected, mark it for removal
          (if receiver #f receiver-box))))))
  
  ; create the opaque value (weakly, so that the worker thread doesn't retain it)
  (define mc (make-weak-box (multicast-channel channel receivers lock)))
  
  ; create a will executor to manage the worker thread
  (define executor (make-will-executor))
  (will-register executor (weak-box-value mc)
                 (λ (v) (kill-thread worker-thread)))
  
  (weak-box-value mc))

; creates a new multicast receiver channel for a given multicast broadcaster
(define (make-multicast-receiver mc)
  (define ac (make-async-channel))
  
  ; add the listener to the reciever set in another thread so this function can't block
  (async
   (with-semaphore (multicast-channel-lock mc)
     (set-add! (multicast-channel-receiver-set mc) (make-weak-box ac))))
  
  ac)

; broadcasts a value through a multicast channel
(define (multicast-channel-put mc v)
  (async-channel-put (multicast-channel-internal-channel mc) v))
