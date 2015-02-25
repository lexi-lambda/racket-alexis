#lang racket/base

(module+ test
  (require rackunit
           racket/function
           racket/async-channel
           alexis/multicast)
  
  (test-case
   "Basic operations"
   (define mc (make-multicast-channel))
   
   (define a (make-multicast-receiver mc))
   (define b (make-multicast-receiver mc))
   
   (multicast-channel-put mc 'a)
   
   (check-equal? (async-channel-get a) 'a)
   
   (define c (make-multicast-receiver mc))
   
   (multicast-channel-put mc 'b)
   (multicast-channel-put mc 'c)
   (multicast-channel-put mc 'd)
   
   (check-equal? (async-channel-get a) 'b)
   (check-equal? (async-channel-get a) 'c)
   (check-equal? (async-channel-get a) 'd)
   
   (check-equal? (async-channel-get b) 'a)
   (check-equal? (async-channel-get b) 'b)
   (check-equal? (async-channel-get b) 'c)
   (check-equal? (async-channel-get b) 'd)
   
   (check-equal? (async-channel-get c) 'b)
   (check-equal? (async-channel-get c) 'c)
   (check-equal? (async-channel-get c) 'd))
  
  (test-case
   "Read-only receivers"
   (define mc (make-multicast-channel))
   (define ac (make-multicast-receiver mc))
   
   (check-exn #rx"read-only"
              (thunk (async-channel-put ac 'foo))))
  
  )
