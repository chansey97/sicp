#lang racket
(provide (all-defined-out))
(require "../stream/stream.rkt"
         "../stream/op-streams.rkt")

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (integral-delayed delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt) int))))
  int)



(module+ main
  (require rackunit rackunit/text-ui)
  (require "../stream/stream.rkt")
  (require "../stream/integers.rkt")
  
  (stream-take (integers-starting-from 1) 10)

  (stream-take (integral (integers-starting-from 1) 0 1)
               10)

  
  )
