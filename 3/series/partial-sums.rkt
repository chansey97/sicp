#lang racket
(provide (all-defined-out))
(require "../stream/stream.rkt")
(require "../stream/op-streams.rkt")

(define (partial-sums stream)
  (define result
    (stream-cons (stream-car stream)
                 (add-streams (stream-cdr stream) result)))
  result)
