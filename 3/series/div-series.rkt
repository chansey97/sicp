#lang racket
(provide (all-defined-out))
(require "../stream/stream.rkt"
         "./mul-series.rkt"
         "./invert-unit-series.rkt")

(define (div-series a b)
  (if (= (stream-car b) 0)
      (error "Cannot divide by a power series with constant term = 0")
      (mul-series a (invert-unit-series b))))
