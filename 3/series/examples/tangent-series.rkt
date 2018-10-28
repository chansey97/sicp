; SICP exercise 3.62
;
; Use the result of exercise 3.60 and exercise 3.61 to define a procedure
; div-series that divides two power series. div-series should work for any two
; series, provided that the denominator series begins with a nonzero constant
; term. (If the denominator has a zero constant term, the div-series should
; signal an error.) Show how to use div-series together with the result of
; exercise 3.59 to generate the power series for tangent.

#lang racket
(require rackunit rackunit/text-ui)
(require "../../stream/stream.rkt"
         "../../stream/op-streams.rkt"
         "../../stream/integers.rkt"
         "../div-series.rkt")

(define (integrate-series stream)
  (div-streams stream integers))

(define cosine-series
  (stream-cons 1 (integrate-series (neg-stream sine-series))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define tangent-series (div-series sine-series cosine-series))

(define sicp-3.62-tests
  (test-suite
   "Tests for SICP exercise 3.62"

   (check-exn exn? (lambda () (div-series ones (stream-cons 0 ones))))

   (check-equal? (stream-take tangent-series 6)
                 (list 0 1 0 (/ 1 3) 0 (/ 2 15)))
   ))

(run-tests sicp-3.62-tests)
