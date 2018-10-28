; SICP exercise 3.61
;
; Let S be a power series (exercise 3.59) whose constant term is 1. Suppose we
; want to find the power series 1/S, that is, the series X such that SX = 1.
; Write S = 1 + Sᵣ where Sᵣ is the part of S after the constant term. Then we
; can solve for X as follows:
;
;          S·X = 1
;   (1 + Sᵣ)·X = 1
;     X + Sᵣ·X = 1
;            X = 1 - Sᵣ·X
;
; In other words, X is the power series whose constant term is 1 and whose
; higher-order terms are given by the genative of Sᵣ times X. Use this idea to
; write a procedure invert-unit-series that computes 1/S for a power series S
; with constant term 1. You will need to use mul-series from exercise 3.60.

#lang racket
(provide (all-defined-out))
(require "../stream/stream.rkt"
         "../stream/op-streams.rkt"
         "./mul-series.rkt")

(define (invert-unit-series series)
  (stream-cons 1 (neg-stream (mul-series (stream-cdr series)
                                         (invert-unit-series series)))))

(module+ main
  (require rackunit rackunit/text-ui)
  (require "../stream/integers.rkt")
  
  (define (integrate-series stream)
    (div-streams stream integers))

  (define cosine-series
    (stream-cons 1 (integrate-series (neg-stream sine-series))))

  (define sine-series
    (stream-cons 0 (integrate-series cosine-series)))
  
  (define sicp-3.61-tests
    (test-suite
     "Tests for SICP exercise 3.61"

     (check-equal? (stream-take (mul-series cosine-series
                                            (invert-unit-series cosine-series))
                                6)
                   '(1 0 0 0 0 0))
     ))

  (run-tests sicp-3.61-tests)
  )


