#lang racket
(provide (all-defined-out))
(require "./sum.rkt")

;; Once we have sum, we can use it as a building block in formulating further concepts.
;; For instance, the definite integral of a function f between the limits a and b
;; can be approximated numerically using the formula

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
