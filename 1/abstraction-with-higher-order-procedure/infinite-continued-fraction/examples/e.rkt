; SICP exercise 1.38
;
; In 1737, the Swiss mathematician Leonhard Euler published a memoir De
; Fractionibus Continuis, which included a continued fraction expansion of e-2,
; where e is the base of the natural logarithms. In this fraction, Nᵢ are all
; 1, and the Dᵢ are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8,…. Write a
; program that uses your cont-frac procedure from exercise 1.37 to approximate
; e, based on Euler's expansion.

#lang racket
(require rackunit rackunit/text-ui)
(require "../cont-frac.rkt")

(define (approximate-e)
  (define (term n)
    (if (= (remainder (+ n 1) 3) 0)
        (* 2 (/ (+ n 1) 3))
        1.0))

  (+ (cont-frac (lambda (x) 1.0) term 20)
     2))

(define sicp-1.38-tests
  (test-suite
    "Tests for SICP exercise 1.38"

    (check-= (approximate-e) 2.71828183 0.00000001)
))

(run-tests sicp-1.38-tests)
