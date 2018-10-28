; SICP exercise 2.34
;
; Evaluating a polynomial in x at a given value of x can be formulated as an
; accumulation. We evaluate the polynomial
;
; aᵢxⁱ + aᵢ₋₁xⁱ⁻¹ + … + a₁x + a₀
;
; using a well-known algorithm called Horner's rule, which structures the
; computation as
;
; (…(aᵢx + aᵢ₋₁)x + … + a₁)x + a₀
;
; In other words, we start with aᵢ, multiply by x, add aᵢ₋₁, multiply by x, and
; so on, until we reach a₀.
;
; Fill in the following template to produce a procedure that evaluates a
; polynomial using Horner's rule. Assume that the coefficients of the
; polynomial are arranged in a sequence, from a₀ through aᵢ.
;
; (define (horner-eval x coefficient-sequence)
;   (accumulate (lambda (this-coeff higher-terms) <??>)
;               0
;               coefficient-sequence)
;
; For example, to compute 1 + 3x + 5x³ + x⁵ at x = 2 you would evaluate
;
; (horner-eval 2 (list 1 3 0 5 0 1))

#lang racket
(require rackunit rackunit/text-ui)
(require "../list.rkt")

;; a0 + ax1 +a2x^2 + a3x^3 = a0 + (a1 + (a2 + a3x)x)x
;; this method don't use index in coefficient-sequence, it don't need known order number
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;; 1 + 3x + 5x³ + x⁵ at x = 2
;; order number from low to high
;; (horner-eval 2 (list 1 3 0 5 0 1))

(define sicp-2.34-tests
  (test-suite
    "Tests for SICP exercise 2.34"

    (check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)
))

(run-tests sicp-2.34-tests)
