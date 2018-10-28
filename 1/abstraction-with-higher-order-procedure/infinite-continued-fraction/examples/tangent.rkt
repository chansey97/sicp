; SICP exercise 1.39
;
; A continued fraction representation of tangent function was published in
; 1770 by the German mathematician J. H. Lambert:
;
;                x
; tan(x) = ─────────────
;                  x²
;          1 - ─────────
;                    x²
;              3 - ─────
;                  5 - ⋱
;
; where x is in radians. Define a procedure (tan-cf x k) that computes an
; approximation to the tangent function based on Lambert's formula. x specifies
; the number of terms to compute, as in exercise 1.37.

#lang racket
(require rackunit rackunit/text-ui)
(require "../cont-frac.rkt")

(define (tan-cf x k)
  (let ((neg-x-squared (- (* x x))))
    (cont-frac (lambda (n) (if (= n 1) x neg-x-squared))
               (lambda (n) (- (* n 2) 1))
               k)))

(define sicp-1.39-tests
  (test-suite
    "Tests for SICP exercise 1.39"

    (check-= (tan-cf 0 100) 0 0.00001)
    (check-= (tan-cf 1.0 100) 1.55740 0.00001)
    (check-= (tan-cf 2.0 100) -2.18503 0.00001)
    (check-= (tan-cf 3.141592 100) 0.0 0.00001)
))

(run-tests sicp-1.39-tests)
