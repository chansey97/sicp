; SICP exercise 1.45
;
; We saw in section 1.3.3 that attempting to compute square roots by naively
; finding a fixed point of x ↦ x/y does not converge, and that this can be
; fixed by average damping. The same method works for finding cube roots as
; fixed points of the average-damped y ↦ x/y². Unfortunatelly, the process does
; not work for fourth roots — a single average damp is not enough to make a
; fixed-point search for y ↦ x/y³ converge. On the other hand, if we average
; damp twice (i.e., use the average damp of the average damp of y ↦ x/y³) the
; fixed-point search does converge. Do some experiments to determine how many
; average damps are required to compute nth roots as a fixed-point search based
; upon repeated average damping of y ↦ x/yⁿ⁻¹. Use this to implement a simple
; procedure for computing nth roots using fixed-point, average-damp, and the
; repeated procedure of exercise 1.43. Assume that any arithmetic operations
; you need are available as primitives.

; After a few experiments, I found out that we need to do log₂n avergage damps
; to approximate the nth root. So, here's the function

#lang racket
(require rackunit rackunit/text-ui)
(require "../../function-composition/repeated.rkt"
         "../fixed-point-of-transform.rkt"
         "../average-damp.rkt")

(define (nth-root n x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (ceiling (/ (log n) (log 2))))
                            1.0))

(define sicp-1.45-tests
  (test-suite
    "Tests for SICP exercise 1.45"

    (check-= (nth-root 2 4) 2 0.00001)
    (check-= (nth-root 3 8) 2 0.00001)
    (check-= (nth-root 4 16) 2 0.00001)
    (check-= (nth-root 5 32) 2 0.00001)
    (check-= (nth-root 8 256) 2 0.00001)
    (check-= (nth-root 9 512) 2 0.00001)
))

(run-tests sicp-1.45-tests)

