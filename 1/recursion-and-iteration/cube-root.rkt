; SICP exercise 1.08
;
; Newton's method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given by
; the value:
;
; x/y² + 2y
; ─────────
;     3
;
; Use this formula to implement a cube-root procedure analogous to the
; square-root procedure. (In section 1.3.4 we will see how to implement
; Newton's method in general as an abstraction of these square-root and
; cube-root procedures).

#lang racket
(require rackunit rackunit/text-ui)

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x)
               x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(define sicp-1.08-tests
  (test-suite
    "Tests for SICP exercise 1.08"

    (check-= (cube-root 8) 2 0.00001)
    (check-= (cube-root 27) 3 0.00001)
    (check-= (cube-root 1000) 10 0.00001)
))

(run-tests sicp-1.08-tests)
