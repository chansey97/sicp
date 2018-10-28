#lang racket
(require rackunit rackunit/text-ui)
(require "./symbolic-differentiation.rkt")

(define sicp-2.57-tests
  (test-suite
    "Tests for SICP exercise 2.57"

    (check-equal? (multiplier '(* x y z)) 'x)
    (check-equal? (multiplicand '(* x y z)) '(* y z))

    (check-equal? (addend '(+ x y z)) 'x)
    (check-equal? (augend '(+ x y z)) '(+ y z))

    (check-equal? (deriv '(* x y (+ x 3)) 'x)
                  '(+ (* x y) (* y (+ x 3))))

    (check-equal? (deriv '(+ 0 y x) 'x)
                  1)
))

(run-tests sicp-2.57-tests)

;; (deriv '(* x y (+ x 3)) 'x)
