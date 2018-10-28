#lang racket
(require rackunit rackunit/text-ui)
(require "./generic-operations.rkt")

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-equ?-package)
(install-=zero?-package)

(define sicp-2.79-tests
  (test-suite
    "Tests for SICP exercise 2.79"

    (check-true (equ? (make-scheme-number 1) (make-scheme-number 1)))
    (check-false (equ? (make-scheme-number 1) (make-scheme-number 2)))

    (check-true (equ? (make-rational 1 2) (make-rational 1 2)))
    (check-true (equ? (make-rational 1 2) (make-rational 2 4)))
    (check-false (equ? (make-rational 1 2) (make-rational 2 2)))
    (check-false (equ? (make-rational 1 2) (make-rational 1 1)))

    (check-true (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2)))
    (check-true (equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 2)))
    (check-false (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 1)))
    (check-false (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 2 2)))
    (check-false (equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 1)))
    (check-false (equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 2 2)))
))

(define sicp-2.80-tests
  (test-suite
    "Tests for SICP exercise 2.80"

    (check-true (=zero? (make-scheme-number 0)))
    (check-false (=zero? (make-scheme-number 1)))

    (check-true (=zero? (make-rational 0 1)))
    (check-true (=zero? (make-rational 0 2)))
    (check-false (=zero? (make-rational 1 2)))

    (check-true (=zero? (make-complex-from-real-imag 0 0)))
    (check-false (=zero? (make-complex-from-real-imag 0 1)))

    (check-true (=zero? (make-complex-from-mag-ang 0 1)))
    (check-false (=zero? (make-complex-from-mag-ang 1 0)))
))

(run-tests sicp-2.79-tests)
(run-tests sicp-2.80-tests)
