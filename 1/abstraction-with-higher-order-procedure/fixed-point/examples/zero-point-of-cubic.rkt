; SICP exercise 1.40
;
; Define a procedure cubic that can be used together with newtons-method
; procedure in expressions of the form
;
; (newtons-method (cubic a b c) 1)
;
; to approximate zeroes of the cubic x³ + ax² + bx + c

; It is simpler than it sounds - we just define a lambda, that computes
; x³ + ax² + bx + c

#lang racket
(require rackunit rackunit/text-ui)
(require "../newton-method.rkt")

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;; zeroes point of the cubic x³ + x² + 2x + 3
(newtons-method (cubic 1 2 3) 1)

(define sicp-1.40-tests
  (test-suite
    "Tests for SICP exercise 1.40"

    (check-= (newtons-method (cubic 0 0 -1) 2.0) 1.0 0.00001)
    (check-= (newtons-method (cubic 0 0 -27) 2.0) 3.0 0.00001)
))

(run-tests sicp-1.40-tests)
