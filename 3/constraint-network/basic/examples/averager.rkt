; SICP exercise 3.33
;
; Using primitive multiplier, adder, and constant constraints, define a
; procedure averager that takes three connectors a, b and c as inputs and
; establishes the constraint that the value of c is average of the values of a
; and b.

#lang racket
(require rackunit rackunit/text-ui)
(require "../constraint-network.rkt")

; Averager
; c = (a + b)/2
;
; 2c = a + b
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))

    (adder a b x)
    (multiplier c y x)
    (constant 2 y)
    'ok))

(define sicp-3.33-tests
  (test-suite
    "Tests for SICP exercise 3.33"

    (test-case "averager"
      (define a (make-connector))
      (define b (make-connector))
      (define c (make-connector))

      (averager a b c)

      (set-value! a 4 'user)
      (set-value! b 6 'user)
      (check-equal? (get-value c) 5)

      (forget-value! b 'user)
      (set-value! c 5 'user)
      (check-equal? (get-value b) 6)

      (forget-value! a 'user)
      (set-value! b 6 'user)
      (check-equal? (get-value a) 4))
))

(run-tests sicp-3.33-tests)


;; (define A (make-connector))
;; (define B (make-connector))
;; (define C (make-connector))
;; (averager A B C)

;; (probe "Averager" A)
;; (probe "Averager" B)
;; (probe "Averager" C)

;; (set-value! A 100 'user)
;; (set-value! B 50 'user)
