#lang racket
(require rackunit rackunit/text-ui)
(require "../constraint-network.rkt")

; Celsius to Fahrenheit converter
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))

    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define celsius-fahrenheit-converter-tests
  (test-suite
    "Tests for SICP exercise 3.33"

    (test-case "celsius-fahrenheit-converter"
      (define C (make-connector))
      (define F (make-connector))
      (celsius-fahrenheit-converter C F)

      (set-value! C 25 'user)
      (check-equal? (get-value F) 77)

      (check-exn exn? (lambda () (set-value! F 212 'user)))

      (forget-value! C 'user)
      (set-value! F 212 'user)
      (check-equal? (get-value C) 100))
))

(run-tests celsius-fahrenheit-converter-tests)


(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Celsius temp" F)

(set-value! C 25 'user)
;; (set-value! F 77 'user) ; ignored
;; (set-value! F 212 'user) ; error
(forget-value! C 'user)
(set-value! F 212 'user)
