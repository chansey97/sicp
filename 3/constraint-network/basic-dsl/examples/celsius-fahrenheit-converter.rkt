#lang racket
(require rackunit rackunit/text-ui)
(require "../constraint-network.rkt")

; Celsius to Fahrenheit converter

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define sicp-3.37-tests
  (test-suite
    "Tests for SICP exercise 3.37"

    (test-case "celsius-fahrenheit-converter"
      (define C (make-connector))
      (define F (celsius-fahrenheit-converter C))

      (set-value! C 25 'user)
      (check-equal? (get-value F) 77)

      (check-exn exn? (lambda () (set-value! F 212 'user)))

      (forget-value! C 'user)
      (set-value! F 212 'user)
      (check-equal? (get-value C) 100))
))

(run-tests sicp-3.37-tests)
