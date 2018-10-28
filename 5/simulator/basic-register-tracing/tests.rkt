#lang racket
(require rackunit rackunit/text-ui)
(require "simulator.rkt")
(require "sample-machines.rkt")

(define simulator-tests
  (test-suite "Register machine simulator tests"

    (test-case "(gcd 206 40)"
      (set-register-contents! gcd-machine 'a 206)
      (set-register-contents! gcd-machine 'b 40)
      (start gcd-machine)

      (check-eq? (get-register-contents gcd-machine 'a)
                 2))

    (test-case "(gcd 54 24)"
      (set-register-contents! gcd-machine 'a 54)
      (set-register-contents! gcd-machine 'b 24)
      (start gcd-machine)

      (check-eq? (get-register-contents gcd-machine 'a)
                 6))

    (test-case "(gcd 13 7)"
      (set-register-contents! gcd-machine 'a 13)
      (set-register-contents! gcd-machine 'b 7)
      (start gcd-machine)

      (check-eq? (get-register-contents gcd-machine 'a)
                 1))

    (test-case "(factorial 5)"
      (set-register-contents! factorial-machine 'n 5)
      (start factorial-machine)

      (check-eq? (get-register-contents factorial-machine 'val)
                 120))

    (test-case "(fibonacci 8)"
      (set-register-contents! fibonacci-machine 'n 8)
      (start fibonacci-machine)

      (check-eq? (get-register-contents fibonacci-machine 'val)
                 21))
))

;; Exercise 5.18

(define traces '())
(define (get-traces)
  (reverse traces))
(define (collect-traces name old new)
  (set! traces (cons `(,name old = ,old new = ,new) traces)))
(define (reset-tracing!)
  (set! traces '()))

(define machine
  (make-machine '(a) '()
                '((assign a (const 1))
                  (assign a (const 2)))))

(define sicp-5.18-tests
  (test-suite
    "Tests for SICP exercise 5.18"

    (test-case "Tracing a register"
      (reset-tracing!)
      ((machine 'install-register-trace-proc) collect-traces)
      ((machine 'register-trace-on) 'a)

      (start machine)

      (check-equal? (get-traces)
                    '((a old = *unassigned* new = 1)
                      (a old = 1            new = 2))))

    (test-case "Tracing turned off"
      (reset-tracing!)
      ((machine 'install-register-trace-proc) collect-traces)
      ((machine 'register-trace-off) 'a)

      (start machine)

      (check-equal? (get-traces) '()))
))

(run-tests simulator-tests)
(run-tests sicp-5.18-tests)
