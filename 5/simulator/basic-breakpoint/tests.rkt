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

;; Exercise 5.19

(define (test-machine)
  (make-machine '(a)
                '()
                '(start
                    (assign a (const 1))
                    (assign a (const 2))
                    (assign a (const 3))
                    (assign a (const 4))
                    (assign a (const 5))
                  before-six
                    (assign a (const 6))
                    (assign a (const 7))
                    (assign a (const 8))
                    (assign a (const 9))
                    (assign a (const 10)))))

(define sicp-5.19-tests
  (test-suite
    "Tests for SICP exercise 5.19"

    (test-case "Checking breakpoints"
      (define machine (test-machine))

      (set-breakpoint machine 'start 3)
      (set-breakpoint machine 'start 5)
      (set-breakpoint machine 'before-six 3)

      (start machine)
      (check-eq? (get-register-contents machine 'a) 2)

      (proceed-machine machine)
      (check-eq? (get-register-contents machine 'a) 4)

      (proceed-machine machine)
      (check-eq? (get-register-contents machine 'a) 7))

    (test-case "Canceling breakpoints"
      (define machine (test-machine))

      (set-breakpoint machine 'start 3)
      (set-breakpoint machine 'start 5)
      (set-breakpoint machine 'before-six 3)

      (cancel-breakpoint machine 'start 3)

      (start machine)
      (check-eq? (get-register-contents machine 'a) 4)

      (cancel-all-breakpoints machine)
      (proceed-machine machine)
      (check-eq? (get-register-contents machine 'a) 10))
))

(run-tests simulator-tests)
(run-tests sicp-5.19-tests)
