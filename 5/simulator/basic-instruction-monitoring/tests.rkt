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

;; Exercise 5.15

(define sicp-5.15-tests
  (test-suite
    "Tests for SICP exercise 5.15"

    (test-begin "Counting instructions"
      (let ((machine (make-machine '(a) '()
                                   '(begin
                                       (goto (label middle))
                                       (assign a (const 1))
                                     middle
                                       (assign a (const 2))
                                       (assign a (const 3))
                                     end))))
        (start machine)
        (check-eq? (machine 'instruction-count) 3)
        (check-eq? (machine 'instruction-count) 0)))
))

;; Exercise 5.16

(define instructions '())
(define (get-instructions)
  (reverse instructions))
(define (collect-instructions inst)
  (set! instructions (cons inst instructions)))
(define (reset-tracing!)
  (set! instructions '()))

(define machine
  (make-machine '(a) '()
                '(begin
                    (goto (label middle))
                    (assign a (const 1))
                  middle
                    (assign a (const 2))
                    (assign a (const 3))
                  end)))

(define sicp-5.16-tests
  (test-suite
    "Tests for SICP exercise 5.16"

    (test-begin "Tracing instructions"
      (reset-tracing!)
      (machine 'trace-on)
      ((machine 'install-trace-proc) collect-instructions)
      (start machine)
      (check-equal? (get-instructions)
                    '((goto (label middle))
                      (assign a (const 2))
                      (assign a (const 3)))))

    (test-begin "Tracing turned off"
      (reset-tracing!)
      (machine 'trace-off)
      ((machine 'install-trace-proc) collect-instructions)
      (start machine)
      (check-equal? (get-instructions) '()))
))


(run-tests simulator-tests)
(run-tests sicp-5.15-tests)
(run-tests sicp-5.16-tests)
