#lang racket
(require rackunit rackunit/text-ui)
(require "simulator.rkt")

(define sicp-5.11-tests
  (test-suite
    "Tests for SICP exercise 5.11"

    (test-case "c. A stack per register"
      (let ((machine (make-machine '(x y)
                                   '()
                                   '((assign x (const 1))
                                     (assign y (const 2))
                                     (save x)
                                     (save y)
                                     (assign x (const 3))
                                     (assign y (const 4))
                                     (restore x)
                                     (restore y)))))
        (start machine)

        (check-eq? (get-register-contents machine 'x) 1)
        (check-eq? (get-register-contents machine 'y) 2)))
))

(run-tests sicp-5.11-tests)
