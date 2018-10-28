#lang racket
(require rackunit rackunit/text-ui)
(require "simulator.rkt")

(define sicp-5.11-tests
  (test-suite
   "Tests for SICP exercise 5.11"

   (test-case "b. Erroring out on restoring wrong register"
     (check-exn (regexp "Mismatching registers: x \\(restore y\\)")
                (lambda ()
                  (start (make-machine '(x y)
                                       '()
                                       '((assign x (const 1))
                                         (save x)
                                         (restore y)))))))
   ))

(run-tests sicp-5.11-tests)
