#lang racket
(require rackunit rackunit/text-ui)
(require "simulator.rkt")

(define fibonacci-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '(
        (assign continue :fib-done)
      fib-loop
        (test < @n 2)
        (branch :immediate-answer)
        (save continue)
        (assign continue :after-fib-n-1)
        (save n)
        (assign n - @n 1)
        (goto :fib-loop)
      after-fib-n-1
        (restore n)
        (restore continue)
        (assign n - @n 2)
        (save continue)
        (assign continue :after-fib-n-2)
        (save val)
        (goto :fib-loop)
      after-fib-n-2
        (assign n @val)
        (restore val)
        (restore continue)
        (assign val + @val @n)
        (goto @continue)
      immediate-answer
        (assign val @n)
        (goto @continue)
      fib-done)))

(define sicp-5.10-tests
  (test-suite
    "Tests for SICP exercise 5.10"

    (test-case "(fibonacci 8)"
      (set-register-contents! fibonacci-machine 'n 8)
      (start fibonacci-machine)

      (check-eq? (get-register-contents fibonacci-machine 'val)
                 21))
))

(run-tests sicp-5.10-tests)
