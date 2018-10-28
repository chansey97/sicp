#lang racket
(require rackunit rackunit/text-ui)
(require "../simulator.rkt")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define fibonacci-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '(
        (assign continue (label fib-done))
      fib-loop  ; when at this point, there is a contract: N is contains args, continue is recipent
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        (save continue)
        (assign continue (label after-fib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
      after-fib-n-1
        (restore n)
        (restore continue) ; can be optimized by peephole
        (assign n (op -) (reg n) (const 2))
        (save continue)    ; can be optimized by peephole
        (assign continue (label after-fib-n-2))
        (save val)
        (goto (label fib-loop))
      after-fib-n-2
        (assign n (reg val))
        (restore val)
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
        fib-done)))

(set-register-contents! fibonacci-machine 'n 8)
(start fibonacci-machine)
(get-register-contents fibonacci-machine 'val)
