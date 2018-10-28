#lang racket
(require rackunit rackunit/text-ui)
(require "../simulator.rkt")

;; (define (factorial n)
;;   (if (= n 1) 1 (* (factorial (- n 1)) n)))

(define factorial-machine
  (make-machine
    '(n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '(
        (assign continue (label fact-done))
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        ;; Set up for the recursive call by saving n and continue.
        ;; Set up continue so that the computation will continue
        ;; at after-fact when the subroutine returns.
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
        (goto (reg continue))                 ;return to caller
      base-case
        (assign val (const 1))                ;base case: 1! = 1
        (goto (reg continue))                 ;return to caller
      fact-done)))

(set-register-contents! factorial-machine 'n 5)
(start factorial-machine)
(get-register-contents factorial-machine 'val)
