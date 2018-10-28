; SICP exercise 5.14
;
; Measure the number of pushes and the maximum stack depth required to compute
; n! for various small values of n using the factorial machine shown in figure
; 5.11. From your data determine formulas in terms of n for the total number
; of push operations and the maximum stack depth used in computing n! for any
; n > 1. Note that each of these is a linear function of n and is thus
; determined by two constants. In order to get the statistics printed, you
; will have to augment the factorial machine with instructions to initialize
; the stack and print the statistics. You may want to also modify the machine
; so that it repeatedly reads a value for n, computes the factorial, and
; prints the result (as we did for the GCD machine in figure 5.4), so that you
; will not have to repeatedly invoke get-register-contents,
; set-register-contents!, and start.

; The results are:
;
;   Running 1!: (total-pushes = 0 maximum-depth = 0)
;   Running 2!: (total-pushes = 2 maximum-depth = 2)
;   Running 3!: (total-pushes = 4 maximum-depth = 4)
;   Running 4!: (total-pushes = 6 maximum-depth = 6)
;   Running 5!: (total-pushes = 8 maximum-depth = 8)
;   Running 6!: (total-pushes = 10 maximum-depth = 10)
;   Running 7!: (total-pushes = 12 maximum-depth = 12)
;   Running 8!: (total-pushes = 14 maximum-depth = 14)
;   Running 9!: (total-pushes = 16 maximum-depth = 16)
;
; This implies that for computing n!, there are in total 2(n - 1) pushes. This
; number is equal to the maximum depth too.

#lang racket
(provide (all-defined-out))
(require "../simulator.rkt")

(define factorial-machine
  (make-machine
    '(n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '(
        (perform (op initialize-stack))
        (assign continue (label fact-done))
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      fact-done
        (perform (op print-stack-statistics)))))

(define (measure-factorial n)
  (set-register-contents! factorial-machine 'n n)
  (display "Running ")
  (display n)
  (display "!: ")
  (start factorial-machine))

(for ([n (in-range 1 10)])
  (measure-factorial n))
