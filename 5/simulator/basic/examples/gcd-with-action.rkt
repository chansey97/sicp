#lang racket
(require "../simulator.rkt")

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder)
         (list '= =)
         (list 'read read)
         (list 'print print))

   '(gcd-loop
     (assign a (op read))
     (assign b (op read))
     test-b
     (test (op =)
           (reg b)
           (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done
     (perform (op print)
              (reg a))
     (goto (label gcd-loop)))))

;; input 2 number and output gcd result, then go to input loop
(start gcd-machine)


