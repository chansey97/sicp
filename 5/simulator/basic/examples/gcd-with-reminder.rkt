#lang racket
(require "../simulator.rkt")

;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (remainder a b))))

;; (define (remainder n d)
;;   (if (< n d)
;;       n
;;       (remainder (- n d) d)))

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list '- -) (list '= =) (list '< <))

   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (reg a))
     rem-loop
     (test (op <) (reg t) (reg b))
     (branch (label rem-done))
     (assign t (op -) (reg t) (reg b))
     (goto (label rem-loop))
     rem-done
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(get-register-contents gcd-machine 'a)
;; 2
