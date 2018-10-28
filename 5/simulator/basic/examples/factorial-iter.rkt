; SICP exercise 5.02
;
; Use the register machine language to describe the iterative factorial
; machine of exercise 5.1.

#lang racket
(require rackunit rackunit/text-ui)
(require "../simulator.rkt")

;; (define (factorial n)
;;   (define (iter product counter)
;;     (if (> counter n)
;;         product
;;         (iter (* counter product)
;;               (+ counter 1))))
;;   (iter 1 1))

(define factorial-machine
  (make-machine
   '(c p n)
   (list (list '+ +) (list '* *) (list '> >))
   '(
       (assign c (const 1))
       (assign p (const 1))
     test->
       (test (op >) (reg c) (reg n))
       (branch (label factorial-done))
       (assign p (op *) (reg c) (reg p))
       (assign c (op +) (reg c) (const 1))
       (goto (label test->))
     factorial-done)))

(set-register-contents! factorial-machine 'n 5)
(start factorial-machine)
(get-register-contents factorial-machine 'p)
