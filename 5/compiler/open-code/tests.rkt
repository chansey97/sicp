#lang racket
(require rackunit rackunit/text-ui)
(require "./operations.rkt")
(require "./helpers.rkt")
(require "../../simulator/basic/simulator.rkt")

(define (run exp)
  (define compiled-code (compiled-instructions exp))
  (define machine (make-machine cm-registers cm-operations compiled-code))
  
  (set-register-contents! machine 'env the-global-environment)
  (start machine)
  (get-register-contents machine 'val)
  )

(define sicp-5.38-tests
  (test-suite
   "Tests for SICP exercise 5.38"

   (check-equal? (run '(+ 1 2)) 3)
   (check-equal? (run '(+ 1 2 3)) 6)
   (check-equal? (run '(+ (+ 1 2 3) (+ 4 5 6) (+ 7 8 9))) 45)

   (check-equal? (run '(begin (define a 1)
                              (define b 2)
                              (+ a b)))
                 3)

   (check-equal? (run '(begin (define (a) 1)
                              (+ (a) (a))))
                 2)

   (check-equal? (run '(begin (define (twice n) (+ n n))
                              (define n 4)
                              (+ (twice 1) n (twice 1))))
                 8)

   (check-equal? (run '(begin (define (factorial n)
                                (if (= n 1)
                                    1
                                    (* n (factorial (- n 1)))))
                              (factorial 5)))
                 120)

   (check-equal? (run '(begin (define (factorial n)
                                (define (iter product counter)
                                  (if (> counter n)
                                      product
                                      (iter (* product counter)
                                            (+ counter 1))))
                                (iter 1 1))
                              (factorial 5)))
                 120)
   
   ))

(run-tests sicp-5.38-tests)
