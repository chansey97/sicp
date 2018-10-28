#lang racket
(require r5rs/init)
(require "evaluator.rkt")

(define (run exp)
  (actual-value exp (setup-environment)))

(run '(begin (define (p) (p))

       (define (test x y)
         (if (= x 0)
             0
             y))

       (test 0 (p))))
