#lang racket
(require r5rs/init)
(require "../../lazy/basic/evaluator.rkt")

(define (run exp)
  (actual-value exp (setup-environment)))

(run '(begin
        
        (define y-combinator
          (lambda (f)
            ((lambda (x) (f (x x)))
             (lambda (x) (f (x x))))))

        (define expt-f
          (lambda (g)
            (lambda(x n)
              (cond ((= n 0) 1)
                    (else
                     (* x (g x (- n 1))))))))
        
        (define expt (y-combinator expt-f))

        (expt 2 10)))
