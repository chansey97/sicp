#lang racket
(require r5rs/init)
(require "../evaluator.rkt")

(define (a-value-of exp)
  (ambeval exp
           (setup-environment)
           (lambda (value fail) value)
           (lambda () '())))

(define (all-values exp)
  (ambeval exp
           (setup-environment)
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(displayln
 ;; (one-value
 (all-values
  '(begin
     
     (define (square x)
       (* x x))

     (define (smallest-divisor n) (find-divisor n 2))

     (define (find-divisor n test-divisor)
       (cond ((> (square test-divisor) n) n)
             ((divides? test-divisor n) test-divisor)
             (else (find-divisor n (+ test-divisor 1)))))

     (define (divides? a b) (= (remainder b a) 0))

     (define (prime? n)
       (= n (smallest-divisor n)))

     (define (prime-sum-pair list1 list2)
       (let ((a (an-element-of list1))
             (b (an-element-of list2)))
         (require (prime? (+ a b)))
         (list a b)))
     
     (prime-sum-pair '(1 3 5 8) '(20 35 110))
     )

  ))
