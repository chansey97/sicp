; SICP exercise 2.40
;
; Define a procedure unique-pairs that, given an integer n, generates the
; sequence of pairs (i, j) with 1 ≤ j < i ≤ n. Use unique-pairs to simplify the
; definition of prime-sum-pairs given above.

#lang racket
(provide (all-defined-out))
(require "./flatmap.rkt"
         "./enumerate-interval.rkt")

(define (unique-pairs n)
  (flatmap (lambda (a)
             (map (lambda (b) (list a b))
                  (enumerate-interval (+ a 1) n)))
           (enumerate-interval 1 n)))
