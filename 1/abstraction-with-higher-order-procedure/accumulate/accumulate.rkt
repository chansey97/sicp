; SICP exercise 1.32
;
; a. Show that sum and product (exercise 1.31) are both special cases of a
; still more general notion called accumulate that combines a collection of
; terms, using some general accumulation function:
;
; (accumulate combiner null-value term a next b)
;
; accumulate takes as arguments the same term and range specifications as sum
; and product, together with a combiner procedure (of two arguments) that
; specifies how the current term is to be combined with the accumulation of the
; preceding terms and a null-value that specifies the base value to use when
; the terms run out. Write accumulate and show how both sum and product can be
; defined as simple calls to accumulate.
;
; b. If your accumulate procedure generates a recursive process, write one that
; generates an iterative. If it generates an iterative process, run one that
; generates a recursive process.

#lang racket
(provide (all-defined-out))

;; accumulate right
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;; accumulate left, like for/while loop in C language
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define accumulate-right accumulate)
(define accumulate-left accumulate-iter)

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define (product term a next b)
    (accumulate * 1 term a next b))

  (define (sum term a next b)
    (accumulate + 0 term a next b))

  (define (product-iter term a next b)
    (accumulate-iter * 1 term a next b))

  (define (sum-iter term a next b)
    (accumulate-iter + 0 term a next b))
  
  (define (increment n)
    (+ n 1))
  
  (define (one-over-pow-of-2 n)
    (/ 1 (expt 2 n)))

  (define sicp-1.32-tests
    (test-suite
     "Tests for SICP exercise 1.32"

     (check-equal? (accumulate * 1 identity 1 increment 5) 120)
     (check-equal? (product identity 1 increment 5) 120)
     (check-equal? (sum identity 1 increment 10) 55)
     (check-= (accumulate + 0 one-over-pow-of-2 0 increment 1000) 2 0.001)

     (check-equal? (accumulate-iter * 1 identity 1 increment 5) 120)
     (check-equal? (product-iter identity 1 increment 5) 120)
     (check-equal? (sum-iter identity 1 increment 10) 55)
     (check-= (accumulate-iter + 0 one-over-pow-of-2 0 increment 1000) 2 0.001)
     ))

  (run-tests sicp-1.32-tests)
  )
