#lang racket
(require "../stream.rkt"
         "../integers.rkt")

(define (square x)
  (* x x))

(define (divisible? x y) (= (remainder x y) 0))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;; Defining streams implicitly
(define primes
  (stream-cons
   2
   (stream-filter prime? (integers-starting-from 3))))

(stream-ref primes 50)

