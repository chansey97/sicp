#lang racket
(require "../stream.rkt"
         "../integers.rkt")

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)
