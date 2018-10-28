#lang racket
(provide prime?)

;; Searching for divisors
;; Since ancient times, mathematicians have been fascinated by problems
;; concerning prime numbers, and many people have worked on the problem
;; of determining ways to test if numbers are prime. One way to test
;; if a number is prime is to find the number’s divisors. The following program
;; finds the smallest integral divisor (greater than 1) of a given number
;; n. It does this in a straightforward way, by testing n for divisibility
;; by successive integers starting with 2.

;; n is prime if and only if n is its own smallest divisor.
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square a)
  (* a a))
(define (divides? a b)
  (= (remainder b a) 0))

;; (prime? 2)
;; (prime? (- (expt 2 31) 1))


