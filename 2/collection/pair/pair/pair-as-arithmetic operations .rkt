; SICP exercise 2.05
;
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair a and b as the integer
; that is the product 2ª3ᵇ. Give the corresponding definitions of the
; procedures cons, car and cdr.

#lang racket
(require rackunit rackunit/text-ui)

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car pair)
  (count-divisor pair 2))

(define (cdr pair)
  (count-divisor pair 3))

(define (count-divisor number divisor)
  (define (iter number result)
    (if (= (remainder number divisor) 0)
        (iter (quotient number divisor) (+ result 1))
        result))
  (iter number 0))

(define sicp-2.05-tests
  (test-suite
    "Tests for SICP exercise 2.05"

    (check-equal? (car (cons 5 7)) 5)
    (check-equal? (cdr (cons 5 7)) 7)

    (check-equal? (car (cons 0 7)) 0)
    (check-equal? (cdr (cons 5 0)) 0)
))

(run-tests sicp-2.05-tests)

;; This is Gödel numbering, that means:  
;; (Nat, Nat) ~ Nat
;; They are is bijective, has same cardinality, even if (Nat, Nat) and Nat are infinite set!

;; f :: (Nat, Nat) -> Nat 
;; f (a, b) = 2^a * 3^b
;; f^-1 :: Nat -> Nat, Nat) 
;; f^-1 n = (a = number of factors of 2, b = number of factors of 3) ; fundamental theorem of arithmetic

