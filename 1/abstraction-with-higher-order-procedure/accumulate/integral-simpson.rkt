; SICP exercise 1.29
;
; Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpson's Rule, the integral of a function f
; between a and b is approximated as
;
; h
; ─(y₀ + 4y₁ + 2y₂ + 4y₃ + 2y₄ + ... + 2yₙ₋₂ + 4yₙ₋₁ + yₙ)
; 3
;
; where h = (b - a)/n, for some even integer n, and yₖ = f(a + kh). (Increasing
; n increases the accuracy of the approximation.) Define a procedure that takes
; as arguments f, a, b and n and returns the value of the integral, computed
; using Simpson's Rule. Use your procedure to integrate cube between 0 and 1
; (with n = 100 and n = 1000), and compare the results to those of the integral
; procedure shown above.

#lang racket
(require "./sum.rkt")

(define (simpson-integral f a b n) 
  (define fixed-n (round-to-next-even n)) 
  (define h (/ (- b a) fixed-n)) 
  (define (simpson-term k) 
    (define y (f (+ a (* k h)))) 
    (if (or (= k 0) (= k fixed-n)) 
        (* 1 y) 
        (if (even? k) 
            (* 2 y) 
            (* 4 y)))) 
  (* (/ h 3) (sum simpson-term 0 inc fixed-n))) 

(define (round-to-next-even x) 
  (+ x (remainder x 2)))

(define (inc n) (+ n 1)) 

(module+ main
  (require rackunit rackunit/text-ui)
  (require "./integral.rkt")
  
  (define (cube x) (* x x x))

  (define sicp-1.29-tests
    (test-suite
     "Tests for SICP exercise 1.29"

     (check-= (integral cube 0 1 0.001) 0.25 0.001)
     (check-= (integral (lambda (x) x) 0 1 0.001) 0.5 0.001)
     
     (check-= (simpson-integral cube 0 1 100) 0.25 0.01)
     (check-= (simpson-integral (lambda (x) x) 0 1 100) 0.5 0.01)
     (check-= (simpson-integral (lambda (x) x) 0 1 1000) 0.5 0.001)
     ))

  (run-tests sicp-1.29-tests)
  )

