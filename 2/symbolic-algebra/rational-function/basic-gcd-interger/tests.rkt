#lang racket
(require rackunit rackunit/text-ui)
(require "./generic-operations.rkt")

(define (poly var . coeffs)
  (define (to-term-list coeffs)
    (cond ((null? coeffs) '())
          ((=zero? (car coeffs)) (to-term-list (cdr coeffs)))
          (else (cons (list (- (length coeffs) 1) (car coeffs))
                      (to-term-list (cdr coeffs))))))

  (make-polynomial var (to-term-list coeffs)))

(define p1 (poly 'x 1 -2 1))
(define p2 (poly 'x 11 0 7))
(define p3 (poly 'x 13 5))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(define sicp-2.96-tests
  (test-suite
    "Tests for SICP exercise 2.96"

    (check-equal? (greatest-common-divisor q1 q2) p1)
))

(run-tests sicp-2.96-tests)

;; In an implementation like MIT Scheme, this produces a polynomial that is indeed
;; a divisor of Q1 and Q2, but with rational coefficients. In many other Scheme systems,
;; in which division of integers can produce limited-precision decimal numbers, we may
;; fail to get a valid divisor.

; SICP exercise 2.95
;
; ₁₂₃
; Define P₁, P₂, and P₃ to be the polynomials:
;
;     P₁: x² - 2x + 1
;     P₂: 11x² + 7
;     P₃: 13x + 5
;
; Now define Q₁ to be the product of P₁ and P₂ and Q₂ to be the product of P₁
; and P₃, and use greated-common-divisor (exercise 2.94) to compute the GCD of
; Q₁ and Q₂. Note that the answer is not the same as P₁. This example
; introduces noninteger operations into the computation, causing difficulties
; with the GCD algorithms. To understand what is happening, try tracing
; gcd-terms while computing the GCD or try performing the division by hand.

(greatest-common-divisor (poly 'x 11 -22 18 -14 7)
                         (poly 'x 13 -21 3 5))

;; '(polynomial x (2 1458/169) (1 -2916/169) (0 1458/169))

; As you can see, it goes into divison of non-integers. Since Racket supports
; rational numbers, this is actually a GCD of Q₁ and Q₂, although with
; rational coefficients (as the footnote indicates). It can easily produce
; rounding errors in case of floating-point precision.

;; we get integer coefficients
;; '(polynomial x (2 1) (1 -2) (0 1))

;; numbers are not good, in fraction in these div...
;; (div (poly 'x 11 -22 18 -14 7) (poly 'x 1 -2 1))
;; (div (poly 'x 13 -21 3 5) (poly 'x 1 -2 1))
