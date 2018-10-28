#lang racket
(require rackunit rackunit/text-ui)
(require "./generic-operations.rkt")

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

(define n1 (make-scheme-number 10))
(define n2 (make-scheme-number 100))
(add n1 n2)

(define rat1 (make-rational 1 2))
(define rat2 (make-rational 3 5))
(add rat1 rat2)

(define complex1 (make-complex-from-real-imag 10 20))
(define complex2 (make-complex-from-real-imag -1 0))
(define complex2x (make-complex-from-mag-ang 1 3.13159))
(add complex1 complex2)
(add complex1 complex2x)
(mul complex1 complex2)
(mul complex1 complex2x)

(define (poly var . coeffs)
  (define (value coeff)
    (cond ((number? coeff) (make-scheme-number coeff))
          (else coeff)))
  (define (to-term-list coeffs)
    (cond ((null? coeffs) '())
          ((=zero? (value (car coeffs))) (to-term-list (cdr coeffs)))
          (else (cons (list (- (length coeffs) 1) (value (car coeffs)))
                      (to-term-list (cdr coeffs))))))

  (make-polynomial var (to-term-list coeffs)))

(define polynomial-tests
  (test-suite
   "Tests for SICP exercise 2.87"

   (test-suite "polynomials"
               (check-equal? (add (poly 'x 1 2 3) (poly 'x 4 5 6))
                             (poly 'x 5 7 9))
               (check-equal? (mul (poly 'x 1 1) (poly 'x 1 -1))
                             (poly 'x 1 0 -1))
               (check-equal? (mul (poly 'x 1 (poly 'y 1 0))
                                  (poly 'x 1 (poly 'y 1 0)))
                             (poly 'x 1 (poly 'y 2 0) (poly 'y 1 0 0)))
               )
   ))

(run-tests polynomial-tests)

;; (add (poly 'x 1 2 3) (make-scheme-number 1))
