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

(define sicp-2.92-tests
  (test-suite
   "Tests for SICP exercise 2.92"
   
    ;; One can impose a towerlike structure on this by ordering the variables
    ;; and thus always converting any polynomial to a “canonical form” with
    ;; the highest-priority variable dominant and the lower-priority variables
    ;; buried in the coefficients. This strategy works fairly well, except that
    ;; the conversion may expand a polynomial unnecessarily, making it hard
    ;; to read and perhaps less efficient to work with.

    (check-equal? (add (poly 'x 1 0) (poly 'y 1 0))
                  (poly 'x 1 (poly 'y 1 0)))
    (check-equal? (add (poly 'y 1 0) (poly 'x 1 0))
                  (poly 'x 1 (poly 'y 1 0)))

    (check-equal? (mul (poly 'x 1 2 0) (poly 'y 1 0))
                  (poly 'x (poly 'y 1 0) (poly 'y 2 0) 0))
    (check-equal? (mul (poly 'y 1 0) (poly 'x 1 2 0))
                  (poly 'x (poly 'y 1 0) (poly 'y 2 0) 0))

    (check-equal? (sub (poly 'x 1 0) (poly 'y 1 0))
                  (poly 'x 1 (poly 'y -1 0)))
    (check-equal? (sub (poly 'y 1 0) (poly 'x 1 0))
                  (poly 'x -1 (poly 'y 1 0)))
))

(run-tests sicp-2.92-tests)
