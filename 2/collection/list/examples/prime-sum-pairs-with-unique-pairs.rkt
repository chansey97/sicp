#lang racket
(require rackunit rackunit/text-ui)
(require "../list.rkt")

(define (prime-sum-pairs n)
  (filter (lambda (pair)
            (prime? (+ (car pair)
                       (cadr pair))))
          (unique-pairs n)))

(define (prime? n)
  (null?
   (filter (lambda (x) (= 0 (remainder n x)))
           (enumerate-interval 2 (- n 1)))))

(define sicp-2.40-tests
  (test-suite
    "Tests for SICP exercise 2.40"

    (check-equal? (enumerate-interval 1 5) '(1 2 3 4 5))

    (check-equal? (unique-pairs 2) '((1 2)))
    (check-equal? (unique-pairs 3) '((1 2) (1 3) (2 3)))
    (check-equal? (unique-pairs 4) '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))

    (check-equal? (prime-sum-pairs 6) '((1 2) (1 4) (1 6) (2 3) (2 5) (3 4) (5 6)))
))

(run-tests sicp-2.40-tests)
