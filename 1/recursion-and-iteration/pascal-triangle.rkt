; SICP exercise 1.12
;
; The following pattern of numbers is called Pascal's triangle.
;
;       1
;      1 1
;     1 2 1
;    1 3 3 1
;   1 4 6 4 1
;      ...
;
; The numbers at the edge of the triange are all 1, and each number inside the
; triangle is the sum of the two numbers above it. Write a procedure that
; computes the elements of Pascal's triangle by means of recursive process.

#lang racket
(require rackunit rackunit/text-ui)

(define (binom row index)
  (cond ((= index 1) 1)
        ((= index row) 1)
        (else (+ (binom (- row 1) (- index 1))
                 (binom (- row 1) index)))))

(define sicp-1.12-tests
  (test-suite
    "Tests for SICP exercise 1.12"

    (check-equal? (binom 1 1) 1)
    (check-equal? (binom 2 1) 1)
    (check-equal? (binom 2 2) 1)
    (check-equal? (binom 3 1) 1)
    (check-equal? (binom 3 2) 2)
    (check-equal? (binom 3 3) 1)
    (check-equal? (binom 4 1) 1)
    (check-equal? (binom 4 2) 3)
    (check-equal? (binom 4 3) 3)
    (check-equal? (binom 4 4) 1)
    (check-equal? (binom 5 1) 1)
    (check-equal? (binom 5 2) 4)
    (check-equal? (binom 5 3) 6)
    (check-equal? (binom 5 4) 4)
    (check-equal? (binom 5 5) 1)
))

(run-tests sicp-1.12-tests)
