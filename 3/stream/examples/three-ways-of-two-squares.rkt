; SICP exercise 3.72
;
; In a similar way to exercise 3.72 generate a stream of all numbers that can
; be written as the sum of two squares in three different ways (showing how
; they can be so written).

#lang racket
(require rackunit rackunit/text-ui)
(require "../stream.rkt"
         "../integers.rkt"
         "../weighted-pairs.rkt")

(define (three-ways-of-two-squares)
  (define (square x) (* x x))
  (define (weight pair) (+ (square (car pair)) (square (cadr pair))))
  (define (uniq stream)
    (if (= (stream-car stream) (stream-car (stream-cdr stream)))
        (uniq (stream-cons (stream-car stream) (stream-cdr (stream-cdr stream))))
        (stream-cons (stream-car stream) (uniq (stream-cdr stream)))))
  (define ordered-integers (weighted-pairs integers integers weight))
  (define (filter-numbers stream)
    (let ((p1 (stream-car stream))
          (p2 (stream-car (stream-cdr stream)))
          (p3 (stream-car (stream-cdr (stream-cdr stream)))))
      (if (= (weight p1) (weight p2) (weight p3))
          (stream-cons (weight p1)
                       (filter-numbers (stream-cdr stream)))
          (filter-numbers (stream-cdr stream)))))
  (uniq (filter-numbers ordered-integers)))

;; (println (stream-take (three-ways-of-two-squares) 20))

(define sicp-3.72-tests
  (test-suite
    "Tests for SICP exercise 3.72"

    (check-equal? (stream-take (three-ways-of-two-squares) 20)
                  '( 325  425  650  725  845  850  925 1025 1105 1250
                    1300 1325 1445 1450 1525 1625 1690 1700 1825 1850))
))

(run-tests sicp-3.72-tests)
