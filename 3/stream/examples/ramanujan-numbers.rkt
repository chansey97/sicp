; SICP exercise 3.71
;
; Numbers that can be expressed as the sum of two cubes in more than one way
; are sometimes called Ramanujan numbers, in honor of the mathematician
; Srinivasa Ramanujan. Ordered streams of pairs provide an elegant solution to
; the problem of computing these numbers. To find a number that can be written
; as the sum of two cubes in two different ways, we need only generate the
; stream of pairs of integers (i, j) weighted according to the sum i^3 + j^3
; (see exercise 3.70), then search the stream for two consecutive pairs with
; the same weight. Write a procedure to generate the Ramanujan numbers. The
; first such number is 1,729. What are the next five?

#lang racket
(require rackunit rackunit/text-ui)
(require "../stream.rkt"
         "../integers.rkt"
         "../weighted-pairs.rkt")

(define (ramanujan-numbers)
  (define (cube x) (* x x x))
  (define (weight pair) (+ (cube (car pair)) (cube (cadr pair))))
  (define ordered-integers (weighted-pairs integers integers weight))
  (define (filter-ramanujan stream)
    (let ((p1 (stream-car stream))
          (p2 (stream-car (stream-cdr stream))))
      (if (= (weight p1) (weight p2))
          (stream-cons (weight p1)
                       (filter-ramanujan (stream-cdr stream)))
          (filter-ramanujan (stream-cdr stream)))))
  (filter-ramanujan ordered-integers))

;; (println (stream-take (ramanujan-numbers) 6))

(define sicp-3.71-tests
  (test-suite
   "Tests for SICP exercise 3.71"

   (check-equal? (stream-take (ramanujan-numbers) 6)
                 '(1729 4104 13832 20683 32832 39312))
   ))

(run-tests sicp-3.71-tests)
