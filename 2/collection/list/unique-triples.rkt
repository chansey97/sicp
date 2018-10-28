; SICP exercise 2.41
;
; Write a procedure to find all ordered priples of distinct positive integers
; i, j, and k less than or equal to a given integer n that sums to a given
; integer s.

#lang racket
(provide (all-defined-out))
(require "./flatmap.rkt"
         "./enumerate-interval.rkt")

(define (unique-triples n)
  (flatmap (lambda (a)
             (flatmap (lambda (b)
                        (map (lambda (c) (list a b c))
                             (enumerate-interval (+ b 1) n)))
                      (enumerate-interval (+ a 1) n)))
           (enumerate-interval 1 n)))

(module+ main
  (require rackunit rackunit/text-ui)
  (require "./accumulate.rkt")
  
  (define (triples-sum n s)
    (filter (lambda (triple) (= s (sum triple)))
            (unique-triples n)))
  
  (define (sum numbers)
    (accumulate + 0 numbers))
  
  (define sicp-2.41-tests
    (test-suite
     "Tests for SICP exercise 2.41"

     (check-equal? (triples-sum 5 9) '((1 3 5) (2 3 4)))
     (check-equal? (triples-sum 5 10) '((1 4 5) (2 3 5)))
     ))

  (run-tests sicp-2.41-tests)
  )

