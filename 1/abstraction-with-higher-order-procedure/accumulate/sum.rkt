; SICP exercise 1.30
;
; The sum procedure above generates a linear recursion. The procedure can be
; rewritten so that the sum is performed iteratively. Show how to do this
; by filling in the missing expressions in the following definition:
;
; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))
#lang racket
(provide (all-defined-out))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a )
         (sum term (next a) next b ))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-1.30-tests
    (test-suite
     "Tests for SICP exercise 1.30"

     (check-equal? (sum-iter (lambda (x) x) 1 (lambda (x) (+ 1 x)) 100) 5050)
     (check-equal? (sum-iter (lambda (x) x) 0 (lambda (x) (+ 1 x)) 1) 1)
     (check-equal? (sum-iter (lambda (x) x) 0 (lambda (x) (+ 1 x)) 0) 0)
     (check-equal? (sum-iter (lambda (x) x) 0 (lambda (x) (+ 2 x)) 10) 30)

     (check-= (sum-iter (lambda (x) (/ 1 (expt 2 x))) 1.0 (lambda (x) (+ 1 x)) 100) 1.0 0.01)
     ))

  (run-tests sicp-1.30-tests)
  )
