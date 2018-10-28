; SICP exercise 2.59
;
; Implement the union-set operation for the unordered list representation of
; sets.

#lang racket
(provide (all-defined-out))

(define empty-set '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-2.59-tests
    (test-suite
     "Tests for SICP exercise 2.59"

     (check-equal? (union-set '(1 2) '(3 4))
                   '(1 2 3 4))

     (check-equal? (union-set '(1 2 3) '(2 4))
                   '(1 3 2 4))
     ))

  (run-tests sicp-2.59-tests)
  
  ;; (element-of-set? 1 '(1 2 3))
  ;; (adjoin-set 4 '(1 2 3))
  ;; (union-set '(1 2 3) '(3 4 5))
  )

