; SICP exercise 2.61
;
; Give an implementation of adjoin-set using the ordered representation. By
; analogy with element-of-set? show how to take advantage of the ordering to
; produce a procedure that requires on the average about half as many steps as
; with the unordered representation.

; SICP exercise 2.62
;
; Given an жи(n) implementation of union-set for the sets represented as ordered
; lists.

#lang racket
(provide (all-defined-out))

(define empty-set '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        ((< x (car set)) (cons x set))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-2.61-tests
    (test-suite
     "Tests for SICP exercise 2.61"

     (check-equal? (adjoin-set 0 '()) '(0))

     ;; Note: input set must be ordered
     (check-equal? (adjoin-set 0 '(1 3 5)) '(0 1 3 5))
     (check-equal? (adjoin-set 1 '(1 3 5)) '(1 3 5))
     (check-equal? (adjoin-set 2 '(1 3 5)) '(1 2 3 5))
     (check-equal? (adjoin-set 3 '(1 3 5)) '(1 3 5))
     (check-equal? (adjoin-set 4 '(1 3 5)) '(1 3 4 5))
     (check-equal? (adjoin-set 5 '(1 3 5)) '(1 3 5))
     (check-equal? (adjoin-set 6 '(1 3 5)) '(1 3 5 6))
     (check-equal? (adjoin-set 7 '(1 3 5)) '(1 3 5 7))
     ))

  (define sicp-2.62-tests
    (test-suite
     "Tests for SICP exercise 2.62"

     (check-equal? (union-set '() '()) '())
     (check-equal? (union-set '(1) '()) '(1))
     (check-equal? (union-set '() '(1)) '(1))

     (check-equal? (union-set '(1) '(2)) '(1 2))
     (check-equal? (union-set '(2) '(1)) '(1 2))

     (check-equal? (union-set '(1 3) '(2)) '(1 2 3))
     (check-equal? (union-set '(1) '(2 3)) '(1 2 3))
     (check-equal? (union-set '(2) '(1 3)) '(1 2 3))
     (check-equal? (union-set '(1 2) '(3)) '(1 2 3))

     (check-equal? (union-set '(1 2) '(1 3)) '(1 2 3))
     (check-equal? (union-set '(1 2 3) '(1 2)) '(1 2 3))
     (check-equal? (union-set '(1 2) '(1 2 3)) '(1 2 3))

     (check-equal? (union-set '(1 3 5 7 9) '(2 4 6 8 10)) '(1 2 3 4 5 6 7 8 9 10))
     ))

  (run-tests sicp-2.61-tests)
  (run-tests sicp-2.62-tests)
  )
