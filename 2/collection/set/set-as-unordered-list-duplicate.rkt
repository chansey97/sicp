; SICP exercise 2.60
;
; We specified that a set would be represented as a list with no duplicates.
; Now suppose we allow duplicates. For instance, the set {1, 2, 3} could be
; represented as the list (2 3 2 1 3 2 2). Design procedures element-of-set?,
; adjoin-set, union-set, and intersection-set that operate on this
; representation. How does the efficiency of each compare with the
; corresponding procedure for the non-duplicate representation? Are there any
; applications for which you would use this representation in preference to the
; non-duplicate one?

; The procedures are below.
;
; In comparison, the set representation in this exercise allows implementing
; adjoin-set and union-set in constant time. In that sense, the implementation
; is way faster than when having no duplicates.
;
; On the down side, element-of-set? can be way slower. Its complexity is жи(n),
; where n is the number of times and element was added to the set, not the
; number of elements in the set. intersection-set is potentially slower for the
; same reason.
;
; I would prefer using the duplicate version when I have a lot of unions and
; adjoins and a lot fewer tests and intersections. I would probably try to
; normalize the set after all the unions, though.

#lang racket
(provide (all-defined-out))

(define empty-set '())

(define (element-of-set? x set)
  (and (not (null? set))
       (or (eq? (car set) x)
           (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-2.60-tests
    (test-suite
     "Tests for SICP exercise 2.60"

     (check-true (element-of-set? 1 '(2 3 2 1 3 2 2)))
     (check-true (element-of-set? 2 '(2 3 2 1 3 2 2)))
     (check-true (element-of-set? 3 '(2 3 2 1 3 2 2)))
     (check-false (element-of-set? 4 '(2 3 2 1 3 2 2)))

     (check-equal? (adjoin-set '1 '()) '(1))
     (check-equal? (adjoin-set '1 '(2 3)) '(1 2 3))
     (check-equal? (adjoin-set '2 '(3 2 1)) '(2 3 2 1))

     (check-equal? (union-set '(1 2) '(3 4)) '(1 2 3 4))
     (check-equal? (union-set '(1 2) '(1 3)) '(1 2 1 3))

     (check-equal? (intersection-set '(1 2 3) '(2 3 4)) '(2 3))
     (check-equal? (intersection-set '(1 2 2 3 3) '(2 3 3 4 4 4)) '(2 2 3 3))
     (check-equal? (intersection-set '(1 2 3) '(2 2 2 3 3 4 4 4)) '(2 3))
     ))

  (run-tests sicp-2.60-tests)

  
  ;; (element-of-set? 1 '(2 3 2 1 3 2 2))
  ;; (adjoin-set 4 '(1 2 3))
  ;; (union-set '(1 2 3) '(3 4 5))
  )

