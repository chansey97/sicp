; SICP exercise 2.28
;
; Write a procedure fringe that takes as argument a tree (represented as a
; list) and returns a list whose elements are all the leaves of the tree
; arranged in left-to-right order. For example,
;
; (define x (list (list 1 2) (list 3 4)))
;
; (fringe x)
; (1 2 3 4)
;
; (fringe (list x x))
; (1 2 3 4 1 2 3 4)

#lang racket
(provide (all-defined-out))

; Here's a recursive version:

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define fringe enumerate-tree)

; I'm not too happy about it, so I am also going to make an iterative version
; that does not use append or reverse.

(define (fringe-iter tree)
  (define (iter left bottom result)
    (cond ((and (null? left) (null? bottom)) result)
          ((null? bottom) (iter (cdr left) (car left) result))
          ((pair? bottom) (iter (cons (car bottom) left) (cdr bottom) result))
          (else (iter left '() (cons bottom result)))))

  (iter '() tree '()))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-2.28-tests
    (test-suite
     "Tests for SICP exercise 2.28"

     (check-equal? (fringe '((1 2) (3 4))) '(1 2 3 4))
     (check-equal? (fringe '(((1 2) (3 4)) ((1 2) (3 4)))) '(1 2 3 4 1 2 3 4))
     (check-equal? (fringe '(((1 2) 3 4) 5)) '(1 2 3 4 5))
     (check-equal? (fringe '(1 (2 (3 4) 5 (6)) 7)) '(1 2 3 4 5 6 7))
     ))

  (run-tests sicp-2.28-tests)
  )
