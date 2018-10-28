; SICP exercise 2.31
;
; Abstract your answers to exercise 2.30 to produce a procedure tree-map with
; the property that square-tree could be defined as
;
; (define (square-tree tree) (tree-map square tree))

#lang racket
(provide (all-defined-out))

(define (tree-map function tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (function tree))
        (else (cons (tree-map function (car tree))
                    (tree-map function (cdr tree))))))

;; Using map to define tree-map
;; (define (tree-map function tree)
;;   (map (lambda (sub-tree)
;;          (if (pair? sub-tree)
;;              (tree-map function sub-tree)
;;              (function sub-tree)))
;;        tree)
;;   )

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define (square-tree tree)
    (tree-map square tree))

  (define (square x)
    (* x x))

  (define sicp-2.31-tests
    (test-suite
     "Tests for SICP exercise 2.31"

     (check-equal? (square-tree '(1 (2 (3 4) 5) (6 7)))
                   '(1 (4 (9 16) 25) (36 49)))

     (check-equal? (tree-map (lambda (x) (+ x 1))
                             '(1 (2 (3 4) 5) (6 7)))
                   '(2 (3 (4 5) 6) (7 8)))
     ))

  (run-tests sicp-2.31-tests)
  )

