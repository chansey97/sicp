#lang racket

;; Another way to implement scale-tree is to regard the tree as a sequence
;; of sub-trees and use map. We map over the sequence, scaling
;; each sub-tree in turn, and return the list of results. In the base case,
;; where the tree is a leaf, we simply multiply by the factor:

;; A pattern:
;; Call map in some function, and map's op-lambda recursive call the function.
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
