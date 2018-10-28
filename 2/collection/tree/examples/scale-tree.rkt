#lang racket

;; Just as map is a powerful abstraction for dealing with sequences, map
;; together with recursion is a powerful abstraction for dealing with trees.
;; For instance, the scale-tree procedure, analogous to scale-list of
;; Section 2.2.1, takes as arguments a numeric factor and a tree whose
;; leaves are numbers. It returns a tree of the same shape, where each
;; number is multiplied by the factor. The recursive plan for scale-tree
;; is similar to the one for count-leaves:

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
