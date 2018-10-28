#lang racket
(require "../tree-map.rkt")

(define (scale-tree tree factor)
  (tree-map (lambda (x) (* x factor)) tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
