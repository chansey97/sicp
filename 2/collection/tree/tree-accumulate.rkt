#lang racket
(provide (all-defined-out))
(require "../list/list.rkt")

(define (tree-accumulate op initial tree)
  (cond ((null? tree) initial)
        ((not (pair? tree)) (op tree initial))
        (else (accumulate (lambda (subtree value) (tree-accumulate op value subtree)) initial tree))))

(module+ main
  (require "./fringe.rkt")
  
  (define (fringe-with-tree-accumulate tree)
    (tree-accumulate cons '() tree))
  

  (fringe '(((1 2) (3 4)) ((1 2) (3 4))))
  (fringe '(1 (2 (3 4) 5 (6)) 7))
  
  (fringe-with-tree-accumulate '(((1 2) (3 4)) ((1 2) (3 4))))
  (fringe-with-tree-accumulate '(1 (2 (3 4) 5 (6)) 7))
  
  )
