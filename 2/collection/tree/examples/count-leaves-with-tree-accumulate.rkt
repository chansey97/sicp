#lang racket
(require "../tree-accumulate.rkt")

(define (count-leaves tree)
  (tree-accumulate (lambda (_ y) (+ y 1)) 0 tree))

(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)

(list x x)
(length (list x x))
(count-leaves (list x x))
