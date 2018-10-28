#lang racket
(require "../helpers.rkt")

(define factorial-alt-code
  '(define (factorial-alt n)
     (if (= n 1)
       1
       (* n (factorial-alt (- n 1))))))

(pretty-print (compiled-instructions factorial-alt-code))
