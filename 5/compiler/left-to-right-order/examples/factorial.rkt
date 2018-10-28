#lang racket
(require "../helpers.rkt")

(define factorial-code
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(pretty-print (compiled-instructions factorial-code))
