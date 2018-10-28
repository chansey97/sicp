#lang racket
(require "../z-combinator.rkt")

(define factorial-f
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

(define factorial (z-combinator factorial-f))

(factorial 0)  ;1
(factorial 5)  ;120
