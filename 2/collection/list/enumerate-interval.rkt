#lang racket
(provide (all-defined-out))

(define (enumerate-interval a b)
  (if (> a b)
      (list)
      (cons a
            (enumerate-interval (+ a 1) b))))
