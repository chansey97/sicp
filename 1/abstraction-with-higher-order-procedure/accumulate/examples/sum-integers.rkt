#lang racket
(require "../sum.rkt")

;; no higer order function
;; (define (sum-integers a b)
;;   (if (> a b)
;;       0
;;       (+ a (sum-integers (+ a 1) b))))

(define (inc n) (+ n 1))
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 100)
