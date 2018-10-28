#lang racket
(require "../sum.rkt")

;; no higer order function
;; (define (sum-cubes a b)
;;   (if (> a b)
;;       0
;;       (+ (cube a)
;;          (sum-cubes (+ a 1) b))))

(define (inc n) (+ n 1))
(define (cube x) (* x x x ))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)
