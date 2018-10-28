#lang racket
(require "../sum.rkt")

; π    2     2      2 
; ─ = ─── + ─── + ───── + ...
; 8   1·3   5·7    9·11

;; no higer order function
;; (define (pi-sum a b)
;;   (if (> a b)
;;       0
;;       (+ (/ 1.0 (* a (+ a 2)))
;;          (pi-sum (+ a 4) b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(pi-sum 1 1000)
(/ 3.14 8)
