#lang racket
(require "../simplifier.rkt")
(require "./algebra-rules.rkt")

(define asimp
  (simplifier algebra-rules))

;; (asimp '(* (+ (* 2 x) 1) 3))
;; '(+ 3 (* 6 x))
;; it's good

;; (asimp '(+ (* x y) (* x y)))
;; '(+ (* x y) (* x y))
;; no rule for collect common terms and sums

;; (asimp '(+ (* a (+ x x)) b))
;; '(+ (* a x) (+ (* a x) b))
;; rules can only simplify expression in one direction, and make it no longer be simplified

;; (asimp '(+ (* x y) (* y (+ x 3))))
;; '(+ (* x y) (+ (* 3 y) (* y x)))
;; no rule for collect common terms and sums


