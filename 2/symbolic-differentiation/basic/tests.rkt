#lang racket
(require "./symbolic-differentiation.rkt")

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; ax^2 + bx + c
;; Note: if no simplify, the deduction shape are the same!

(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'x)
;; '(+ (+ (* a (+ (* x 1) (* 1 x))) (* 0 (* x x))) (+ (+ (* b 1) (* 0 x)) 0))

(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'a)
;; '(+ (+ (* a (+ (* x 0) (* 0 x))) (* 1 (* x x))) (+ (+ (* b 0) (* 0 x)) 0))

(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'b)
;; '(+ (+ (* a (+ (* x 0) (* 0 x))) (* 0 (* x x))) (+ (+ (* b 0) (* 1 x)) 0))


(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'c)
;; '(+ (+ (* a (+ (* x 0) (* 0 x))) (* 0 (* x x))) (+ (+ (* b 0) (* 0 x)) 1))
