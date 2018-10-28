#lang racket
(require "./symbolic-differentiation.rkt")

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
;; '(+ (* x y) (* y (+ x 3)))
;; can't collect common terms and sums

;; ax^2 + bx + c

(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'x)


(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'a)

(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'b)

(deriv '(+ (* a (* x x))
           (+ (* b x)
              c)) 'c)

