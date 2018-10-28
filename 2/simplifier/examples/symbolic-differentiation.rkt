#lang racket
(require "../simplifier.rkt")
(require "./deriv-rules.rkt")
(require "./algebra-rules.rkt")

(define dsimp
  (simplifier deriv-rules))

(define asimp
  (simplifier algebra-rules))

;; (dsimp '(dd (+ x y) x))
;; (asimp (dsimp '(dd (+ x y) x)))

;; (dsimp '(dd (+ x 3) x))
(asimp (dsimp '(dd (+ x 3) x)))

;; (dsimp '(dd (* x y) x))
(asimp (dsimp '(dd (* x y) x)))

;; (dsimp '(dd (* (* x y) (+ x 3)) x))
(asimp (dsimp '(dd (* (* x y) (+ x 3)) x)))

;; ax^2 + bx + c

(asimp (dsimp '(dd (+ (* a (* x x))
                      (+ (* b x)
                         c))
                   x)))

(asimp (dsimp '(dd (+ (* a (* x x))
                      (+ (* b x)
                         c))
                   a)))

(asimp (dsimp '(dd (+ (* a (* x x))
                      (+ (* b x)
                         c))
                   b)))

(asimp (dsimp '(dd (+ (* a (* x x))
                      (+ (* b x)
                         c))
                   c)))
