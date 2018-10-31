; dc
; ── = 0 for c a constant or a variable different from x
; dx

; dx
; ── = 1
; dx

; d(u+v)   du   dv
; ────── = ── + ──
;   dx     dx   dx

; d(uv)     dv    du
; ───── = u ── +v ──
;   dx      dx    dx

; d(uⁿ)         du
; ───── = nuⁿ⁻¹ ── 
;   dx          dx

;; Compare with "../../../../2/symbolic-differentiation/basic"

#lang racket
(require "../evaluator.rkt")
(require "../helpers.rkt")

(define-namespace-anchor a)
(ns-initialize (namespace-anchor->namespace a))

(define (atom? exp)
  (not (pair? exp)))

(define deriv-rules
  '(
    (rule (same ?x ?x))

    (rule (deriv ?c ?v 0)
          (and (lisp-value atom? ?c) (not (same ?c ?v))))

    (rule (deriv ?v ?v 1)
          (and (lisp-value atom? ?v) (lisp-value symbol? ?v)))

    (rule (deriv (+ ?x1 ?x2) ?v (+ ?y1 ?y2))
          (and (deriv ?x1 ?v ?y1)
               (deriv ?x2 ?v ?y2)))

    (rule (deriv (* ?x1 ?x2) ?v (+ (* ?y1 ?y2) (* ?y3 ?y4)))
          (and (same ?x1 ?y1)
               (same ?x2 ?y3)
               (deriv ?x2 ?v ?y2)
               (deriv ?x1 ?v ?y4)))
    ))

(add-to-data-base! deriv-rules)

(matches-of '(deriv x x ?y))
(matches-of '(deriv 1 x ?y))

(matches-of '(deriv (+ x 3) x ?y))
(matches-of '(deriv (* x y) x ?y))
(matches-of '(deriv (* (* x y) (+ x 3)) x ?y))

(matches-of '(deriv (+ (* a (* x x))
                       (+ (* b x)
                          c)) x ?y))

(matches-of '(deriv (+ (* a (* x x))
                       (+ (* b x)
                          c)) a ?y))

(matches-of '(deriv (+ (* a (* x x))
                       (+ (* b x)
                          c)) b ?y))

(matches-of '(deriv (+ (* a (* x x))
                       (+ (* b x)
                          c)) c ?y))

;; (matches-of '(deriv ?e x 0))
;; Unknown pat var -- LISP-VALUE (? 1 c)
;; ?e is not bounded, so can not do lisp-value
;; So can't solve integral
;; TODO: maybe write new rules for integral?

;; It can't solve simplify problem as well, since no number encoding representation and can't do add mul.
;; TODO: maybe implement number encoding representation?
