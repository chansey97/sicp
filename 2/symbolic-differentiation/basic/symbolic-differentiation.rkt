#lang racket
(provide deriv)

;; Differentiation of any such expression can be carried out by applying the following reduction rules:

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

;; In derivation, the right hand side is a proper sub expression of the left hand side.

;; Conversely, producing integrals is difficult because multiple rules may match a given pattern and they don't easily terminate.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (=number? expr num)
  (and (number? expr)
       (= expr num)))
