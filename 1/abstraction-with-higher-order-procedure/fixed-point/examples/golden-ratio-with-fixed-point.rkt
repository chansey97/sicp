; SICP exercise 1.35
;
; Show that the golden ratio 𝜙 (section 1.2.2) is a fixed point of the
; transformation x |-> 1 + 1/x, and use this fact to compute 𝜙 by means of the
; fixed-point procedure.

; Showing that 𝜙 is a fixed point is trivial. We have that:
;
; 𝜙 = (1 + √5)/2
;
; When we apply the transformation, we get
;
;     1   𝜙 + 1   (1 + √5)/2 + 2/2   3 + √5   (3 + √5)(1 - √5)
; 1 + ─ = ───── = ──────────────── = ────── = ──────────────── =
;     𝜙     𝜙        (1 + √5)/2      1 + √5   (1 + √5)(1 - √5)
;
;   3 - 3√5 + √5 - 5   -2 - 2√5   1 + √5
; = ──────────────── = ──────── = ────── = 𝜙
;          -4             -4        2
;
; Clearly, it is a fixed point.
;
; As for computing it:

#lang racket
(require rackunit rackunit/text-ui)
(require "../fixed-point.rkt")

(define tolerance 0.00001)

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define sicp-1.35-tests
  (test-suite
    "Tests for SICP exercise 1.35"

    (check-= (* golden-ratio golden-ratio) (+ 1 golden-ratio) 0.00001)
))

(run-tests sicp-1.35-tests)

;; (display golden-ratio)
