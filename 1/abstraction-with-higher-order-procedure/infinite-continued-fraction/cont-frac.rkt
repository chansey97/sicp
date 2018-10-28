
; SICP exercise 1.37
;
; a. An infinite continued fraction is an expression of the form:
;
;            N₁
; f = ────────────────
;              N₂
;     D₁ + ───────────
;                 N₃
;          D₂ + ──────
;               D₃ + …
;
; As an example, one can show that the infinite continued fraction expansion
; with the Nᵢ and the Dᵢ all equal to 1 produces 1/𝜙, where 𝜙 is the golden
; ratio (described in section 1.2.2). One way to approximate an infinite
; continued fraction is to truncate the expansion after a given number of
; terms. Such a truncation — a so called k-term finite continued fraction — has
; the form
;
;     N₁
; ──────────
;        N₂
; D₁ + ─────
;     ⋱   Nᵢ
;       + ──
;         Dᵢ
;
; Suppose that n and d are procedures of one argument (the term index i) that
; return the Nᵢ and the Dᵢ of the terms of the continued fraction. Defined a
; procedure cont-frac such that evaluating (cont-frac n d k) computes the value
; of the k-term finite continued fraction. Check your procedure by
; approximating 1/𝜙 using
;
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)
;
; for successive values of k. How large must you make k in order to get an
; approximation that is accurate to 4 decimal places?
;
; b. If your cont-frac procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.

; k must be 11 in order to get an approximation, accurate to 4 decimal places
;
; Here are the functions:

#lang racket
(provide (all-defined-out))

(define (cont-frac n d k)
  (define (frac i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (frac (+ i 1))))))
  (frac 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter k 0))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define one (lambda (i) 1.0))
  (define two (lambda (i) 2.0))

  (define sicp-1.37-tests
    (test-suite
     "Tests for SICP exercise 1.37"

     (check-equal? (cont-frac one one 1) 1.0)
     (check-equal? (cont-frac one two 1) 0.5)
     (check-equal? (cont-frac two one 1) 2.0)
     (check-= (cont-frac one one 11) 0.6180 0.00006)

     (check-equal? (cont-frac-iter one one 1) 1.0)
     (check-equal? (cont-frac-iter one two 1) 0.5)
     (check-equal? (cont-frac-iter two one 1) 2.0)
     (check-= (cont-frac-iter one one 11) 0.6180 0.00006)
     ))

  (run-tests sicp-1.37-tests)

  )
