; SICP exercise 1.31
;
; a. The sum procedure is only the simplest of a vast number of similar
; abstractions that can be captured as higher-order procedures. Write an
; analogous procedure called product that returns the product of the values of
; a function at points over a given range. Show how to define factorial in
; terms of product. Also use product to compute approximations of π using the
; formula:
;
; π   2·4·4·6·6·8…
; ─ = ────────────
; 4   3·3·5·5·7·7…
;
; b. If your product procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write
; one that generates a recursive process.

#lang racket
(require "../product.rkt")

(define (approximated-pi precision)
  (define (term n)
    (/ (* 2 (quotient (+ n 2) 2))
       (+ 1 (* 2 (quotient (+ n 1) 2)))))
  (define (next n)
    (+ n 1))
  (product term 1.0 next precision))

(approximated-pi 1000)
(/ 3.14 4)
