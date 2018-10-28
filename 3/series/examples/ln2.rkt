; SICP exercise 3.65
;
; Use the series
;
;             1   1   1
;   ln2 = 1 - ─ + ─ - ─ + …
;             2   3   4
;
; to compute three sequences of approximations of the natural logarithm of 2,
; in the same way we did above for π. How rapidly do these sequences converge?

; The definitions are at the end of file. You can run it in order to see how
; many steps it takes to converge on a specific tolerance with all the
; sequences. This is the result from running it:
;
;   ln2-stream takes 9999 steps to tolerance 0.0001
;   ln2-stream-euler takes 12 steps to tolerance 0.0001
;   ln2-stream-accelarated takes 4 steps to tolerance 0.0001

#lang racket
(require "../../stream/stream.rkt"
         "../../stream/op-streams.rkt"
         "../../stream/steps-to-tolerance.rkt"
         "../partial-sums.rkt"
         "../acceleration.rkt")

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (neg-stream (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))
(define ln2-stream-euler (euler-transform ln2-stream))
(define ln2-stream-accelarated (accelerated-sequence euler-transform ln2-stream))
;; (define tolerance 0.0001)
(define tolerance 0.001)

(printf "ln2-stream takes ~s steps to tolerance ~s\n"
        (steps-to-tolerance ln2-stream tolerance)
        tolerance)
(printf "ln2-stream-euler takes ~s steps to tolerance ~s\n"
        (steps-to-tolerance ln2-stream-euler tolerance)
        tolerance)
(printf "ln2-stream-accelarated takes ~s steps to tolerance ~s\n"
        (steps-to-tolerance ln2-stream-accelarated tolerance)
        tolerance)
