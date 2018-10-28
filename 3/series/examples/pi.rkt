#lang racket
(require "../../stream/stream.rkt"
         "../../stream/op-streams.rkt"
         "../partial-sums.rkt"
         "../acceleration.rkt")

; π       2   2   1 
; ─ = 1 - ─ + ─ - ─ + ...
; 4       3   5   7

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(println (stream-ref pi-stream 100))

;; we can transform a stream with a sequence accelerator
;; that converts a sequence of approximations to a new sequence that converges to the same value as the original,
;; only faster. 
(println (stream-ref (euler-transform pi-stream) 20))

;; Even better, we can accelerate the accelerated sequence, and recursively
;; accelerate that, and so on.
(println (stream-ref (accelerated-sequence euler-transform pi-stream) 8))

