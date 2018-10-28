#lang racket
(provide (all-defined-out))
(require "./stream.rkt"
         "./op-streams.rkt")

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))
(define ones-and-zeroes (stream-cons 1 (stream-cons 0 ones-and-zeroes)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
