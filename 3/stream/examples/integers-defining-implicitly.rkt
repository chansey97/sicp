#lang racket
(require "../stream.rkt"
         "../op-streams.rkt")

;; Defining streams implicitly
(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

(stream-take integers 10)
