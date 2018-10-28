#lang racket
(require "../stream.rkt"
         "../op-streams.rkt"
         "../integers.rkt")

(define factorials (stream-cons 1 (mul-streams factorials (stream-cdr integers))))
(stream-take factorials 10)
