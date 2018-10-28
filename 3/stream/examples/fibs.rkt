#lang racket
(require "../stream.rkt"
         "../op-streams.rkt")

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(stream-take fibs 10)
