#lang racket
(require "../stream.rkt")

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(stream-take integers 10)
