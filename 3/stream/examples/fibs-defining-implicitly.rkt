#lang racket
(require "../stream.rkt"
         "../op-streams.rkt")

;; Defining streams implicitly
(define fibs
  (stream-cons
   0
   (stream-cons 1 (add-streams (stream-cdr fibs) fibs))))

(stream-take fibs 10)
