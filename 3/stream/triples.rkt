#lang racket
(provide (all-defined-out))
(require "./stream.rkt"
         "./pairs.rkt")

;; takes three infinite streams, S, T , and U , and produces the stream of triples (Si, Tj, Uk) such that i <= j <= k.
(define (triples s t u)
  (stream-cons
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (append (list (stream-car s)) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
