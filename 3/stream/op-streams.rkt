#lang racket
(provide (all-defined-out))
(require "./stream.rkt"
         "./stream-map-2.rkt")

(define (neg-stream a) (stream-map - a))
(define (add-streams a b) (stream-map2 + a b))
(define (mul-streams a b) (stream-map2 * a b))
(define (div-streams a b) (stream-map2 / a b))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define (alternate-signs stream)
  (stream-cons (stream-car stream)
               (neg-stream (alternate-signs (stream-cdr stream)))))
