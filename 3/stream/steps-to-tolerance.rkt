#lang racket
(provide (all-defined-out))
(require "./stream.rkt")

(define (steps-to-tolerance stream tolerance)
  (define (count stream n)
    (let ((s1 (stream-car stream))
          (s2 (stream-car (stream-cdr stream))))
      (if (< (abs (- s1 s2))
             tolerance)
          n
          (count (stream-cdr stream) (+ n 1)))))
  (count stream 1))
