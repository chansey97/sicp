#lang racket
(provide (all-defined-out))
(require "../stream/stream.rkt")

;; euler acceleration
(define (euler-transform s)
  (define (square x)
    (* x x))
  (let ((s0 (stream-ref s 0)) ; Sn-1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; super-acceleration
(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
