#lang racket
(provide (all-defined-out))
(require "./stream.rkt")
(require "./stream-map-2.rkt")

;; combines two ordered streams into one ordered result stream, eliminating repetitions:
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-cdr s2))))
                 (else
                  (stream-cons s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))
