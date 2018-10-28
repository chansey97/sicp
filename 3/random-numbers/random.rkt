#lang racket
(provide (all-defined-out))
;; https://rosettacode.org/wiki/Linear_congruential_generator

(define random-modulus 4294967296)
(define random-multiplier 1664525)
(define random-increment 1013904223)

(define (rand-update number)
  (let ((modulus random-modulus)
        (multiplier random-multiplier)
        (increment random-increment))
    (modulo (+ (* multiplier number) increment) modulus)))
