#lang racket
(require rackunit rackunit/text-ui)
(require "../constraint-network.rkt")

(define X (make-connector))
(define Y (make-connector))
(probe "Squarer" X)
(probe "Squarer" Y)
(squarer X Y)
;; (set-value! X 10 'user)
(set-value! Y 100 'user)
