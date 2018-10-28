#lang racket
(provide (all-defined-out))
(require "../../simulator/basic/simulator.rkt")
(require "controller-text.rkt")
(require "operations.rkt")

(define (make-explicit-control-machine)
  (make-machine ec-registers ec-operations ec-controller-text))

(define (make-explicit-control-repl-machine)
  (make-machine ec-registers ec-operations ec-repl-controller-text))
