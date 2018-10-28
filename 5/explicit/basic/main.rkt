#lang racket
(require "../../simulator/basic/simulator.rkt")
(require "evaluator.rkt")

(define ec-repl-machine (make-explicit-control-repl-machine))
(start ec-repl-machine)
