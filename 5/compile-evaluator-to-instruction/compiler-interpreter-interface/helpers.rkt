#lang racket
(provide (all-defined-out))
(require "../simulator/simulator.rkt")
(require "../compiler/compiler.rkt")
(require "../compiler/operations.rkt")

(define (compile-in-machine machine expression)
  (let ((instructions (assemble (statements (compile-exp expression 'val 'return))
                                machine)))
    (set-register-contents! machine 'env the-global-environment)
    (set-register-contents! machine 'val instructions)
    (set-register-contents! machine 'flag true)
    (start machine)))
