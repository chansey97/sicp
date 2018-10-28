#lang racket
(provide (all-defined-out))
(require "./compiler.rkt")

(define (compiled-instructions expression)
  (statements (compile-exp expression 'val 'next)))
