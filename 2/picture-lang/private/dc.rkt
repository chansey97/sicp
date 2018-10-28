#lang racket
(provide (all-defined-out))

(define dc '())

(define (dc-initialize dc0)
  (set! dc dc0))

(define (dc-finalize)
  (set! dc '()))
