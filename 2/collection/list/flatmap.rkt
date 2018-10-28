#lang racket
(provide (all-defined-out))
(require "./accumulate.rkt")

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
