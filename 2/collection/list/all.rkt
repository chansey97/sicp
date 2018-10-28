#lang racket
(provide (all-defined-out))

(define (all? proc seq)
  (cond ((null? seq) #t)
        ((proc (car seq)) (all? proc (cdr seq)))
        (else #f)))
