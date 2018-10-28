#lang racket
(provide (all-defined-out))

;; start end are vector
(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))
