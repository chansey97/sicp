#lang racket
(provide (all-defined-out))
(require "./stream.rkt")

(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
      (stream-ref stream 1)
      (stream-limit (stream-cdr stream) tolerance)))

;; (define (stream-limit stream tolerance)
;;   (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
;;       (stream-ref stream 1)
;;       (stream-limit (stream-cdr stream) tolerance)))
