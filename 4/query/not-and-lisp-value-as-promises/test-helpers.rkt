#lang racket
(provide (all-defined-out))
(require "evaluator.rkt")

(define (matches-of query)
  (let ((processed-query (query-syntax-process query)))
    (stream->list
      (stream-map
        (lambda (frame)
          (instantiate-exp processed-query frame (lambda (v f) (contract-question-mark v))))
        (qeval processed-query (singleton-stream empty-frame))))))

(define (matches? query)
  (not (null? (matches-of query))))
