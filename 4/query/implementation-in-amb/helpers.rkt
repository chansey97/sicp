#lang racket
(provide (all-defined-out))
(require (planet murphy/amb:1:1/amb))
(require "evaluator.rkt")

(define (add-to-data-base! assertions)
  (for-each (compose add-rule-or-assertion! query-syntax-process)
            assertions))

(define (matches-of query)
  (let ((processed-query (query-syntax-process query)))
    (map (lambda (frame)
           (instantiate-exp processed-query frame (lambda (v f) (contract-question-mark v))))
        (amb-collect (qeval processed-query '())))))

(define (matches? query)
  (not (null? (matches-of query))))
