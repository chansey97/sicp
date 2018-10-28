; SICP exercise 3.50
;
; Complete the following definition, which generalizes stream-map to allow
; procedures that take multiple arguments analogous to map in section 2.2.1,
; footnote 12.
;
;   (define (stream-map proc . argstreams)
;     (if (<??> (car argstreams))
;         the-empty-stream
;         (<??>
;          (apply proc (map <??> argstreams))
;          (apply stream-map
;                 (cons proc (map <??> argstreams))))))

#lang racket
(provide (all-defined-out))
(require "./stream.rkt")

;; Lisp support var-argus in lambda, in racket we can use for/list with multiple [id sequence-expr] clauses
(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-3.50-tests
    (test-suite
     "Tests for SICP exercise 3.50"

     (check-equal?
      (stream->list (stream-map2 +
                                 (stream-cons 1 (stream-cons 2 (stream-cons 3 the-empty-stream)))
                                 (stream-cons 4 (stream-cons 5 (stream-cons 6 the-empty-stream)))))
      '(5 7 9))

     (check-equal?
      (stream->list (stream-map2 +
                                 (stream-cons 1 (stream-cons 2 (stream-cons 3 the-empty-stream)))
                                 (stream-cons 4 (stream-cons 5 (stream-cons 6 the-empty-stream)))
                                 (stream-cons 7 (stream-cons 8 (stream-cons 9 the-empty-stream)))))
      '(12 15 18))
     ))

  (run-tests sicp-3.50-tests)
  )
