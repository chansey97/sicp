; SICP exercise 2.67
;
; Define an encoding tree and a sample message:
;
; (define sample-tree
;   (make-code-tree (make-leaf 'A 4)
;                   (make-code-tree
;                     (make-leaf 'B 2)
;                     (make-code-tree (make-leaf 'D 1)
;                                     (make-leaf 'C 1)))))
;
; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;
; Use the decode procedure to decode the message, and give the result.

#lang racket
(require rackunit rackunit/text-ui)
(require "../huffman-encoding.rkt")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sicp-2.67-tests
  (test-suite
    "Tests for SICP exercise 2.67"

    (check-equal? (decode sample-message sample-tree)
                  '(A D A B B C A))
))

(run-tests sicp-2.67-tests)
