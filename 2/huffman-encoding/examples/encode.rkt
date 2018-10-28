; SICP exercise 2.68
;
; The encode procedure takes as arguments a message and a tree and produces the
; list of bits that gives the encoded message.
;
; (define (encode message tree)
;   (if (null? message)
;       '()
;       (append (encode-symbol (car message) tree)
;               (encode (cdr message) tree))))
;
; encode-symbol is a procedure, which you must write, that returns the lists of
; bits that encodes a given symbol according to a given tree. You should design
; encode-symbol so that it signals an error if the symbol is not in the tree at
; all. The your procedure by encoding the result you obtained in exercise 2.67
; with the sample tree and seeing whether it is the same as the original sample
; message.

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

(define sicp-2.68-tests
  (test-suite
    "Tests for SICP exercise 2.68"

    (check-equal? (encode '(A D A B B C A) sample-tree)
                  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

    (check-exn exn? (lambda () (encode '(E) sample-tree)))
))

(run-tests sicp-2.68-tests)
