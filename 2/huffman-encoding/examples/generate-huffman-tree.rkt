; SICP exercise 2.69
;
; The following procedure as its arguments a list of symbol-frequency pairs
; (where no symbol appears in more than one pair) and generates a Huffman
; encoding tree according to the Huffman algorithm.
;
;   (define (generate-huffman-tree pairs)
;     (successive-merge (make-leaf-set pairs)))
;
; make-leaf-set is the procedure given above that transforms the list of pairs
; into an ordered set of leaves. successive-merge is the procedure you must
; write, using make-code-tree to successively merge the smallest-weight
; elements of the set until there is only one element left, which is the
; desired Huffman tree. (This procedure is slightly tricky; but not really
; complicated. If you find yourself designing a complex procedure, then you
; are almost certainly doing something wrong. You can take significant
; advantage of the fact that we are using ordered set representation).

#lang racket
(require rackunit rackunit/text-ui)
(require "../huffman-encoding.rkt")

(define sicp-2.69-tests
  (test-suite
    "Tests for SICP exercise 2.69"

    (check-equal?
      (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
      (make-code-tree (make-leaf 'A 4)
                      (make-code-tree
                        (make-leaf 'B 2)
                        (make-code-tree (make-leaf 'D 1)
                                        (make-leaf 'C 1)))))
))

(run-tests sicp-2.69-tests)
