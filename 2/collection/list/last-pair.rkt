; SICP exercise 2.17
;
; Define a procedure last-pair that returns the list that contains only the last
; element of a given (nonempty) list:
;
; (list-pair (list 23 72 149 34))
; (34)

#lang racket
(provide (all-defined-out))

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(module+ main
  (require rackunit rackunit/text-ui)

  (define sicp-2.17-tests
    (test-suite
     "Tests for SICP exercise 2.17"

     (check-equal? (last-pair (list 23 72 149 34)) (list 34))
     (check-equal? (last-pair (list 34)) (list 34))
     ))

  (run-tests sicp-2.17-tests)
  )

