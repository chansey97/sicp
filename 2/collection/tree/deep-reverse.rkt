; SICP exercise 2.27
;
; Modify your reverse procedure in exercise 2.18 to produce a deep-reverse
; procedure that takes a list as argument and returns as its value the list
; with its elements reversed and with all sublists deep-reversed as well. For
; example,
;
; (define x (list (list 1 2) (list 3 4)))
;
; x
; ((1 2) (3 4))
;
; (reverse x)
; ((3 4) (1 2))
;
; (deep-reverse x)
; ((4 3) (2 1))

#lang racket
(provide (all-defined-out))

;; Note: this is not iterative version, it still be recursive
(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? (car items))
           (iter (cdr items) (cons (deep-reverse (car items)) result)))
          (else (iter (cdr items) (cons (car items) result)))))
  
  (iter items '()))


(module+ main
  (require rackunit rackunit/text-ui)

  (define sicp-2.27-tests
    (test-suite
     "Tests for SICP exercise 2.27"

     (check-equal? (deep-reverse '((1 2) (3 4))) '((4 3) (2 1)))
     ))

  (run-tests sicp-2.27-tests)
  )

