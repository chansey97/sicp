; SICP exercise 1.42
;
; Let f and g be two one-argument functions. The composition f after g is defined to be
; the function x ↦ f(g(x)). Define a procedure compose that implements composition. For
; example, if inc is a procedure that adds 1 to its argument,
;
; ((compose square inc) 6)
; 49

#lang racket
(provide (all-defined-out))

(define (compose f g)
  (lambda (x) (f (g x))))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define (square x)
    (* x x))

  (define (inc x)
    (+ x 1))

  (define sicp-1.42-tests
    (test-suite
     "Tests for SICP exercise 1.42"

     (check-equal? ((compose square inc) 6) 49)
     ))

  (run-tests sicp-1.42-tests)
  )
