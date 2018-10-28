#lang racket
(require rackunit rackunit/text-ui)
(require "./generic-operations.rkt")

; Now, let's introduce our types:

(define (make-a) (attach-tag 'a "a"))
(define (make-b) (attach-tag 'b "b"))
(define (make-c) (attach-tag 'c "c"))

; Not some coercion operations:

(put-coercion 'a 'b (lambda (x) (attach-tag 'b (string-append (contents x) "->" "b"))))

; Our operations would be nonsensical - foo, bar and baz. 

(define (foo x y) (apply-generic 'foo x y))
(define (bar x y z) (apply-generic 'bar x y z))
(define (baz w x y z) (apply-generic 'baz w x y z))

(put 'foo '(a a) (lambda args (cons 'foo-a-a (map string->symbol args))))
(put 'foo '(b b) (lambda args (cons 'foo-b-b (map string->symbol args))))
(put 'bar '(a a a) (lambda args (cons 'bar-a-a-a (map string->symbol args))))
(put 'bar '(b b b) (lambda args (cons 'bar-b-b-b (map string->symbol args))))
(put 'baz '(a a a a) (lambda args (cons 'baz-a-a-a-a (map string->symbol args))))
(put 'baz '(b b b b) (lambda args (cons 'baz-b-b-b-b (map string->symbol args))))

; As for when this not sufficiently general - consider having foo defined for
; '(c c), but passing 'a and 'b - coercing just to b won't be enough. We need
; to coerce both arguments to c, but our apply-generic has no way of knowing
; that. Another example is if (foo b c) is defined and we call it with a and
; c. apply-generic will attempt to convert either to a or to c, but foo would
; not be defined for both.


(define sicp-2.82-tests
  (test-suite
    "Tests for SICP exercise 2.82"

    (check-equal? (foo (make-a) (make-a)) '(foo-a-a a a))
    (check-equal? (foo (make-a) (make-b)) '(foo-b-b a->b b))
    (check-equal? (foo (make-b) (make-a)) '(foo-b-b b a->b))

    (check-equal? (bar (make-a) (make-a) (make-a)) '(bar-a-a-a a a a))
    (check-equal? (bar (make-a) (make-b) (make-b)) '(bar-b-b-b a->b b b))
    (check-equal? (bar (make-b) (make-a) (make-b)) '(bar-b-b-b b a->b b))
    (check-equal? (bar (make-b) (make-b) (make-a)) '(bar-b-b-b b b a->b))
    (check-equal? (bar (make-a) (make-a) (make-b)) '(bar-b-b-b a->b a->b b))
    (check-equal? (bar (make-a) (make-b) (make-a)) '(bar-b-b-b a->b b a->b))
    (check-equal? (bar (make-b) (make-a) (make-a)) '(bar-b-b-b b a->b a->b))

    (check-equal? (baz (make-a) (make-a) (make-a) (make-a)) '(baz-a-a-a-a a a a a))
    (check-equal? (baz (make-a) (make-b) (make-b) (make-b)) '(baz-b-b-b-b a->b b b b))
    (check-equal? (baz (make-b) (make-a) (make-b) (make-b)) '(baz-b-b-b-b b a->b b b))
    (check-equal? (baz (make-b) (make-b) (make-a) (make-b)) '(baz-b-b-b-b b b a->b b))
    (check-equal? (baz (make-b) (make-b) (make-b) (make-a)) '(baz-b-b-b-b b b b a->b))

    (check-exn exn? (lambda () (foo (make-a) (make-c))))
))

(run-tests sicp-2.82-tests)
