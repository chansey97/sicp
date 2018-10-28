; SICP exercise 2.39
;
; Complete the following definitions of reverse (exercise 2.18) in terms of
; fold-right and fold-left from exercise 2.38:
;
; (define (reverse sequence)
;   (fold-right (lambda (x y) <??>) nil sequence))
;
; (define (reverse sequence)
;   (fold-left (lambda (x y) <??>) nil sequence))

#lang racket
(require rackunit rackunit/text-ui)
(require "../list.rkt")

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x)))
              '()
              sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x))
             '()
             sequence))

(define sicp-2.39-tests
  (test-suite
    "Tests for SICP exercise 2.39"

    (check-equal? (reverse-r '(1 2 3 4 5)) '(5 4 3 2 1))
    (check-equal? (reverse-l '(1 2 3 4 5)) '(5 4 3 2 1))
))

(run-tests sicp-2.39-tests)
