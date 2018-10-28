; SICP exercise 3.64
;
; Write a procedure stream-limit that takes as arguments a stream and a number
; (the tolerance). It should examine the stream until it finds two successive
; elements that differ in absolute value by less than the tolerance, and
; return the second of the two elements. Using this, we could compute square
; roots up to a given tolerance by
;
;   (define (sqrt x tolerance)
;     (stream-limit (sqrt-stream x) tolerance))

#lang racket
(require rackunit rackunit/text-ui)
(require "../stream.rkt"
         "../stream-limit.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

;; Formulating iterations as stream processes
(define (sqrt-stream x)
  (define guesses
    (stream-cons
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (sqrt-tolerance number tolerance)
  (stream-limit (sqrt-stream number) tolerance))

;; (println (stream-take (sqrt-stream 2) 10))

;; (sqrt-tolerance 2 0.01)

(define sicp-3.64-tests
  (test-suite
    "Tests for SICP exercise 3.64"

    (check-= (sqrt-tolerance 2 0.0000000001) 1.41421356237 0.0000000001)
))

(run-tests sicp-3.64-tests)
