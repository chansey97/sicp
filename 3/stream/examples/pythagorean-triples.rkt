; SICP exercise 3.69
;
; Write a procedure triples that takes three infinite streams, S, T and U and
; produces the stream of triples (Sᵢ, Tᵣ, Uᵥ) such that i ≤ r ≤ v. Use triples
; to generate the stream of all Pythagorean triples of positive integers, i.e.
; the triples (i, j, k) such that i ≤ j and i² + j² = k².

#lang racket
(require rackunit rackunit/text-ui)
(require "../stream.rkt"
         "../integers.rkt"
         "../triples.rkt")

(define int-triples (triples integers integers integers))
;; (println (stream-take int-triples 100))

(define (square x) (* x x))

(define (pythagorean? triple)
  (let ((a (car triple))
        (b (cadr triple))
        (c (caddr triple)))
    (= (+ (square a) (square b))
       (square c))))

(define pythagorean-triples (stream-filter pythagorean? int-triples))

;; very few, so don't use large value (> 5) in stream-take
(println (stream-take pythagorean-triples 5))

(define sicp-3.69-tests
  (test-suite
    "Tests for SICP exercise 3.69"

    (check-equal? (stream-take pythagorean-triples 5)
                  '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17)))
))

(run-tests sicp-3.69-tests)
