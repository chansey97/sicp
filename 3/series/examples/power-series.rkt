; SICP exercise 3.59
;
; Section 2.5.3 we saw how to implement a polynomial arithmetic system
; representing polynomials as lists of terms. In a similar way, we can work
; with power series, such as
;
;                  x²   x³     x⁴
;     eⁿ = 1 + x + ─ + ─── + ───── + …
;                  2   3·2   4·3·2
;
;              x²    x⁴
;   cosx = 1 - ─ + ───── - …
;              2   4·3·2
;
;               x³     x⁵
;   sinx = x - ─── + ─────── - …
;              3·2   5·4·3·2
;
; represented as infinite streams. We will represent the series a₀ + a₁x +
; a₂x² + a₃x³ + … as the stream whose elements are the coefficients a₀, a₁,
; a₂, a₃, ….
;
;
; a. The integral of the series a₀ + a₁x + a₂x² + a₃x³ + … is the series
;
;             1       1       1
;   c + a₀x + ─a₁x² + ─a₂x³ + ─a₃x⁴ + …
;             2       3       4
;
;
; where c is any constant. Define a procedure integrate-series that takes as
; input a stream a₀, a₁, a₂, … representing a power series and returns the
; stream a₀, ½a₁, ⅓a₂, … of coefficients of the non-constant terms of the
; integral series. (Since the result has no constant term, it doesn't
; represent a power series; when we use integrate-series, we will cons on the
; appropriate constant.)
;
; b. The function x ↦ eⁿ is its own derivative. This implies that eⁿ and the
; ingral of eⁿ are the same series, except for the constant term, which is
; e⁰ = 1. Accordingly, we can generate the series for eⁿ as
;
;   (define exp-series
;     (cons-stream 1 (integrate-series exp-series)))
;
; Show how to generate the series for sine and cosine starting from the facts
; that the given derivative of sine is cosine and the derivative of cosine is
; the negative of sine:
;
;   (define cosine-series
;     (cons-stream 1 <??>))
;
;   (define sine-series
;     (cons-stream 0 <??>))

; This is ridiculously easy. So easy, I'm shocked how much time it took to
; input the exercise in comparison to solving it.

#lang racket
(require rackunit rackunit/text-ui)
(require "../../stream/stream.rkt"
         "../../stream/op-streams.rkt"
         "../../stream/integers.rkt"
         ".././div-series.rkt")

(define (integrate-series stream)
  (div-streams stream integers))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (integrate-series (neg-stream sine-series))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define sicp-3.59-tests
  (test-suite
    "Tests for SICP exercise 3.59"

    (check-equal? (stream-take (integrate-series integers) 6)
                  '(1 1 1 1 1 1))

    (check-equal? (stream-take cosine-series 6)
                  (list 1 0 (/ -1 2) 0 (/ 1 24) 0))
    (check-equal? (stream-take sine-series 6)
                  (list 0 1 0 (/ -1 6) 0 (/ 1 120)))
))

(run-tests sicp-3.59-tests)
