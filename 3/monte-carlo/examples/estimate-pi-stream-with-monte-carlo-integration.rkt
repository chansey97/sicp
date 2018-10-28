; SICP exercise 3.82
;
; Redo exercise 3.5 on Monte Carlo integration in terms of streams. The stream
; version of estimate-integral will not have an argument telling how many
; trials to perform. Instead, it will produce a stream of estimates based on
; succesively more trials.

#lang racket
(require rackunit rackunit/text-ui)
(require "../../random-numbers/random.rkt"
         "../../monte-carlo/monte-carlo-with-stream.rkt"
         "../../stream/stream.rkt")

(define random-init (modulo (current-milliseconds) random-modulus))

(define random-integers
  (stream-cons random-init
               (stream-map rand-update random-integers)))

(define random-floats
  (stream-map (lambda (x) (exact->inexact (/ x random-modulus))) random-integers))

(define (map-successive-pairs f s)
  (stream-cons
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (estimate-integral predicate x1 x2 y1 y2)
  (define (scale-within value low high)
    (+ (* value (- high low)) low))
  (define experiment-stream
    (map-successive-pairs
     (lambda (a b)
       (predicate (scale-within a x1 x2)
                  (scale-within b y1 y2)))
     random-floats))
  (monte-carlo experiment-stream 0 0))

(define (estimate-pi tries)
  (define (circle x y)
    (<= (+ (* x x) (* y y)) 1))
  (* (stream-ref (estimate-integral circle -1 1 -1 1) tries)
     4.0))

;; (estimate-pi 100000)

(define sicp-3.82-tests
  (test-suite
    "Tests for SICP exercise 3.82"

    (check-= (estimate-pi 100000) 3.14 0.01)
))

(run-tests sicp-3.82-tests)

