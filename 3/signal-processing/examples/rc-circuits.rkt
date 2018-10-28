; SICP exercise 3.73
;
; We can model electrical circuits using streams to represent the values of
; currents or voltages at a sequence of times. For instance, suppose we have
; an RC circuit consisting of a resistor of resistance R and a capacitor C in
; series. The voltage response v of the circuit to an injected current i is
; determined by the formula in figure 3.33, whose structure is shown by the
; accomplanying signal-flow diagram:
;
;   [figure 3.33]
;
; Write a procedure RC that models this circuit. RC should take as inputs the
; values of R, C, and dt and should return a procedure that takes as inputs a
; stream representing the current i and an initial value for the capacitor
; voltage v₀ and produces as output the stream of voltages v. For example, you
; should be able to use RC to model an RC circuit with R = 5 ohms, C = 1
; farad, and 0.5-second time step by evaluating (define RC1 (RC 5 1 0.5)).
; This defines RC1 as a procedure that takes a stream representing the time
; sequence of currents and an initial capacitor voltage and produces the
; output stream of voltages.

; Man, this brings me a long time back. Also, I have no idea what I'm doing.

#lang racket
(require rackunit rackunit/text-ui)
(require "../../stream/stream.rkt"
         "../../stream/op-streams.rkt"
         "../../stream/integers.rkt"
         "../signal-processing.rkt")

(define (RC resistance capacity dt)
  (define (result stream initial-voltage)
    (add-streams (scale-stream stream resistance)
                 (integral (scale-stream stream (/ 1 capacity)) initial-voltage dt)))
  result)

; RC1 is a procedure which input stream i and v0, output stream v
(define RC1 (RC 5 1 0.5))

(define sicp-3.73-tests
  (test-suite
   "Tests for SICP exercise 3.73"

   (check-equal? (stream-take (RC1 ones-and-zeroes 0) 10)
                 '(5 0.5 5.5 1.0 6.0 1.5 6.5 2.0 7.0 2.5))
   ))

(run-tests sicp-3.73-tests)


