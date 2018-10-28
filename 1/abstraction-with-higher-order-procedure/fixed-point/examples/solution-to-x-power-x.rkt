; SICP exercise 1.36
;
; Modify the fixed-point so that it prints the sequence of approximations it
; generates, using the newline and display primitives shown in exercise 1.22.
; Then find a solution to xˣ = 1000 by finding a fixed point of
; x ↦ log(1000)/log(x). (Use Scheme's primitive log procedure, which computes
; natural logarithms.) Compare the number of step this takes with and without
; average damping. (Note that you cannot start fixed-point with a guess of 1,
; as this ould cause divisino by log(1) = 0.)

; Curiously enough, the version with average damping takes significantly less
; steps. I expected it to be the other way around. I believe the reason is that
; without average damping, the guesses oscillate around the answer, while with
; it, the guesses approaches steadily from one direction.

#lang racket
(require "../fixed-point.rkt")

(define (average x y)
  (/ (+ x y) 2))

(display "Without average damping:")
(newline)
(fixed-point-print-sequence (lambda (x) (/ (log 1000) (log x))) 2.0)

(newline)
(display "With average damping:")
(newline)
(fixed-point-print-sequence (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
