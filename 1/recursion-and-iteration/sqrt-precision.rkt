; SICP exercise 1.07
;
; The good-enough? test used in computing square roots will not be very
; effective for finding the square roots of very small numbers. Also, in real
; computers, arithmetic operations are almost always performed with limited
; precision. This makes our test inadequate for very large numbers. Explain
; these statements, with examples showing how the tests fails for small and
; large numbers. An alternative strategy for implementing good-enough? is to
; watch how guess changes from one iteration to the next and to stop when the
; change is a very small fraction of the guess. Design a square-root procedure
; that uses this kind of end test. Does this work better for small and large
; numbers?

; It is very easy to demonstrate how this version of good-enough? fails with
; small numbers. Let's try finding the squre of 0.00000004. The result we
; expect is 0.0002.
;
; On the third iteration the guess will be 0.0312. The square of the guess is
; 0.0009, which is just below 0.001. Thus good-enough concludes that 0.0312 is
; the correct answer, if if it is two orders of magnitude apart from the
; desired answer, 0.0002.
;
; Simply put, when the number we are square rooting is below the precision,
; there error is quite big.
;
; As for large numbers, we can demonstrate it by attempting to find the square
; root of 10e+48. On my machine, this simply gets stuck in an infinite loop.
;

#lang racket
(require rackunit rackunit/text-ui)

;; The current good-enough? stops as soon as the difference between the
;; square of the guess and the actual number is less than 0.001.
;; For extremely small or large numbers; a close non-accurate guess will pass this test
;; even though it is not the accurate value.
;; Also this is the reason why (sqrt 9) is not an absolute 3.0 value.
(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? prevGuess nextGuess)
  (<  (/ (abs (- prevGuess nextGuess)) prevGuess)
      1.0e-20))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9) ; absolute 3 value

(sqrt 0.00000004)

(sqrt 10e+48)

(define sicp-1.07-tests
  (test-suite
    "Tests for SICP exercise 1.07"

    (check-= (sqrt 4e-8) 2e-4 1e-16)
    (check-= (* (sqrt 10e+48) (sqrt 10e+48)) 10e+48 10e+33)
))

(run-tests sicp-1.07-tests)
