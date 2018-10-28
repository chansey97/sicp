; SICP exercise 1.46
;
; Several of the numerical methods described in this chapter are instances of
; an extremely general computational strategy known as iterative improvement.
; Iterative improvement says that, to compute something, we start with an
; initial guess for the answer, test if the guess is good enough, and otherwise
; improve the guess and continue the process using the improved guess as the
; new guess. Write a procedure iterative-improve that takes two procedures as
; arguments: a method for telling whether a guess is good enough and a method
; for improving a guess. iterative-improve should return as its value a
; procedure that takes a guess as argument and keeps improving the guess until
; it is good enough. Rwerite the sqrt procedure of section 1.1.7 and the
; fixed-point procedure of section 1.3.3 in terms of iterative-improve.

#lang racket
(provide iterative-improve)

;; iterative-improve return a procedure that takes a guess as argument,
;; when call this procudure it will keep improving the guess until it is good enough.
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(module+ main
  (require rackunit rackunit/text-ui)

  (define (sqrt x)
    ((iterative-improve (lambda (guess) (< (abs (- (* guess guess) x))
                                           0.000001))
                        (lambda (guess) (/ (+ guess (/ x guess)) 2)))
     1.0))

  (define (fixed-point f first-guess)
    ((iterative-improve (lambda (guess) (< (abs (- guess (f guess)))
                                           0.000001))
                        f)
     first-guess))
  
  (define sicp-1.46-tests
    (test-suite
     "Tests for SICP exercise 1.46"

     (check-= (sqrt 9) 3 0.0000001)
     (check-= (sqrt 256) 16 0.0000001)

     (check-= (fixed-point cos 1.0) 0.739084 0.00001)
     ))

  (run-tests sicp-1.46-tests)
  )
