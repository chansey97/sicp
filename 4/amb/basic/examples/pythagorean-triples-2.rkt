; SICP exercise 4.36
;
; Exercise 3.69 discussed how to generate the stream of all Pythagorean
; triples, with no upper bound on the size of the integers to be searched.
; Explain why simply replacing an-integer-between by an-integer-starting-from
; in the procedure in exercise 4.35 is not an adequate way to generate
; arbitrary Pythagorean triples. Write a procedure that actually will
; accomplish this. (That is, write a procedure for which repeatedly typing
; try-again would in principle eventually generate all Pythagorean triples).

#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")

; Say we do said replacing. On the first backtrack, the innermost call to
; an-integer-starting-from will result to 2. On the second backtrack, it will
; be 3 and so on, never giving a chance to the previous calls to return
; anything different than the initial value.

; The solution follows. There is some support code for the tests in the end of
; the file.

(define solution
  '((define (an-integer-starting-from n)
      (amb n (an-integer-starting-from (+ n 1))))
    (define (an-integer-between low high)
      (if (> low high)
          (amb)
          (amb low (an-integer-between (+ low 1) high))))
    (define (a-pythagorean-triple)
      (let ((k (an-integer-starting-from 1)))
        (let ((i (an-integer-between 1 k)))
          (let ((j (an-integer-between i k)))
            (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k)))))))

(define (first-n-values n exp)
  (define (take n results)
    (if (= n 0)
        '()
        (cons (car results)
              (take (- n 1) (force (cdr results))))))
  (take n
        (ambeval exp
                 solution-environment
                 (lambda (value fail) (cons value (delay (fail))))
                 (lambda () '()))))

(define solution-environment
  ((lambda ()
     (define environment (setup-environment))
     (for-each (lambda (definition)
                 (ambeval definition
                          environment
                          (lambda (value fail) 'ok)
                          (lambda () 'ok)))
               solution)
     environment)))

(define sicp-4.36-tests
  (test-suite
   "Tests for SICP exercise 4.36"

   (check-equal? (first-n-values 5 '(a-pythagorean-triple))
                 '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17)))

   ))

(run-tests sicp-4.36-tests)
