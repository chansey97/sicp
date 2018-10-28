; SICP exercise 4.21
;
; Amazingly, Louis's intuition on exercise 4.20 is correct. It is indeed
; possible to specify recursive procedures without using letrec (or even
; define), although the method for accomplishing this is much more subtle than
; Louis imagined. The following expression computes 10 factorial by applying a
; recursive factorial procedure:
;
;   ((lambda (n)
;      ((lambda (fact)
;         (fact fact n))
;       (lambda (ft k)
;         (if (= k 1)
;             1
;             (* k (ft ft (- k 1)))))))
;    10)
;
; a. Check (by evaluating the expression) that this really does compute
; factorials. Devise an analogous expression for computing the Fibonacci
; numbers.
;
; b. Consider the following procedure, which includes mutually recursive
; internal definitions.
;
;   (define (f x)
;     (define (even? n)
;       (if (= n 0)
;           true
;           (odd? (- n 1))))
;     (define (odd? n)
;       (if (= n 0)
;           false
;           (even? (- n 1))))
;     (even? x))
;
; Fill in the missing expressions to complete an alternative definition of f,
; which uses neither internal definitions nor letrec:
;
;   (define (f x)
;     ((lambda (even? odd?)
;        (even? even? odd? x))
;      (lambda (ev? od? n)
;        (if (= n 0) true (od? <??> <??> <??>)))
;      (lambda (ev? od? n)
;        (if (= n 0) false (ev? <??> <??> <??>)))))

#lang racket
(require rackunit rackunit/text-ui)

; a. The check is in the tests. The similar expression is:

(define (y-fibonacci number)
  ((lambda (n)
     ((lambda (fibonacci)
        (fibonacci fibonacci n))
      (lambda (fib n)
        (cond ((= n 0) 0)
              ((= n 1) 1)
              (else (+ (fib fib (- n 1)) (fib fib (- n 2))))))))
   number))

; b. This is the alternative definition of f:

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(define sicp-4.21-tests
  (test-suite
   "Tests for SICP exercise 4.21"

   (check-equal? 3628800
                 ((lambda (n)
                    ((lambda (fact)
                       (fact fact n))
                     (lambda (ft k)
                       (if (= k 1)
                           1
                           (* k (ft ft (- k 1)))))))
                  10))

   (check-equal? (y-fibonacci 3) 2)
   (check-equal? (y-fibonacci 4) 3)
   (check-equal? (y-fibonacci 5) 5)
   (check-equal? (y-fibonacci 6) 8)
   (check-equal? (y-fibonacci 7) 13)

   (check-true (f 0))
   (check-false (f 1))
   (check-true (f 2))
   (check-false (f 3))
   (check-true (f 4))
   (check-false (f 5))
   ))

(run-tests sicp-4.21-tests)
