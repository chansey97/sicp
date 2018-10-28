; SICP exercise 1.27
;
; Demonstrate that the Carmichael numbers listed in footnote 1.17 really do
; fool the Fermat test. That is, write a procedure that takes an integer n and
; tests whether aⁿ is congruent to a modulo n for every a < n, and try your
; procedure on the given Carmichael numbers.

#lang racket
(provide fast-prime? carmichael?)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square n)
  (* n n))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Just use the function carmichael?, although do note, that it should be named
; carmichael-or-prime?

(define (carmichael? number)
  (define (congruent-to-number-below a)
    (cond ((= a 1) #t)
          ((= (expmod a number number) (remainder a number))
           (congruent-to-number-below (- a 1)))
          (else #f)))

  (congruent-to-number-below (- number 1)))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-1.27-tests
    (test-suite
     "Tests for SICP exercise 1.27"

     (check-true (carmichael? 561))
     (check-true (carmichael? 1105))
     (check-true (carmichael? 1729))
     (check-true (carmichael? 2465))
     (check-true (carmichael? 2821))
     (check-true (carmichael? 6601))

     (check-false (carmichael? 27))
     (check-false (carmichael? 1001))
     ))

  (run-tests sicp-1.27-tests)
  )

