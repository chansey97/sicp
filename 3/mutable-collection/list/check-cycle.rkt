; SICP exercise 3.18
;
; Write a procedure that examines a list and determines whether it contains a
; cycle, that is, whether a program that tried to find the end of the list by
; taking successive cdrs would go into an infinite loop. Exercise 3.13
; constructed such lists.

#lang racket
(require rackunit rackunit/text-ui)
(require r5rs/init)

(define (has-cycle? x)
  (let ((counted '()))
    (define (cycle? x)
      (cond ((null? x) #f)
            ((memq x counted) #t)
            (else
             (set! counted (cons x counted))
             (cycle? (cdr x)))))
    (cycle? x)))

(define (lastpair x)
  (if (null? (cdr x))
      x
      (lastpair (cdr x))))

(define (make-cycle x)
  (set-cdr! (lastpair x) x)
  x)

(define sicp-3.18-tests
  (test-suite
    "Tests for SICP exercise 3.18"

    (check-true (has-cycle? (make-cycle '(a b c d))))
    (check-false (has-cycle? '(a b c d)))
))

(run-tests sicp-3.18-tests)

;; test
(define z1 (make-cycle (list 'a 'b 'c)))
(define z2 (list 'a 'b 'c))

(has-cycle? z1)
(has-cycle? z2)
