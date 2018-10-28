; SICP exercise 1.18
;
; Using the results of exercises 1.16 and 1.17, devise a procedure that
; generates an iterative process for multiplying two integers in terms of
; adding, doubling and halving and uses a logarithmic number of steps.

; Say we want to multiply a by b. We are going to do this iteratively and our
; invariant quantitiy will be ab + c, with c initially being 0. We iteratively
; apply the following transformations:
;
; ab + c = { 2a(b/2) + c       if b is even
;          { a(b-1) + a + c    if b is odd
;
; We conclude the result is c when b is zero.
#lang racket
(require rackunit rackunit/text-ui)

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (** a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))

  (iter a b 0))

(define sicp-1.18-tests
  (test-suite
   "Tests for SICP exercise 1.18"

   (check-equal? (** 5 1) 5)
   (check-equal? (** 5 2) 10)
   (check-equal? (** 5 3) 15)
   (check-equal? (** 5 4) 20)
   (check-equal? (** 5 5) 25)

   (check-equal? (** 1 2) 2)
   (check-equal? (** 2 2) 4)
   (check-equal? (** 3 2) 6)
   (check-equal? (** 4 2) 8)
   (check-equal? (** 5 2) 10)
   ))

(run-tests sicp-1.18-tests)
