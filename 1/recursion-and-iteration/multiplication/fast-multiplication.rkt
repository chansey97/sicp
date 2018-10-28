; SICP exercise 1.17
;
; The exponentiation algorithms in this section are based on performing
; exponentiation by means of repeated multiplication. In a similar way, one can
; perform integer multiplication by means of repeated addition. The following
; multiplication procedure (in which it is assumed that our language can only
; add, not multiply) is analogous to the expt procedure.
;
; (define (* a b)
;   (if (= b 0)
;       0
;       (+ add (* a (- b 1)))))
;
; This algorithm takes a number of steps that is linear in b. Now suppose we
; include together with addition, operations double, which doubles an integer,
; and halve, which divides an (even) integer by 2. Using these, design a
; multiplication procedure analogous to fast-expt that uses a logarithmic
; number of steps.

#lang racket
(require rackunit rackunit/text-ui)

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (** a b)
  (cond ((= b 0) 0)
        ((even? b) (** (double a) (halve b)))
        (else (+ a (** a (- b 1))))))

(define sicp-1.17-tests
  (test-suite
   "Tests for SICP exercise 1.17"

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

(run-tests sicp-1.17-tests)
