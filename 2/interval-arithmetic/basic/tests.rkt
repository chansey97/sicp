#lang racket
(require rackunit rackunit/text-ui)
(require "./interval-arithmetic.rkt")

(define sicp-2.07-tests
  (test-suite
   "Tests for SICP exercise 2.07"

   (check-equal? (lower-bound (make-interval 4 5)) 4)
   (check-equal? (upper-bound (make-interval 4 5)) 5)
   ))

(define sicp-2.08-tests
  (test-suite
   "Tests for SICP exercise 2.08"

   (check-equal? (sub-interval (make-interval 20 25)
                               (make-interval 1 2))
                 (make-interval 18 24))

   (check-equal? (sub-interval (make-interval 10 12)
                               (make-interval -3 -1))
                 (make-interval 11 15))
   ))

(define sicp-2.10-tests
  (test-suite
   "Tests for SICP exercise 2.10"

   ;; (spans-zero? (make-interval  1   1)) ;#f
   ;; (spans-zero? (make-interval  1  -1)) ;#t
   ;; (spans-zero? (make-interval -1  1)) ;#t
   ;; (spans-zero? (make-interval -1 -1)) ;#f

   ;; (spans-zero? (make-interval  0   1)) ;#t
   ;; (spans-zero? (make-interval  0  -1)) ;#t
   ;; (spans-zero? (make-interval  1   0)) ;#t
   ;; (spans-zero? (make-interval  -1  0)) ;#t
   ;; (spans-zero? (make-interval  0  0)) ;#f
   
   (check-equal? (div-interval (make-interval 10.0 20.0)
                               (make-interval 2.0 5.0))
                 (make-interval 2.0 10.0))

   (check-exn exn? (lambda () (div-interval (make-interval 10 20)
                                            (make-interval -1 1))))
   ))

(run-tests sicp-2.07-tests)
(run-tests sicp-2.08-tests)
(run-tests sicp-2.10-tests)
