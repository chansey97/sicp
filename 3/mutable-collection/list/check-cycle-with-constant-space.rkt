; SICP exercise 3.19
;
; Redo exercise 3.18 using an algorithm that takes only a constant amount of
; space. (This requires a very clever idea.)

#lang racket
(require rackunit rackunit/text-ui)
(require r5rs/init)

;; Why Floyd's cycle detection algorithm works? Detecting loop in a linked list.
;; https://www.youtube.com/watch?v=LUm2ABqAs1w

(define (has-cycle? x)
  (if (null? x)
      #f
      (let ((p1 x)
            (p2 (cdr x)))
        (define (loop)
          (cond ((null? p2) #f)
                ((null? (cdr p2)) #f)
                ((eq? p1 p2) #t)
                (else
                 (set! p1 (cdr p1))
                 (set! p2 (cddr p2))
                 (loop))))
        (loop))))

(define (lastpair x)
  (if (null? (cdr x))
      x
      (lastpair (cdr x))))

(define (make-cycle x)
  (set-cdr! (lastpair x) x)
  x)

(define sicp-3.19-tests
  (test-suite
    "Tests for SICP exercise 3.19"

    (check-true (has-cycle? (make-cycle '(a b c d))))
    (check-true (has-cycle? (make-cycle '(a b c d e))))

    (check-false (has-cycle? '()))
    (check-false (has-cycle? '(a)))
    (check-false (has-cycle? '(a b)))
    (check-false (has-cycle? '(a b c)))
    (check-false (has-cycle? '(a b c d)))
))

(run-tests sicp-3.19-tests)

;; test
(define z1 (make-cycle (list 'a 'b 'c)))
(define z2 (list 'a 'b 'c))

(has-cycle? z1)
(has-cycle? z2)
