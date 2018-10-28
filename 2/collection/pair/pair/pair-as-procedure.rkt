;; When we introduced compound data, we observed in Section 2.1.3 that
;; pairs can be represented purely in terms of procedures:
#lang racket

(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
