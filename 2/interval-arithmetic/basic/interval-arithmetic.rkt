; SICP exercise 2.07
;
; Alyssa's program is incomplete because she has not specified the
; implementation of the interval abstraction. Here is a definition of the
; interval constructor:
;
; (define (make-interval a b) (cons a b))
;
; Define selectors upper-bound and lower-bound to complete the implementation.

; SICP exercise 2.08
;
; Using reasoning analogous to Alyssa's, describe how the difference of two
; intervals may be computed. Define a corresponding subtraction procedure,
; called sub-interval.

; We can be extremely elaborate here, but I don't really want to go there.
; Simply, the minimum is the lower bound of the minuend minus the upper bound
; of the subtrahend and vica-versa.

; SICP exercise 2.10
;
; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and
; comments that it is not clear what it means to divide by an interval that
; spans zero. Modify Alyssa's code to check for this condition and to signal an
; error if it occurs.

#lang racket
(provide (all-defined-out))

;; a lower-bound
;; b upper-bound
(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Exercise: 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.10

;; (define (div-interval x y)
;;   (mul-interval
;;    x
;;    (make-interval (/ 1.0 (upper-bound y))
;;                   (/ 1.0 (lower-bound y)))))

;; note: y's lower-bound still not be == zero
(define (div-interval x y)
  (if (spans-zero? y)
      (error "Cannot divide by an interval that spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (spans-zero? interval)
  (>= 0 (* (lower-bound interval) (upper-bound interval))))
