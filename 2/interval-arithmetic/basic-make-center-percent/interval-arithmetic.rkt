; SICP exercise 2.12
;
; Define a constructor make-center-percent that takes a center and a percentage
; tolerance and produces the desired interval. You must also define a selector
; percent that produces the percentage tolerance for a given interval. The
; center selector is the same as the one shown above.

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

; Exercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent value tolerance)
  (let ((width (* value (/ tolerance 100))))
    (make-interval (- value tolerance) (+ value tolerance))))

(define (percent i)
  (* (/ (width i) (center i))
     100))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
