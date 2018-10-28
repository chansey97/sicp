#lang racket
(require "./complex.rkt")

;; complex operations
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define complex1 (make-from-real-imag 10 20))
(define complex2 (make-from-real-imag -1 0))
(define complex3 (make-from-mag-ang 1 3.14159))

(add-complex complex1 complex2)
(add-complex complex1 complex3)

(mul-complex complex1 complex2)
(mul-complex complex1 complex3)
