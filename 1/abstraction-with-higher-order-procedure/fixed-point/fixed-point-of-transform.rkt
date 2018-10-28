#lang racket
(provide (all-defined-out))
(require "./fixed-point.rkt")

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(module+ main
  (require "./average-damp.rkt")
  (require "./newton-method.rkt")
  
  ;; fixed-point-of-transform average-damped the function: y |-> x/y, then compute fixed point
  (define (sqrt-1 x)
    (fixed-point-of-transform
     (lambda (y) (/ x y)) average-damp 1.0))
  
  (sqrt-1 2)

  ;; fixed-point-of-transform newton transform the function: y |-> y^2 - x, then compute fixed point
  (define (sqrt-2 x)
    (fixed-point-of-transform
     (lambda (y) (- (square y) x)) newton-transform 1.0))

  (define (square x)
    (* x x))
  
  (sqrt-2 2)
  )
