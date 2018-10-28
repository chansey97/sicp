#lang racket
(provide newtons-method newton-transform deriv)
(require "./fixed-point.rkt")

;; newtons-method calculate g's zero point
;; which equivalent to fixed-point of (newton-transform g)
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(module+ main

  ;; derive
  
  (define (cube x)
    (* x x x))

  ((deriv cube) 5) ; 75.00014999664018

  ;; compute square root
  
  (define (square x)
    (* x x))

  ;; sqrt is a zero point of the function: y |-> y^2 - x, that is the y which make y^2 - x = 0
  (define (sqrt x)
    (newtons-method (lambda (y) (- (square y ) x ))
                    1.0))
  (sqrt 2)
  )
