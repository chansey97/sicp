#lang racket
(provide fixed-point fixed-point-print-sequence)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-print-sequence f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iteration)
    (let ((next (f guess)))
      (display iteration)
      (display ". ")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ iteration 1)))))
  (try first-guess 1))

(module+ main
  (require "./average-damp.rkt")
  
  (fixed-point cos 1.0)
  
  ;; a solution to the equation y = siny + cosy,
  ;; that is compute fixed-point of the function y -> siny + cosy
  (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

  ;; Computing the square root of some number x requires finding a y such
  ;; that y2 = x. Putting this equation into the equivalent form y = x=y,
  ;; we recognize that we are looking for a fixed point of the function58
  ;; y 7! x=y, and we can therefore try to compute square roots as:

  ;; (define (sqrt x)
  ;;   (fixed-point (lambda (y) (/ x y))
  ;;                1.0))

  ;; Unfortunately, this fixed-point search does not converge.

  ;; (define (sqrt x)
  ;;   (fixed-point (lambda (y) (average y (/ x y)))
  ;;                1.0))

  ;; (define (average x y)
  ;;   (/ (+ x y) 2))

  ;; square-root computation from this section (where we look for a fixed point of the average-damped version of y -> x/y)
  (define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))
  
  (sqrt 2)
  )

