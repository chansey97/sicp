#lang racket
(provide segments->painter bitmap->painter)
(require "./dc.rkt"
         "./vector.rkt"
         "./frame.rkt"
         "./segment.rkt")

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (bitmap->painter bitmap)
  (lambda (frame)
    (let ((original-transformation (send dc get-transformation))
          (origin (origin-frame frame))
          (x-axis (edge1-frame frame))
          (y-axis (edge2-frame frame))
          (factor (/ 1.0 (- (send bitmap get-width) 2))))
      (send dc transform (vector (xcor-vect x-axis) (xcor-vect y-axis)
                                 (ycor-vect x-axis) (ycor-vect y-axis)
                                 (xcor-vect origin) (ycor-vect origin)))
      (send dc scale factor factor)

      (send dc draw-bitmap bitmap 0 0)

      (send dc set-transformation original-transformation))))

(define (draw-line a b)
  (let ((original-pen (send dc get-pen)))
    (send dc set-pen "black" 1 'solid)

    (send dc draw-line (xcor-vect a) (ycor-vect a)
                       (xcor-vect b) (ycor-vect b))

    (send dc set-pen original-pen)))
