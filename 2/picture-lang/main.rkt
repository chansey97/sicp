#lang racket
(require "./private/dc.rkt"
         "./private/frame.rkt"
         "./private/operations.rkt"
         "./private/primitive-painters.rkt"
         "./private/segment.rkt"
         "./private/vector.rkt")

(provide (all-from-out  "./private/dc.rkt"
                        "./private/frame.rkt"
                        "./private/operations.rkt"
                        "./private/primitive-painters.rkt"
                        "./private/segment.rkt"
                        "./private/vector.rkt"))


(module+ main
  (require racket/gui/base)
  (require racket/draw)
  
  ;; painter-instances
  (define outline
    (segments->painter
     (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
           (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
           (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
           (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))

  (define diamond
    (segments->painter
     (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
           (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
           (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
           (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
           )))

  (define cross
    (segments->painter
     (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
           (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

  (define wave
    (segments->painter
     (list (make-segment (make-vect 0.00 0.70) (make-vect 0.16 0.57))
           (make-segment (make-vect 0.16 0.57) (make-vect 0.30 0.67))
           (make-segment (make-vect 0.30 0.67) (make-vect 0.37 0.67))
           (make-segment (make-vect 0.37 0.67) (make-vect 0.40 0.64))
           (make-segment (make-vect 0.40 0.64) (make-vect 0.42 0.68))
           (make-segment (make-vect 0.42 0.68) (make-vect 0.32 0.80))
           (make-segment (make-vect 0.32 0.80) (make-vect 0.33 0.85))
           (make-segment (make-vect 0.33 0.85) (make-vect 0.36 1.00))

           (make-segment (make-vect 0.60 1.00) (make-vect 0.62 0.84))
           (make-segment (make-vect 0.62 0.84) (make-vect 0.62 0.78))
           (make-segment (make-vect 0.62 0.78) (make-vect 0.53 0.70))
           (make-segment (make-vect 0.53 0.70) (make-vect 0.57 0.64))
           (make-segment (make-vect 0.57 0.64) (make-vect 0.63 0.67))
           (make-segment (make-vect 0.63 0.67) (make-vect 0.68 0.66))
           (make-segment (make-vect 0.68 0.66) (make-vect 0.87 0.51))
           (make-segment (make-vect 0.87 0.51) (make-vect 1.00 0.40))

           (make-segment (make-vect 1.00 0.30) (make-vect 0.73 0.52))
           (make-segment (make-vect 0.73 0.52) (make-vect 0.61 0.53))
           (make-segment (make-vect 0.61 0.53) (make-vect 0.67 0.25))
           (make-segment (make-vect 0.67 0.25) (make-vect 0.71 0.00))

           (make-segment (make-vect 0.60 0.00) (make-vect 0.56 0.23))
           (make-segment (make-vect 0.56 0.23) (make-vect 0.51 0.28))
           (make-segment (make-vect 0.51 0.28) (make-vect 0.46 0.28))
           (make-segment (make-vect 0.46 0.28) (make-vect 0.40 0.12))
           (make-segment (make-vect 0.40 0.12) (make-vect 0.36 0.00))

           (make-segment (make-vect 0.23 0.00) (make-vect 0.34 0.30))
           (make-segment (make-vect 0.34 0.30) (make-vect 0.36 0.52))
           (make-segment (make-vect 0.36 0.52) (make-vect 0.32 0.55))
           (make-segment (make-vect 0.32 0.55) (make-vect 0.28 0.55))
           (make-segment (make-vect 0.28 0.55) (make-vect 0.17 0.45))
           (make-segment (make-vect 0.17 0.45) (make-vect 0.00 0.60))
           )))

  (define smiling-wave
    (segments->painter
     (list (make-segment (make-vect 0.00 0.70) (make-vect 0.16 0.57))
           (make-segment (make-vect 0.16 0.57) (make-vect 0.30 0.67))
           (make-segment (make-vect 0.30 0.67) (make-vect 0.37 0.67))
           (make-segment (make-vect 0.37 0.67) (make-vect 0.40 0.64))
           (make-segment (make-vect 0.40 0.64) (make-vect 0.42 0.68))
           (make-segment (make-vect 0.42 0.68) (make-vect 0.32 0.80))
           (make-segment (make-vect 0.32 0.80) (make-vect 0.33 0.85))
           (make-segment (make-vect 0.33 0.85) (make-vect 0.36 1.00))

           (make-segment (make-vect 0.60 1.00) (make-vect 0.62 0.84))
           (make-segment (make-vect 0.62 0.84) (make-vect 0.62 0.78))
           (make-segment (make-vect 0.62 0.78) (make-vect 0.53 0.70))
           (make-segment (make-vect 0.53 0.70) (make-vect 0.57 0.64))
           (make-segment (make-vect 0.57 0.64) (make-vect 0.63 0.67))
           (make-segment (make-vect 0.63 0.67) (make-vect 0.68 0.66))
           (make-segment (make-vect 0.68 0.66) (make-vect 0.87 0.51))
           (make-segment (make-vect 0.87 0.51) (make-vect 1.00 0.40))

           (make-segment (make-vect 1.00 0.30) (make-vect 0.73 0.52))
           (make-segment (make-vect 0.73 0.52) (make-vect 0.61 0.53))
           (make-segment (make-vect 0.61 0.53) (make-vect 0.67 0.25))
           (make-segment (make-vect 0.67 0.25) (make-vect 0.71 0.00))

           (make-segment (make-vect 0.60 0.00) (make-vect 0.56 0.23))
           (make-segment (make-vect 0.56 0.23) (make-vect 0.51 0.28))
           (make-segment (make-vect 0.51 0.28) (make-vect 0.46 0.28))
           (make-segment (make-vect 0.46 0.28) (make-vect 0.40 0.12))
           (make-segment (make-vect 0.40 0.12) (make-vect 0.36 0.00))

           (make-segment (make-vect 0.23 0.00) (make-vect 0.34 0.30))
           (make-segment (make-vect 0.34 0.30) (make-vect 0.36 0.52))
           (make-segment (make-vect 0.36 0.52) (make-vect 0.32 0.55))
           (make-segment (make-vect 0.32 0.55) (make-vect 0.28 0.55))
           (make-segment (make-vect 0.28 0.55) (make-vect 0.17 0.45))
           (make-segment (make-vect 0.17 0.45) (make-vect 0.00 0.60))

           (make-segment (make-vect 0.41 0.78) (make-vect 0.54 0.78))
           (make-segment (make-vect 0.54 0.78) (make-vect 0.52 0.76))
           (make-segment (make-vect 0.52 0.76) (make-vect 0.43 0.76))
           (make-segment (make-vect 0.43 0.76) (make-vect 0.41 0.78)))))

  (define rogers-size 150)
  (define rogers-bitmap (make-object bitmap% rogers-size rogers-size))
  (send rogers-bitmap load-file "rogers.jpg")

  (define rogers
    (bitmap->painter rogers-bitmap))

  ;; gui canvas
  (define picture-size 640)
  (define picture-margin 20)
  (define canvas-width (+ picture-size picture-margin picture-margin))
  (define canvas-height (+ picture-size picture-margin picture-margin))

  (define target (make-bitmap canvas-width canvas-height))
  (define dc (new bitmap-dc% [bitmap target]))
  ;; (send dc translate 0 canvas-height)
  ;; (send dc scale 1 -1)
  (send dc set-smoothing 'smoothed)
  (send dc set-font (make-object font% 12 'system 'normal 'bold))
  (send dc set-text-foreground "dim gray")
  (dc-initialize dc)

  (define (draw-picture picture)
    (picture (make-frame (make-vect picture-margin picture-margin)
                         (make-vect picture-size 0.0)
                         (make-vect 0.0 picture-size)))) 

  ;; (draw-picture outline)
  ;; (draw-picture cross)
  ;; (draw-picture diamond)
  ;; (draw-picture (beside diamond outline))
  ;; (draw-picture (below outline cross))
  ;; (draw-picture (below (beside cross outline)
  ;;                      (beside diamond cross)))

  ;; (draw-picture wave)
  ;; (draw-picture (flip-horiz wave))
  ;; (draw-picture (rotate180 wave))
  ;; (draw-picture (rotate270 wave))
  ;; (draw-picture smiling-wave)
  ;; (draw-picture (flipped-pairs wave))
  ;; (draw-picture (inverted-square-limit wave 4))
  ;; (draw-picture (squash-inwards wave))
  ;; (draw-picture (up-split outline 4))
  ;; (draw-picture (corner-split outline 4))
  ;; (draw-picture (simpler-corner-split outline 4))
  ;; (draw-picture (square-limit wave 4))

  ;; (draw-picture rogers)
  ;; (draw-picture (simpler-square-limit rogers 4))
  ;; (draw-picture (square-limit rogers 4))
  ;; (draw-picture (inverted-square-limit (flip-vert rogers) 4))
  ;; (draw-picture (squash-inwards rogers))
  (draw-picture (square-limit (squash-inwards rogers) 4))

  ;; gui
  (define frame (new frame% [label "Example"]))
  (define canvas
    (new canvas% [parent frame]
         [min-width canvas-width] [min-height canvas-height]
         [stretchable-width #f] [stretchable-height #f]
         [paint-callback
          (lambda (canvas dc)
            (send dc set-background "yellow")
            (send dc clear)
            (send dc draw-bitmap target 0 0))]))

  (send frame reflow-container)
  (send frame show #t))
