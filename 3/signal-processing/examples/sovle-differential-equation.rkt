#lang racket
(require "../../stream/stream.rkt"
         "../signal-processing.rkt")

;; This is procedure is not guaranteed to work in all Scheme implementations, although
;; for any implementation there is a simple variation that will work. The problem has to
;; do with subtle differences in the ways that Scheme implementations handle internal
;; definitions. (See Section 4.1.6.)

;; Note: r5rs can't work, but racket can.
(define (solve f y0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            1000)
;; 2.716924
