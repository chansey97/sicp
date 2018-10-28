; SICP exercise 3.60
;
; With power series represented as streams of coefficients as in exercise
; 3.59, adding series is implemented by add-streams. Complete the definition
; of the following procedure for multiplying series:
;
;   (define (mul-series s1 s2)
;     (cons-stream <??> (add-streams <??> <??>)))
;
; You can test your procedure by verifying that sin²x + cos²x = 1, using the
; series from exercise 3.59

; It would have been good if I knew how to multiply series. Anyway, let's say
; we have (a₀ + A)(b₀ + B) where A and B are the remainder of the series.
; Then:
;
; (a₀ + A)(b₀ + B) = a₀b₀ + Ab₀ + Ba₀ + AB = a₀b₀ + a₀B + A(b₀ + B)
;
; In that expression, a₀b₀ is the first element of the series and the rest is
; the remaining elements. The solution is at the end.

#lang racket
(provide (all-defined-out))
(require "../stream/stream.rkt"
         "../stream/op-streams.rkt")

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(module+ main
  (require rackunit rackunit/text-ui)
  (require "../stream/integers.rkt")
  
  (define (integrate-series stream)
    (div-streams stream integers))

  (define cosine-series
    (stream-cons 1 (integrate-series (neg-stream sine-series))))

  (define sine-series
    (stream-cons 0 (integrate-series cosine-series)))
  
  (define sicp-3.60-tests
    (test-suite
     "Tests for SICP exercise 3.60"

     (check-equal? (stream-take (add-streams (mul-series sine-series sine-series)
                                             (mul-series cosine-series cosine-series))
                                6)
                   '(1 0 0 0 0 0))
     ))

  (run-tests sicp-3.60-tests)

  )
