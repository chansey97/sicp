; SICP exercise 3.67
;
; Modify the pairs procedure so that (pairs integers integers) will produce
; the stream of all pairs of integers (i, j) (without the condition i ≤ j).
; Hint: You will need to mix in an additional stream.

#lang racket
(provide (all-defined-out))
(require "./stream.rkt")

(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(module+ main
  (require "./integers.rkt")
  
  (define int-pairs (pairs integers integers))
  (println (stream-take int-pairs 100))
  )
