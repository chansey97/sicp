; SICP exercise 3.66
;
; Examine the stream (pairs integers integers). Can you make any general
; comments about the order in which the pairs are placed into the stream? For
; example, approximately how many pairs precede the pair (1, 100)? the pair
; (99, 100)? the pair (100, 100)? (If you can make precise mathematical
; statements here, all the better. But feel free to give more qualitative
; answers if you find yourself getting bogged down.)

; The stream looks like this:
;
;   (1 1)
;   (1 2)
;   (2 2)
;   (1 3)
;   (2 3)
;   (1 4)
;   (3 3)
;   (1 5)
;   (2 4)
;   (1 6)
;   (3 4)
;   (1 7)
;   (2 5)
;   (1 8)
;   (4 4)
;   (1 9)
;   (2 6)
;   (1 10)
;   (3 5)
;   (1 11)
;
; After the first one, every second element starts with 1. If we remove those,
; we get this:
;
;   (2 2)
;   (2 3)
;   (3 3)
;   (2 4)
;   (3 4)
;   (2 5)
;   (4 4)
;   (2 6)
;   (3 5)
;
; We see the same behavior - every second element after the first starts with
; 2. That goes all the way, since the pairs starting with x are interleaved
; with the pairs starting with (x + 1). The interleaving of both streams is
; interleaved with the pairs that start with x - 1. Here are the positions of
; each pair, starting with a specific element
;
;   (1 x):  1  2  4  6  8 10
;   (2 x):  3  5  9 13 17 21
;   (3 x):  7 11 19 27 35 43
;   (4 x): 15 23 39 55 71 87
;
; It is easy to see that (i i) is at the 2ⁱ - 1 position, (i i+1) is 2ⁱ⁻¹
; positions apart and all other pairs starting with i are 2ⁱ positions apart
; afterwards.
;
; Thus, pos((a b)) is
;
;   b - a = 0,   2ⁱ - 1
;   b - a ≥ 1,   2ⁱ - 1 + 2ⁱ⁻¹ + (b-a-1)2ⁱ

#lang racket
(provide (all-defined-out))
(require "./stream.rkt")

;; Infinite streams of pairs
;; the stream of all pairs of integers (i, j) (i <= j).
(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(module+ main
  (require rackunit rackunit/text-ui)
  (require "./integers.rkt")
  
  (define int-pairs (pairs integers integers))

  (define (location stream pair)
    (define (count stream n)
      (if (equal? (stream-car stream) pair)
          n
          (count (stream-cdr stream) (+ n 1))))
    (count stream 1))

  (define (position pair)
    (let* ((a (car pair))
           (b (cadr pair))
           (first (- (expt 2 a) 1))
           (second (expt 2 (- a 1))))
      (cond ((= a b) first)
            ((= (+ a 1) b) (+ first second))
            (else (+ first second (* (- b a 1) (expt 2 a)))))))
  
  (define sicp-3.66-tests
    (test-suite
     "Tests for SICP exercise 3.66"

     (check-equal? (location int-pairs '(1 100)) (position '(1 100)))
     (check-equal? (location int-pairs '(7 30)) (position '(7 30)))
     ))

  (run-tests sicp-3.66-tests)
  )



