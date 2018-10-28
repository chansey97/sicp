; SICP exercise 3.70
;
; It would be nice to be able to generate streams in which the pairs appear in
; some useful order, rather than in the order that results from an ad hoc
; interleaving process. We can use a technique similar to the merge procedure
; in figure 3.56, if we define a way to say that one pair of integers is "less
; than" another. One way to do this is to define a "weighting function"
; W(i, j) and stipulate that (i₁, j₁) is less than (i₂, j₂) if
; W(i₁, j₁) < W(i₂, j₂). Write a procedure merge-weighted that is like merge,
; except that merge-weighted takes an additional argument weight, which is a
; procedure that computes the weight of a pair, and is used to determine the
; order in which elements should appear in the resulting merged stream. Using
; this, generalize pairs to a procedure weighted-pairs that takes two streams,
; together with a procedure that computes a weighting function, and generates
; the stream of pairs, ordered according to weight. Use your procedure to
; generate
;
; a. the stream of all pairs of positive integers (i, j) with i ≤ j ordered
; according to the sum i + j
;
; b. the stream of all pairs of positive integers (i, j) with i ≤ j, where
; neither i nor j is divisable by 2, 3, or 5 and the pairs are ordered
; according to the sum 2i + 3j + 5ij.

#lang racket
(provide (all-defined-out))
(require "./stream.rkt"
         "./pairs.rkt")

(define (merge-weighted s t weight)
  (let* ((s0 (stream-car s))
         (t0 (stream-car t))
         (s0w (weight s0))
         (t0w (weight t0)))
    (if (< s0w t0w)
        (stream-cons s0 (merge-weighted (stream-cdr s) t weight))
        (stream-cons t0 (merge-weighted s (stream-cdr t) weight)))))

(define (weighted-pairs s t weight)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(module+ main
  (require rackunit rackunit/text-ui)
  (require "./integers.rkt")
  
  (define (a-pairs)
    (define (pair-sum pair) (+ (car pair) (cadr pair)))
    (weighted-pairs integers integers pair-sum))
  
  ;; (stream-take (a-pairs) 10)
  
  (define (b-pairs)
    (define (useful? integer)
      (not (or (= (remainder integer 2) 0)
               (= (remainder integer 3) 0)
               (= (remainder integer 5) 0))))
    (define (weight pair)
      (let ((i (car pair)) (j (cadr pair)))
        (+ (* 2 i)
           (* 3 j)
           (* 5 i j))))
    (define useful-integers (stream-filter useful? integers))
    (weighted-pairs useful-integers useful-integers weight))

  ;; (stream-take (b-pairs) 10)
  
  (define sicp-3.70-tests
    (test-suite
     "Tests for SICP exercise 3.70"

     (check-equal? (stream-take (a-pairs) 20)
                   '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3)
                           (2 4) (1 5) (3 4) (2 5) (1 6) (4 4) (3 5)
                           (2 6) (1 7) (4 5) (3 6) (2 7) (1 8)))
     (check-equal? (stream-take (b-pairs) 20)
                   '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23)
                           (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47)
                           (1 49) (1 53) (7 11) (1 59) (1 61) (7 13)))
     ))

  (run-tests sicp-3.70-tests)
  )

