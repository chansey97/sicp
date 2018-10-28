;; Consider this problem:
;; Given a positive integer n, find all ordered pairs of distinct positive
;; integers i and j, where 1 <= j < i <= n, such that i + j is prime. 

#lang racket
(require "../list.rkt")

(define (prime-sum-pairs n)
  (for*/list ([i (enumerate-interval 1 n)]
              [j (enumerate-interval 1 (- i 1))]
              #:when (prime? (+ i j)))
    (list i j (+ i j))
    )
  )

(define (prime? n)
  (null?
   (filter (lambda (x) (= 0 (remainder n x)))
           (enumerate-interval 2 (- n 1)))))

(prime-sum-pairs 5)
