;; Consider this problem:
;; Given a positive integer n, find all ordered pairs of distinct positive
;; integers i and j, where 1 <= j < i <= n, such that i + j is prime. 

#lang racket
(require "../list.rkt")

;; for*/list semantics

(define (prime-sum-pairs n)
  (flatmap
   (lambda (i)
     (map make-pair-sum
          (filter prime-sum?
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))))
   (enumerate-interval 1 n))
  )

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (null?
   (filter (lambda (x) (= 0 (remainder n x)))
           (enumerate-interval 2 (- n 1)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(prime-sum-pairs 5)
