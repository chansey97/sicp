;; Consider this problem:
;; Given a positive integer n, find all ordered pairs of distinct positive
;; integers i and j, where 1 <= j < i <= n, such that i + j is prime. 

#lang racket
(require "../list.rkt")

;; prime-sum-pairs can be implement with syntax sugar:
;; for*/list in racket, see prime-sum-pairs-with-forlist.rkt
;; COLLECT in MIT Scheme
;; list-comprehension in haskell

;; prime-sum-pairs :: Int -> [(Int, Int)]
;; prime-sum-pairs n = [(i, j, i + j) | i <- [1..n],  
                                     ;; j <- [1..i-1],
                                     ;; prime-sum? (i, j)]

;; (DEFINE (PRIME-SUM-PAIRS N)
;;  (COLLECT
;;   (LIST I J (+ I J))
;;   ((I (ENUM-INTERVAL 1 N))
;;   (J (ENUM-INTERVAL 1 (-1+ I))))
;;   (PRIME? (+ I J))))

;; Note:
;; the prime-sum-pairs here is not exactly equivalent to syntax sugar above
;; In here, prime-sum-pairs's filter is outside flatmap
;; In the syntax sugar, guard inside flatmap
;; See prime-sum-pairs-with-forlist-2.rkt, eight-queens-forlist-2.rkt

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (null?
   (filter (lambda (x) (= 0 (remainder n x)))
           (enumerate-interval 2 (- n 1)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(prime-sum-pairs 5)
