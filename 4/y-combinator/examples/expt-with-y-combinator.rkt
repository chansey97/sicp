#lang racket
(require "../y-combinator.rkt")

;; original expt

;; (define (expt x n)
;;   (cond ((= n 0) 1)
;;         (else 
;;          (* x (expt x (- n 1))))))

(define expt-f
  (lambda (g)
    (lambda(x n)
      (cond ((= n 0) 1)
            (else
             (* x (g x (- n 1))))))))

;; loop!!!!!
;; (define expt (y-combinator expt-f))


;; (expt 2 0)
