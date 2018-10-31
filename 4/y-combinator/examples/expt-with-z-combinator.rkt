#lang racket
(require "../z-combinator.rkt")

;; original expt

;; (define (expt x n)
;;   (cond ((= n 0) 1)
;;         (else 
;;          (* x (expt x (- n 1))))))

;; Use lambda is better, because it can represent name are not necessary in recursion

;; (define (expt-f g)
;;   (lambda(x n)
;;     (cond ((= n 0) 1)
;;           (else
;;            (* x (g x (- n 1)))))))

(define expt-f
  (lambda (g)
    (lambda(x n)
      (cond ((= n 0) 1)
            (else
             (* x (g x (- n 1))))))))

(define expt (z-combinator-2args expt-f))

(expt 2 10)