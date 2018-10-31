#lang racket
(require "../evaluator.rkt")
(require "../helpers.rkt")
(require "../database.rkt")

;; (define (merge x y)
;;   (cond
;;     ((null? x) y)
;;     ((null? y) x)
;;     (else
;;      (let ((a (car x)) (b (car y)))
;;        (if (< a b)
;;            (cons a
;;                  (merge (cdr x) y))
;;            (cons b
;;                  (merge x (cdr y))))))))

;; merge xs []         = xs
;; merge [] ys         = ys
;; merge (x : xs) (y : ys) | x <= y    = x : merge xs (y : ys)  -- the whole definition as conclusion,
;;                         | otherwise = y : merge (x:xs) ys

;; (merge '(2 3) '(1 4))

(ns-initialize (module->namespace "../evaluator.rkt"))

(add-to-data-base! '(
                     (rule (merge-to-form () ?y ?y))
                     (rule (merge-to-form ?y () ?y))
                     
                     (rule
                      (merge-to-form (?a . ?x) (?b . ?y) (?b . ?z))
                      (and (merge-to-form (?a . ?x) ?y ?z)
                           (lisp-value > ?a ?b)))
                     
                     (rule (merge-to-form (?a . ?x) (?b . ?y) (?a . ?z))
                           (and (merge-to-form ?x (?b . ?y) ?z)
                                (lisp-value > ?b ?a)))
                     ))

(matches-of '(merge-to-form (2 3) (1 4) (1 2 3 4)))
(matches-of '(merge-to-form ?x ?y (1 2 3 4)))
(matches-of '(merge-to-form (2 ?a) ?x (1 2 3 4)))


;; (matches-of '(merge-to-form (2 3) (1 4) (1 2 3 4)))
