#lang racket
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")
(require "../helpers.rkt")
(require "../database.rkt")

(ns-initialize (module->namespace "../evaluator.rkt"))

;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x) (append (cdr x) y))))

;; I think, append is more prefer input/output rather than relation, so we need function - logic conversion.

;; Function perspective:
;; append-to-form [] y = y
;; append-to-form (u : v) y = u : (append v y) ; the whole definition as conclusion, (append v y) is the ?z in logic perspective

;; Logic perspective:
;; The conclusion (append-to-form (?u . ?v) ?y (?u . ?z)) is true, 
;; implies(append-to-form ?v ?y ?z) is true

(add-to-data-base! '(
                     (rule (append-to-form () ?y ?y))
                     (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                           (append-to-form ?v ?y ?z))
                     ))

;; (matches-of '(append-to-form (a b) (c d) ?z))
;; (matches-of '(append-to-form (a b) ?y (a b c d)))
;; (matches-of '(append-to-form ?x ?y (a b c d)))
;; (matches-of '(append-to-form ?x ?y (a)))

;; Maybe simulate sum ?
;; 2+3
(matches-of '(append-to-form (1 1) (1 1 1) ?sum))
(matches-of '(append-to-form ?a (1 1 1) (1 1 1 1 1)))
