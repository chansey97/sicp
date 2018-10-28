#lang racket
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")
(require "../test-helpers.rkt")
(require "../database.rkt")

(ns-initialize (module->namespace "../evaluator.rkt"))

;; (rule (append-to-form () ?y ?y))
;; (rule (append-to-form (?u . ?v) ?y (?u . ?z))
;;       (append-to-form ?v ?y ?z))

;; (matches-of '(append-to-form (a b) (c d) ?z))
;; (matches-of '(append-to-form (a b) ?y (a b c d)))
(matches-of '(append-to-form ?x ?y (a b c d)))
