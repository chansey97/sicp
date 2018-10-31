#lang racket
(require "./evaluator.rkt")
(require "./helpers.rkt")
(require "./database.rkt")

(ns-initialize (module->namespace "evaluator.rkt"))
(add-to-data-base! database-assertions)
(query-driver-loop)

;; add assertion
;; (assert! (job (Bitdiddle Ben) (computer wizard)))

;; query
;; (job ?x (computer wizard))

