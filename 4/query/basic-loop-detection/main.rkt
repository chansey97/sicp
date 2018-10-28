#lang racket
(require "evaluator.rkt")
(require "database.rkt") ; add assertions and rules to database
(ns-initialize (module->namespace "evaluator.rkt"))
(query-driver-loop)

;; add assertion
;; (assert! (job (Bitdiddle Ben) (computer wizard)))

;; query
;; (job ?x (computer wizard))

