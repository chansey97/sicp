;; An example of rule apply:
;; https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/7b-metacircular-evaluator-part-2/ 23:15

#lang racket
(require "../evaluator.rkt")
(require "../helpers.rkt")
(require "../database.rkt")

(ns-initialize (module->namespace "../evaluator.rkt"))
(add-to-data-base! database-assertions)
(add-to-data-base! '((rule (boss ?z ?d)
                           (and (job ?x (?d . ?y))
                                (supervisor ?x ?z)))
                     ))

;; (matches-of '(boss (Hacker Alyssa P) computer))
;; (matches-of '(boss (Bitdiddle Ben) computer))
