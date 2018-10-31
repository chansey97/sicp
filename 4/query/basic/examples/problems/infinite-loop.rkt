; SICP exercise 4.64
;
; Louis Reasoner mistakenly deletes the outranked-by rule (section 4.4.1) from
; the data base. When he realizes this, he quickly reinstalls it.
; Unfortunatelly, he makes a slight change in the rule, and types it in as
;
; (rule (outranked-by ?staff-person ?boss)
;       (or (supervisor ?staff-person ?boss)
;           (and (outranked-by ?middle-manager ?boss)
;                (supervisor ?staff-person ?middle-manager))))
;
; Just after Louis types this information into the system, DeWitt Aull comes
; by to find out who outranks Ben Bitdiddle. He issues the query
;
; (outranked-by (Bitdiddle Ben) ?who)
;
; After answering, the system goes into an infinite loop. Explain why.

; First the system will answer with
;
; (outranked-by (Bitdiddle Ben) (Warbucks Oliver))
;
; because the first disjunct will match an existing assertion. Afterwards, the
; system will try to match (outranked-by ?middle-manager ?boss), which means
; applying the rule again. In this second application, we will first get all
; supervisor assertions (because they are the first clause of the disjunction),
; but when the second disjunct gets evaluated, it will invoke the rule again.
; This will get everything stuck in a loop.

;; How to detect it in advance?
;; See Exercise 4.67 "../../../basic-loop-detection"

#lang racket
(require "../../evaluator.rkt")
(require "../../helpers.rkt")
(require "../../database.rkt")

(ns-initialize (module->namespace "../../evaluator.rkt"))
(add-to-data-base! database-assertions)

;; Same definition, just reversed the last two clauses...
;; If this were true logic, they would both work.

(add-to-data-base! '(
                     ;; Use this rule is ok
                     
                     (rule (outranked-by ?s ?b)
                           (or (supervisor ?s ?b)
                               (and (supervisor ?s ?m)
                                    (outranked-by ?m ?b))))

                     ;; Use this rule will go into infinite loop, because it is will call itself again with ?m with no restriction set to ?m.
                     ;; The first version will have a constraint on ?m set by the query on supervisor.

                     ;; (rule (outranked-by ?s ?b)
                     ;;       (or (supervisor ?s ?b)
                     ;;           (and (outranked-by ?m ?b)
                     ;;                (supervisor ?s ?m))))
                     ))
