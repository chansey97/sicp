; SICP exercise 4.61
;
; The following rules implement a next-to relation that finds adjancent
; elements of a list.
;
; (rule (?x next-to ?y in (?x ?y . ?u)))
;
; (rule (?x next-to ?y in (?v . ?z))
;       (?x next-to ?y in ?z))
;
; What will the response be to the following queries?
;
; (?x next-to ?y in (1 (2 3) 4))
; (?x next-to 1 in (2 1 3 1))

#lang racket
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")
(require "../helpers.rkt")
(require "../database.rkt")

(ns-initialize (module->namespace "../evaluator.rkt"))

(add-to-data-base!
 '(
   (rule (?x next-to ?y in (?x ?y . ?u)))
   (rule (?x next-to ?y in (?v . ?z))
         (?x next-to ?y in ?z))
   ))

(matches-of '(?x next-to ?y in (1 (2 3) 4)))
(matches-of '(?x next-to 1 in (2 1 3 1)))


