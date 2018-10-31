; SICP exercise 4.68
;
; Define rules to implement the reverse operation of exercise 2.18, which
; returns a list containing the same elements as a given list in reverse
; order. (Hint: Use append-to-form.) Can your rules answer both
; (reverse (1 2 3) ?x) and (reverse ?x (1 2 3))?

; They can't. (reverse ?x (1 2 3)) gets the system stuck in an infinite loop.

#lang racket
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")
(require "../helpers.rkt")
(require "../database.rkt")

(ns-initialize (module->namespace "../evaluator.rkt"))
(add-to-data-base! database-assertions)

;; https://github.com/skanev/playground/blob/master/scheme/sicp/04/68.scm
;; (add-to-data-base!
;;  '((rule (reverse (?x) (?x)))
;;    (rule (reverse (?a . ?b) ?c)
;;          (and (reverse ?b ?r-b)
;;               (append-to-form ?r-b (?a) ?c)))))

;; Another way:
;; reverse :: [a] -> [a]
;; reverse [] = []
;; reverse (x:xs) = reverse xs ++ [x]
(add-to-data-base!
 '(
   (rule (reverse () ()))
   (rule (reverse (?x . ?xs) ?z)
         (and (reverse ?xs ?r)
              (append-to-form ?r (?x) ?z)))
   ))


(matches-of '(reverse (1 2 3) ?x))
(matches-of '(reverse ?x (1 2 3))) ; return the empty stream if it detects a loop.
