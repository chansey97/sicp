; SICP exercise 4.69
;
; Beginning with the data base and the rules you formulated in exercise 4.63,
; devise a rule for adding "greats" to a grandson relationship. This should
; enable the system to deduce that Irad is the great-grandson of Adam, or that
; Jabal and Jubal are the great-great-great-great-great-grandsons of Adam.
; (Hint: Represent the fact about Irad, for example, as ((great grandson) Adam
; Irad). Write rules that determine if a list ends in the word grandson. Use
; this to express a rule that allows one to derive the relationship
; ((great . ?rel) ?x ?y), where ?rel is a list ending in grandson). Check your
; rules on queries such as ((great grandson) ?g ?ggs) and
; (?relationship Adam Irad).

; We base this on the loop detection in 4.67, since otherwise
; (?relationship Adam Irad) will fall into such a loop.

#lang racket
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")
(require "../test-helpers.rkt")
(require "../database.rkt")

(ns-initialize (module->namespace "../evaluator.rkt"))

(define grands-rules
  '((son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    (rule (son ?father ?son)
          (and (wife ?father ?mother)
               (son ?mother ?son)))

    (rule (grandson ?grandfather ?son)
          (and (son ?grandfather ?father)
               (son ?father ?son)))

    (rule (ends-with-grandson (grandson)))
    (rule (ends-with-grandson (?x . ?rest))
          (ends-with-grandson ?rest))

    (rule ((grandson) ?grandfather ?grandson)
          (grandson ?grandfather ?grandson))

    (rule ((great . ?rel) ?ancestor ?descendant)
          (and (ends-with-grandson ?rel)
               (son ?ancestor ?son-of-ancestor)
               (?rel ?son-of-ancestor ?descendant)))))

(define sicp-4.69-tests
  (test-suite
    "Tests for SICP exercise 4.69"

    (add-to-data-base! grands-rules)

    (check-equal? (matches-of '((great grandson) ?g ?ggs))
                  '(((great grandson) Adam Irad)
                    ((great grandson) Cain Mehujael)
                    ((great grandson) Enoch Methushael)
                    ((great grandson) Irad Lamech)
                    ((great grandson) Mehujael Jabal)
                    ((great grandson) Mehujael Jubal)))

    (check-equal? (matches-of '(?relationship Adam Irad))
                  '(((great grandson) Adam Irad)))
))

(run-tests sicp-4.69-tests)
