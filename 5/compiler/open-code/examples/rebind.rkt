#lang racket
(require "../operations.rkt")
(require "../helpers.rkt")
(require "../../../simulator/basic/simulator.rkt")

(define (run exp)
  (define compiled-code (compiled-instructions exp))
  (define machine (make-machine cm-registers cm-operations compiled-code))
  
  (set-register-contents! machine 'env the-global-environment)
  (start machine)
  (get-register-contents machine 'val)
  )

;; Behavior change:
;; Making the primitives into reserved words is in general a bad idea, since a user
;; cannot then rebind these names to different procedures. Moreover, if we add reserved
;; words to a compiler that is in use, existing programs that define procedures with these
;; names will stop working. See Exercise 5.44 for ideas on how to avoid this problem.
(define rebind '(begin
                  (define (+ p1 p2)
                    (- p1 p2))
                  (+ 1 2)
                  ))
(run rebind)
