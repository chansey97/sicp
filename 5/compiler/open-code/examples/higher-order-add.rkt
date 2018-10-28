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

;; no effect on open-code
(define higer-order-add '(begin
                           (define (add proc p1 p2)
                             (proc p1 p2))
                           (add + 1 2)
                           ))

(pretty-print (compiled-instructions higer-order-add))

(run higer-order-add)
