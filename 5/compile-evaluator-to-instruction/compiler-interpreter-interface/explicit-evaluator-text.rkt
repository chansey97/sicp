#lang racket
(provide (all-defined-out))

;; This not explicit evaluator text, but the instruction that simply jump to (reg val) which include compiled code.
(define ec-controller-text
  '(
      (branch (label external-entry)) ; branches if flag is set
      (assign continue (label done))
      
    ; External entries
    external-entry
      (perform (op initialize-stack))
      (assign env (op get-global-environment))
      (assign continue (label done))
      (goto (reg val))
    done))

