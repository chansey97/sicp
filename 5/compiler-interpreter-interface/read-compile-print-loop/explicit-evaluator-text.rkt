#lang racket
(provide (all-defined-out))

(define ec-registers '(exp env val proc argl continue unev))

(define ec-repl-controller-text
  '(read-eval-print-loop
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (assign val (op compile) (reg exp))
      (goto (reg val))
    print-result
      (perform (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))
    unknown-procedure-type
      (restore continue)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))
    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    unknown-expression-type
      (perform (op error) (const "Unknown expression type") (reg exp))
    unknown-procedure-type
      (perform (op error) (const "Unknown procedure type") (reg proc))))
