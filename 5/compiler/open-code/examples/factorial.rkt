#lang racket
(require "../helpers.rkt")

(define compiled-factorial
  '(  (assign val (op make-compiled-procedure) (label entry1) (reg env))
      (goto (label after-lambda2))
    entry1
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

      ; <changed>
      ; We don't need to do a call here, which removes a bunch of instructions
      ; and a save/restore of continue and env
      (assign arg1 (op lookup-variable-value) (const n) (reg env))
      (assign arg2 (const 1))
      (assign val (op =) (reg arg1) (reg arg2))
      ; </changed>

      (test (op false?) (reg val))
      (branch (label false-branch4))
    true-branch3
      (assign val (const 1))
      (goto (reg continue))
    false-branch4

      ; <changed>
      ; We skip another call, which saves a save/restore of proc and argl and
      ; another bunch of instruction
      (save continue)
      (save env) ; Saving env happens here, instead of when entering the procedure
      (assign proc (op lookup-variable-value) (const factorial) (reg env))
      (assign arg1 (op lookup-variable-value) (const n) (reg env))
      (assign arg2 (const 1))
      (assign val (op -) (reg arg1) (reg arg2))
      ; </changed>

      (assign argl (op list) (reg val))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch6))
    compiled-branch7
      (assign continue (label proc-return9)) ; Different return label
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))

    proc-return9
      ; This is different, since we store result in arg1, not val.
      (assign arg1 (reg val))
      (goto (label after-call8))

    primitive-branch6
      (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call8

      ; <changed>
      ; We save another call, including a save/restore of argl.
      (restore env)
      (restore continue)
      (assign arg2 (op lookup-variable-value) (const n) (reg env))
      (assign val (op *) (reg arg1) (reg arg2))
      ; </changed>

      (goto (reg continue))
    after-if5
    after-lambda2
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))))

(define factorial-code
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(pretty-print (compiled-instructions factorial-code))
