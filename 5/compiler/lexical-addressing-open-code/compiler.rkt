; SICP exercise 5.39
;
; Write a procedure lexical-address-lookup that implements the new lookup
; operation. It should take two arguments -- a lexical address and a run-time
; environment -- and return the value of the variable stored at the specified
; lexical address. lexical-address-lookup should signal an error if the value
; of the variable is the symbol *unassigned*. Also, write a procedure
; lexical-address-set! that implements the operation that changes the value of
; the variable at a specified lexical address.

; SICP exercise 5.40
;
; Modify the compiler to maintain the compile-time environment as described
; above. That is, add a compile-time-environment argument to compile and the
; various code-generators, and extend it in compile-lambda-body

; SICP exercise 5.42
;
; Using find-variable from exercise 5.41, rewrite compile-variable and
; compile-assignment to output lexical address instructions. In cases where
; find-variable returns not-found (that is, where the variable is not in the
; compile-time environment), you should have the code generators use the
; evaluator operations, as before, to search for the binding. (The only place
; a variable that is not found at compile time can be is the global
; environment, which is part of the run-time environment but is not part of
; the compile-time environment. Thus, if you wish, you may have the evaluator
; operations look directly in the global environment, which can be obtained
; with the operation (op get-global-environment), instead of having them
; search the whole run-time environment found in env.) Test the modified
; compiler of a few simple cases, such as the nested lambda combination at the
; beginning of this section.

; SICP exercise 5.43
;
; We argued in section 4.1.6 that internal definitions for block structure
; should not be considered "real" defines. Rather, a procedure body should be
; interpreted as if the internal variables being defined were installed as
; ordinary lambda variables initialized to their correct values using set!.
; Section 4.1.6 and exercise 4.16 showed how to modify the metacircular
; interpreter to accomplish this by scanning out internal definitions. Modify
; the compiler to perform the same transformations before it compiles a
; procedure body.

; SICP exercise 5.44
;
; In this section we have focused on the use of the compile-time environment
; to produce lexical addresses. But there are other uses for compile-time
; environments. For instance, in exercise 5.38 we increased the efficiency of
; compiled code by open-coding primitive procedures. Our implementation
; treated the names of open-coded procedures as reserved words. If a program
; were to rebind such a name, the mechanism described in exercise 5.38 would
; still open-code it as a primitive, ignoring the new binding. For example,
; consider the procedure
;
; (lambda (+ * a b x y)
;   (+ (* a x) (* b y)))
;
; which computes a linear combination of x and y. We might call it with
; arguments +matrix, *matrix, and four matrices, but the open-coded compiler
; would still open-code the + and the * in (+ (* a x) (* b y)) as primitive +
; and *. Modify the open-coding compiler to consult the compile-time
; environment in order to compile the correct code for expressions involving
; the names of primitive procedures. (The code will work correctly as long as
; the program does not define or set! these names.)
;
#lang racket
(provide statements compile-exp empty-compile-time-env)
(require "syntax.rkt")
(require "utils.rkt")

; Construction of the compile-time environment

(define (empty-compile-time-env) '())
(define (extend-compile-time-environment formals env) (cons formals env))

; The compile procedure (with compile-time environment)

(define (compile-exp exp target linkage env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage env))
        ((assignment? exp)
         (compile-assignment exp target linkage env))
        ((definition? exp)
         (compile-definition exp target linkage env))
        ((if? exp) (compile-if exp target linkage env))
        ((lambda? exp) (compile-lambda exp target linkage env))
        ((let? exp) (compile-exp (let->combination exp) target linkage env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage env))
        ((cond? exp) (compile-exp (cond->if exp) target linkage env))
        ((open-coded? exp env) (compile-open-coded exp target linkage env))
        ((application? exp)
         (compile-application exp target linkage env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

; Compiling linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

; Compiling simple expressions

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

;; Exercise 5.42
(define (compile-variable exp target linkage env)
  (let ((address (find-variable exp env)))
    ;; if not-found, lookup variable in global enviroment
    (if (eq? address 'not-found)
        (end-with-linkage linkage
         (make-instruction-sequence '(env) (list target 'env)
          `((assign env (op the-global-environment))
            (assign ,target (op lookup-variable-value) (const ,exp) (reg env)))))
        (end-with-linkage linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target (op lexical-address-lookup) (const ,address) (reg env))))))))

(define (compile-assignment exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code (compile-exp (assignment-value exp) 'val 'next env)))
    (let ((address (find-variable var env)))
      (if (eq? address 'not-found)
          (end-with-linkage linkage
           (preserving '(env)
            get-value-code
            (make-instruction-sequence '(env val) (list target 'env)
             `((assign env (op the-global-environment))
               (perform (op set-variable-value!) (const ,var) (reg val) (reg env))
               (assign ,target (const ok))))))
          (end-with-linkage linkage
           (preserving '(env)
            get-value-code
            (make-instruction-sequence '(env val) (list target)
             `((perform (op lexical-address-set!) (const ,address) (reg val) (reg env))
               (assign ,target (const ok))))))))))

;; Note: compile-definition dont't change compile-time enviroment
(define (compile-definition exp target linkage env)
  (let ((var (definition-variable exp))
        (get-value-code (compile-exp (definition-value exp) 'val 'next env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

; Compiling conditional expressions

(define (compile-if exp target linkage env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile-exp (if-predicate exp) 'val 'next env))
            (c-code (compile-exp (if-consequent exp) target consequent-linkage env))
            (a-code (compile-exp (if-alternative exp) target linkage env)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

; Compiling sequences

(define (compile-sequence seq target linkage env)
  (if (last-exp? seq)
      (compile-exp (first-exp seq) target linkage env)
      (preserving '(env continue)
       (compile-exp (first-exp seq) target 'next env)
       (compile-sequence (rest-exps seq) target linkage env))))

; Compiling lambda expressions

(define (compile-lambda exp target linkage env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
         (assign env (op compiled-procedure-env) (reg proc))
         (assign env
                 (op extend-environment)
                 (const ,formals)
                 (reg argl)
                 (reg env))))
     (compile-sequence (lambda-body exp)
                       'val
                       'return
                       (extend-compile-time-environment formals env)))))


; Compiling combinations

(define (compile-application exp target linkage env)
  (let ((proc-code (compile-exp (operator exp) 'proc 'next env))
        (operand-codes (map (lambda (operand) (compile-exp operand 'val 'next env))
                            (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage env)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg (preserving '(argl)
                                       (car operand-codes)
                                       (make-instruction-sequence '(val argl) '(argl)
                                        '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

; Applying procedures

(define (compile-procedure-call target linkage env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage env))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

; Applying compiled procedures

(define all-regs '(arg1 arg2 env proc val argl continue))

(define (compile-proc-appl target linkage env)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          `((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE-EXP" target))
        (else (error "How did we get here?"))))

; Compiling open-coded expressions

(define (compile-open-coded exp target linkage env)
  (when (and (not (vararg-open-coded-exp? exp))
           (not (= (length exp) 3)))
        (error "Expression should be binary" exp))
  (let ((code (car exp))
        (first-operand (cadr exp))
        (rest-operands (cddr exp)))
    (preserving '(env continue)
      (compile-exp first-operand 'arg1 'next env)
      (compile-open-coded-rest-args code rest-operands target linkage env))))

(define (compile-open-coded-rest-args code operands target linkage env)
  (if (null? (cdr operands))
      (preserving '(arg1 continue)
        (compile-exp (car operands) 'arg2 'next env)
        (end-with-linkage linkage
          (make-instruction-sequence '(arg1 arg2) (list target)
            `((assign ,target (op ,code) (reg arg1) (reg arg2))))))
      (preserving '(env continue)
        (preserving '(arg1)
          (compile-exp (car operands) 'arg2 'next env)
          (make-instruction-sequence '(arg1 arg2) '(arg1)
            `((assign arg1 (op ,code) (reg arg1) (reg arg2)))))
        (compile-open-coded-rest-args code (cdr operands) target linkage env))))

; Combining Instruction Sequences

(define (registers-needed s) (if (symbol? s) '() (car s)))
(define (registers-modified s) (if (symbol? s) '() (cadr s)))
(define (statements s) (if (symbol? s) (list s) (caddr s)))
(define (needs-register? seq reg) (memq reg (registers-needed seq)))
(define (modifies-register? seq reg) (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg) (registers-needed seq1))
              (list-difference (registers-modified seq1) (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence (registers-needed seq)
                             (registers-modified seq)
                             (append (statements seq)
                                     (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))

; Instruction sequences

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

; Make label

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

; Compiled procedure operations

(define (make-compiled-procedure entry env) (list 'compiled-procedure entry env))
(define (compiled-procedure? proc) (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

