; SICP exercise 4.04
;
; Recall the definitions of the special forms and and or from chapter 1:
;
; • and: The expressions are evaluated from left to right. If any expression
;   evaluates to false, false is returned; any remaining expressions are not
;   evaluated. If all the expressions evaluate to true values, the value of
;   the last expression is returned. If there are no expressions then true is
;   returned.
; • or: The expressions are evaluated from left to right. If any expression
;   evaluates to a true value, that value is returned; any remaining
;   expressions are not evaluated. If all expressions evaluate to false, or if
;   there are no expressions, then false is returned.
;
; Install and and or as new special forms for the evaluator by defining
; appropriate syntax procedures and evaluation procedures eval-and and
; eval-or. Alternatively, show how to implement and and or as derived
; expressions.

; We will implement it in a similar way to exercise 4.02 - there will be an
; option to flip which approach to use in order to be able to test both of
; them.
;
; We cannot reasonably implement or without having a let. And even then, we
; have the problem of shadowing the name of the value. So we will modify the
; request a bit - we return true, instead of the first non-false value. Then
; we have

; SICP exercise 4.05
;
; Scheme allows additional syntax for cond clauses, (<test> => <recipient>).
; If <test> evaluates to a true value, then <recipient> is evaluated. Its
; value must be a procedure of one argument; this procedure is then invoked on
; the value of the <test>, and the result is returned as the value of the cond
; expession. For example
;
;   (cond ((assoc 'b ((a 1) (b 2))) => cadr)
;         (else false))
;
; returns 2. Modify the handling of cond so that it supports this extended
; syntax.

; We need to rewrite cond not to be a derived form, because otherwise we
; either need to evaluate the test twice or there will be name shadowing in
; the clauses following the stabby one.

; SICP exercise 4.06
;
; Let expressions are derived expressions, because
;
;   (let ((<var₁> <exp₁>) ... (<varᵢ> <expᵢ>))
;     <body>)
;
; is equivalent to
;
;   ((lambda (<var₁> ... <varᵢ>)
;      <body>)
;    <exp₁>
;    ...
;    <expᵢ>)
;
; Implement a syntactic transformation let->combination that reduces
; evaluating let expressions to evaluating combinations of the type show
; above, and add the appropriate clause to eval to handle let expressions.

; SICP exercise 4.07
;
; Let* is similar to let, except that the bindings of the let* variables are
; performed sequentially from left to right, and each binding is made in an
; environment in which all of the preceding bindings are visible. For example
;
;   (let* ((x 3)
;          (y (+ x 2))
;          (z (+ x y 5)))
;     (* x z))
;
; returns 39. Explain how a let* expression can be rewritten as a set of
; nested let expressions and write a procedure let*->nested-lets that performs
; this transformation. If we have already implemented let (exercise 4.6) and
; we want to extend the evaluator to handle let*, is it sufficient to add a
; clause to eval whose action is
;
;   (eval (let*->nested-lets exp) env)
;
; or must we explicitly expand let* in terms of non-derived expressions?

; Simply, the expression above can be converted to:
;
;   (let ((x 3))
;     (let ((y (+ x 2)))
;       (let ((z (+ x y 5)))
;         (* x z))))
;
; You just create a new let statement for each bound name. And of course we
; can define let* in terms of let, which itself is a derived expression.

; SICP exercise 4.08
;
; "Named let" is a variant of let that has the form
;
;   (let <var> <bindings> <body>)
;
; The <bindings> and <body> are just as in ordinary let, except that var is
; bound within <body> to a procedure whose body is <body> and whose parameters
; are the variables in the <bindings>. Thus, one can repeatedly execute the
; <body> by invoking the procedure, named <var>. For example, the iterative
; Fibonacci procedure (section 1.2.2) can be rewritten using named let as
; follows:
;
;   (define (fib n)
;     (let fib-iter ((a 1)
;                    (b 0)
;                    (count n))
;       (if (= count 0)
;           b
;           (fib-iter (+ a b) a (- count 1)))))
;
; Modify let->combination of exercise 4.6 to also support named let.

; SICP exercise 4.09
;
; Many languages support a variety of iteration constructs, such as do, for,
; while and until. In scheme, iterative processes can be expressed in terms of
; ordinart procdure calls, so special iteration constructs provide no
; essential gain in computational power. On the other hand, such constructs
; are often convenient. Design some iteration constructs, give examples of
; their use, and show how to implement them as derived expressions.

; SICP exercise 4.20
;
; Because internal definitions look sequential but are actually simultaneous,
; some people prefer to avoid them entirely, and use the special form letrec
; instead. Letrec looks like let, so it is no surprising that the variables it
; binds are bound simultaneously and have the same scope as each other. The
; sample procedure f above can be written without internal definitions, but
; with exactly the same meaning, as:
;
;   (define (f x)
;     (letrec ((even?
;                (lambda (n)
;                  (if (= n 0)
;                      true
;                      (odd? (- n 1)))))
;              (odd?
;                (lambda (n)
;                  (if (= n 0)
;                      false
;                      (even? (- n 1))))))
;       <rest-of-body-of-f>))
;
; letrec expressions, which have the form
;
;   (letrec ((<var-1> <exp-1>) ... (<var-n> <exp-n>))
;     <body>)
;
; are a variation on let in which the expressions <exp-k> that provide the
; initial values for the variables <var-k> are evaluated in an environment
; that includes all the letrec bindings. This permits recursion in the
; bindings, such as the mutual recursion of even? and odd? in the example
; above, or the evaluation of 10 factorial with
;
;   (letrec ((fact
;              (lambda (n)
;                (if (= n 1)
;                    1
;                    (* n (fact (- n 1)))))))
;     (fact 10))
;
; a. Implement letrec as a derived expression, by transforming a letrec
; expression into a let expression as shown in the text above or in exercise
; 4.18. That is, the letrec variables should be created with a let and then be
; assigned their values with set!.
;
; b. Louis Reasoner is confused by all this fuss about internal definitions.
; The way he sees it, if you don't like to use define inside a procedure, you
; can just use let. Illustrate what is loose about his reasoning by drawing an
; environment diagram that shows the environment in which <rest-of-body-of-f>
; is evaluated during evaluation of the expression (f 5), with f defined as in
; this exercise. Draw an environment diagram for the same evaluation, but with
; let in place of letrec in the definition of f.

#lang racket
(provide (all-defined-out))
(require r5rs/init)

;; The Core of the Evaluator
(define (evaluate exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-cond exp env))
        ((let? exp) (evaluate (let->combination exp) env))
        ((let*? exp) (evaluate (let*->nested-lets exp) env))
        ((letrec? exp) (evaluate (letrec->combination exp) env))
        ((for? exp) (evaluate (for->lambda exp) env))
        ((while? exp) (evaluate (while->lambda exp) env))
        ((until? exp) (evaluate (until->lambda exp) env))
        ((application? exp)
         (apply-procedure (evaluate (operator exp) env)
                          (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type - EVALUATE" exp))))

(define (apply-procedure procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type - APPLY-PROCEDURE" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (evaluate (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (evaluate (if-predicate exp) env))
      (evaluate (if-consequent exp) env)
      (evaluate (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evaluate (first-exp exps) env))
        (else (evaluate (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (evaluate (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (evaluate (definition-value exp) env)
    env)
  'ok)

(define logical-operations-implementation 'none)
(define (set-logical-operations-implementation! type)
  (set! logical-operations-implementation type))

(define (and? exp) (tagged-list? exp 'and))
(define (and-terms exp) (cdr exp))
(define (or? exp) (tagged-list? exp 'or))
(define (or-terms exp) (cdr exp))

(define (eval-and exp env)
  (cond ((eq? logical-operations-implementation 'syntax-procedures)
         (eval-and-terms-procedures (and-terms exp) env))
        ((eq? logical-operations-implementation 'derived-forms)
         (eval-and-terms-derived (and-terms exp) env))
        (error "Unknown implementation" logical-operations-implementation)))

(define (eval-or exp env)
  (cond ((eq? logical-operations-implementation 'syntax-procedures)
         (eval-or-terms-procedures (or-terms exp) env))
        ((eq? logical-operations-implementation 'derived-forms)
         (eval-or-terms-derived (or-terms exp) env))
        (error "Unknown implementation" logical-operations-implementation)))

(define (eval-and-terms-procedures terms env)
  (cond ((empty-exp? terms) true)
        ((last-exp? terms) (evaluate (first-exp terms) env))
        ((evaluate (first-exp terms) env) (eval-and-terms-procedures (rest-exps terms) env))
        (else false)))

(define (eval-or-terms-procedures terms env)
  (if (empty-exp? terms)
      false
      (let ((value (evaluate (first-exp terms) env)))
        (if value
            value
            (eval-or-terms-procedures (rest-exps terms) env)))))

(define (eval-or-terms-derived terms env)
  (define (or->if terms)
    (if (empty-exp? terms)
        'false
        (make-if (first-exp terms)
                 'true
                 (or->if (rest-exps terms)))))
  (evaluate (or->if terms) env))

(define (eval-and-terms-derived terms env)
  (define (and->if terms)
    (cond ((empty-exp? terms) 'true)
          ((last-exp? terms) (first-exp terms))
          (else (make-if (first-exp terms)
                         (and->if (rest-exps terms))
                         'false))))
  (evaluate (and->if terms) env))

(define (eval-cond exp env)
  (define (eval-clauses clauses)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
          (if (not (null? rest))
              (error "ELSE clause is not the last - COND")
              (eval-sequence (cond-actions first) env))
          (let ((test-result (evaluate (cond-predicate first) env)))
            (cond ((not test-result)
                   (eval-clauses rest))
                  ((cond-stabby-clause? first)
                   (apply-procedure (evaluate (cond-stabby-recipient first) env)
                                    (list test-result)))
                  (else
                   (eval-sequence (cond-actions first) env)))))))
  (eval-clauses (cond-clauses exp)))

;; Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (empty-exp? seq) (null? seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-stabby-clause? clause)
  (eq? (car (cond-actions clause)) '=>))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-stabby-recipient clause) (caddr clause))

;; derived expressions

(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (if (symbol? (cadr exp))
      (let ((name (cadr exp))
            (names (map car (caddr exp)))
            (values (map cadr (caddr exp)))
            (body (cdddr exp)))
        (list
         (list
          'lambda
          null
          (append (list 'define (cons name names)) body)
          (cons name values))))
      (let ((names (map car (cadr exp)))
            (values (map cadr (cadr exp)))
            (body (cddr exp)))
        (cons (cons 'lambda (cons names body))
              values))))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define body (cddr exp))
  (define (one-binding? bindings) (null? (cdr bindings)))
  (define (convert bindings)
    (let ((new-binding (list (car bindings)))
          (rest (cdr bindings)))
      (if (one-binding? bindings)
          (cons 'let (cons new-binding body))
          (list 'let new-binding (convert rest)))))

  (convert (cadr exp)))

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-pairs exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec->combination exp)
  (cons 'let
        (cons (map (lambda (pair) (list (car pair) ''*unassigned*))
                   (letrec-pairs exp))
              (append (map (lambda (pair) (list 'set!
                                                (car pair)
                                                (cadr pair)))
                           (letrec-pairs exp))
                      (letrec-body exp)))))

; I am not sure what "do" is, so we will start with the for loop. This is
; tricky, since we need to do it with recursion, and in order to accomplish
; that, we need put a name in the environment. Of course, this will lead to
; name-shadowing conflicts. Since the text of the book ignores this issue, we
; shall do so too. We'll need to introduce two names in the environment -
; items and for-loop. Same goes for the other constructs, really. It might
; make sense to name them something obscure (like _for-loop and _while-loop),
; since this is not a real evaluator, we don't need to do it.

(define (for? exp)
  (tagged-list? exp 'for))

(define (for->lambda exp)
  (let ((name (cadr exp))
        (items (caddr exp))
        (body (cdddr exp)))
    (list
     (list
      'lambda
      (list)
      (list 'define (list 'for-loop 'items)
            (list 'if (list 'null? 'items)
                  ''done
                  (append (list 'begin
                                (list 'define name (list 'car 'items)))
                          body
                          (list (list 'for-loop (list 'cdr 'items))))))
      (list 'for-loop items)))))

(define (while? exp)
  (tagged-list? exp 'while))

;; while = "call a void lambda including an infinite loop function named while-loop and condition break"
;; Use y-combinator (z-combinator-0args) to solve name conflict

;; ((lambda ()
;;    (define (while-loop)
;;      (if condition
;;          (begin body (while-loop))
;;          'done))
;;    (while-loop)))

(define (while->lambda exp)
  (let ((condition (cadr exp))
        (body (cddr exp)))
    `((
       (lambda (f)
         ((lambda (x) (f (lambda () ((x x)))))
          (lambda (x) (f (lambda () ((x x)))))))
       
       (lambda (g)
         (lambda ()
           (if ,condition
               (begin ,(sequence->exp body) (g))
               'done)))
       ))))

; And now, let's implement until:

(define (until? exp)
  (tagged-list? exp 'until))

(define (until->lambda exp)
  (let ((condition (cadr exp))
        (body (cddr exp)))
    (list
     (list
      'lambda
      null
      (append '(define (until-loop))
              (list (list 'if condition
                          ''done
                          (sequence->exp (append body '((until-loop)))))))
      '(until-loop)))))

;; Evaluator Data Structures
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable - SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Running the Evaluator as a Program
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'eq? eq?)
        (list '< <)
        (list 'list list)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))


(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evaluate input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

