;; Pattern matching and rule based substitution
;; https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/4a-pattern-matching-and-rule-based-substitution/

;; In "../symbolic-differentiation", we translate the calculus rules into the language of the computer manually,
;; the better way is to describe the rules directly.

;; Build a language that will allow the computer to understand these rules. 
;; For example:

; dc
; ── = 0 for c a constant or a variable different from x
; dx

; dx
; ── = 1
; dx

; d(u+v)   du   dv
; ────── = ── + ──
;   dx     dx   dx

; d(uv)     dv    du
; ───── = u ── +v ──
;   dx      dx    dx

; d(uⁿ)         du
; ───── = nuⁿ⁻¹ ── 
;   dx          dx

;; All the calculus rules have a left hand and right hand side, the left hand side is pattern and the right hand side is skeleton.
;; Pattern is matched against an expression, application of rule produces a new expression that is an instantiation of a skeleton.
;; There is no fundamental difference from "../symbolic-differentiation", except that we now construct a language to describe the rules.

;; A Garbage-in, Garbage-out simplifier.

;; Uses ? to represent a pattern variable, which we use for matching
;; Uses : to represent substitution objects.

;; Pattern:
;; foo --- matches exactly foo
;; (f a b) --- match any list whose first element is f, 
;;             whose second element is a, and whose third element is b

;; (? x) --- matches anything, call it x
;; (?c x) --- matches a constant, call it x
;; (?v x) --- matches a variable, call it x

;; Skeletons for instantiation
;; foo  --- instantiates to itself
;; (f a b) --- instantiates to a 3.list which are the result of instantiating
;;         each of f, a , and b.
;; (: x) --- instantiate to the value of x as in the matched pattern

;; TODO: try to use this to do type-check and type-infer ?

#lang racket
(provide simplifier)

;; Building up a more complicated expression from a succession of simple parts

;; The key to good programming and design is knowing what not to think about. 
;; See calling simplify-exp, because I have specifications in my mind for what simplify-exp does, I don't have to know how it does it.
;; And it may, in fact, call scan somehow through try rules, which it does, and somehow, I've got another recursion going on here.
;; But since I know that simplify-exp is assumed by wishful thinking to prduce the simplified result, then I dont't have to think about it anymore.

;; (define (simplifier the-rules)
;;   (define (simplify-exp exp)
;;     (try-rules (if (compound? exp)
;;                    (simplify-parts exp)
;;                    exp)))
;;   (define (simplify-parts exp)
;;     (if (null? exp)
;;         '()
;;         (cons (simplify-exp (car exp))
;;               (simplify-parts (cdr exp)))))
;;   (define (try-rules exp)
;;     (define (scan rules)
;;       (if (null? rules)
;;           exp
;;           (let ((dict (match (pattern (car rules)) exp (empty-dictionary))))
;;             (if (eq? dict 'failed)
;;                 (scan (cdr rules))
;;                 (simplify-exp
;;                  (instantiate (skeleton (car rules))
;;                      dict))))))
;;     (scan the-rules))
;;   simplify-exp)

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (map simplify-exp exp)
                   exp)))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict (match (pattern (car rules)) exp (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                 (instantiate (skeleton (car rules)) dict exp (pattern (car rules))))))))
    (scan the-rules))
  simplify-exp)

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         ;; Atomic patterns
         (if (atom? exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        ;; Pattern variable clauses
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dictionary pat exp dict))
        
        ((atom? exp) 'failed)
        (else
         (match (cdr pat) (cdr exp)
                (match (car pat) (car exp) dict)))))

;; Purpose of the instantiator is to make expressions when given a dictionary and a skeleton.

(define (instantiate skeleton dict (dbg-exp '()) (dbg-pattern '()))
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evalution? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))

  (let ((result (loop skeleton)))
    (begin
      ;; (printf "rule: ~a -> ~a\n" dbg-pattern skeleton)
      ;; (printf "inst: ~a -> ~a   \n" dbg-exp result)
      result))
  )

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             (make-base-namespace))
       (map (lambda (v)
              (lookup v dict)) ;; mapcar in video course
            (cdr form)))))

;; Rule

(define (pattern rule)
  (car rule))

(define (skeleton rule)
  (cadr rule))

;; Pattern

(define (arbitrary-constant? pat)
  (and (pair? pat)
       (eq? (car pat) '?c)))


(define (arbitrary-variable? pat)
  (and (pair? pat)
       (eq? (car pat) '?v)))

(define (arbitrary-expression? pat)
  (and (pair? pat)
       (eq? (car pat) '?)))

(define (variable-name pat)
  (cadr pat))

;; Skeleton
(define (skeleton-evalution? s)
  (and (pair? s)
       (eq? (car s) ':)))

(define (eval-exp s)
  (cadr s))

;; Expression

(define (constant? exp)
  (number? exp))

(define (variable? exp)
  (symbol? exp))

(define (atom? exp)
  (not (pair? exp)))

(define (compound? exp)
  (pair? exp))

;; Dictionary

(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((eq? v #f) (cons (list name dat) dict))
            ((equal? (cadr v) dat) dict) ;; eq? in video course
            (else 'failed)))))

;; var used in skeleton instantiate evaluate, if not found return var
(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (eq? v #f) var (cadr v))))

(module+ main

  (define dict1 (match '((? op) (?c e1) (?c e2)) '(+ 1 2) (empty-dictionary)))
  (println dict1)
  (instantiate '(: (op e1 e2)) dict1)
  (instantiate '((: op) (: e2) (: e1)) dict1)

  (define dict2 '((e1 1) (e2 2) (e3 3)))
  (println dict2)
  (instantiate '(* (: e1) (* (: e2) (: e3))) dict2)
  
  (define dict3 (match '((? op) (? e1) (?c e2)) '(+ (* 2 x) 1) (empty-dictionary)))
  (println dict3)
  )
