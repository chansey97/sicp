#lang racket
(require "../../evaluator.rkt")
(require "../../helpers.rkt")

(unify-match '((? x) (? x)) '((a (? y) c) (a b (? z))) '())
;; '(((? z) . c) ((? y) . b) ((? x) a (? y) c))

;; 1. ?x = (a (? y) c)
;; 2. second ?x matched (a b (? z)), and check conflict with old ?x in frame 
;; 3. unify (a (? y) c) and (a b (? z))
;; 4. get ?y= b ?z=c
;; ...
;; Eventually, instantiate frame with query, we get
;; ?x (a b c)
;; ?y b
;; ?z c

;; a little bit more complicate example

(unify-match '((? x) (? x)) '(((? y) a (? w)) (b (? v) (? z))) '())
;; '(((? w) ? z) ((? v) . a) ((? y) . b) ((? x) (? y) a (? w)))

;; Eventually, instantiate frame with query, we get
;; ?y b
;; ?v a
;; ?w ?z
;; ?x (b a ?w)
;; Still have unconstrained variable, that is ok.

;; Abandon solving fixed point form

(unify-match '((? x) (? x)) '((? y) (a . (? y))) '())
;; 'failed

;; 1. ?x = ?y
;; 2. second ?x matched (a . (? y)), and check conflict with old ?x in frame 
;; 3. unify ?y and (a . (? y))
;; In order to do that unification, I have to solve the fixed-point equation (cons 'a y) = y 
;; And in general, I wrote a very simple one. Really doing unification might have to solve an arbitrary fixed-point equation: (f y) = y.
;; And basically, you can't do that and make the thing finite all the time.
;; So how does the logic language handle that? The answer is it doesn't.
;; 4. we find that (a . (? y)) depends-on ?y, cycle dependent, so we abandon it.

The question is:

;; Question:
;; Can you just by building the rules or writing the forms know in advance if you are going to be able to solve to get the unification or not?
;; Can you add some properties either to the rules itself or to the formula that you're writing so that you avoid the problem of not finding unification?
;;
;; Answer:
;; Watch your rules in the kinds of things that your writing (don't write some rule like circle dependent aboce)

