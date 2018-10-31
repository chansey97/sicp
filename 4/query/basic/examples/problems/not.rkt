;; https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/7b-metacircular-evaluator-part-2/ 52:42
#lang racket
(require "../../evaluator.rkt")
(require "../../helpers.rkt")

(ns-initialize (module->namespace "../../evaluator.rkt"))

;; Comparing logic language to classical logic language:

;; ALL HUMANS ARE MORTAL
;; ALL GREEKS ARE HUMAN
;; SOCRATES IS A GREEK
;;—————————————————————————
;; SOCRATES IS MORTAL

;; Compare this to our logic language:

(add-to-data-base! '(

                     (Greek Socrates)
                     (Greek Plato)
                     (Greek Zeus)
                     (god Zeus)
                     
                    ;; All humans are mortal and fallible
                     (rule (mortal ?x)
                           (human ?x))
                     
                     (rule (fallible ?x)
                           (human ?x))
                     
                     ;; All non-god greeks are human
                     (rule (human ?x)
                           (and (Greek ?x) (not (god ?x))))
                     
                     ;; Address of any god is Mount Olympus
                     (rule (address ?x Olympus)
                           (and (Greek ?x) (god ?x)))

                     (rule (perfect ?x)
                           (and (not (mortal ?x))
                                (and (not (fallible ?x)))))
                     ))

;; NOT acted as a filter.

(matches-of '(and (address ?x ?y) (perfect ?x))) ; -----> this will give us Olympus
;; If you think about what's going on here, I'll build this query box where the output of an address piece gets fed into a perfect piece.
;; What will happen is the address piece will set up some things of everyone whose address I know.
;; Those will get filtered by the NOTs inside perfect here.

(matches-of '(and (perfect ?x) (address ?x ?y))) ; ------> this will give us nothing
;; I set this up, started up with an empty frame.
;; The perfect in here doesn't find anything for the NOTs to filter, so nothing comes out here at all.
;; And there's sort of nothing there that gets fed into the address thing. So here, I don't get an answer.
;; The reason for that is NOT isn't generating anything.

;; -------------------------------------------------------------------------------------------------------------------
;; However, there's a more profound problem, which is which one of these is the right answer?
;; Is it Mount Olympus or is it nothing?
;; So you might say it's Mount Olympus, because after all, Zeus is in that database, and Zeus was neither mortal nor fallible.

;; So you might say Zeus wants to satisfy NOT mortal Zeus or NOT fallible Zeus:
(matches-of '(not (mortal zeus))) ; ------> '((not (mortal zeus))) yes
;; But let's actually look at that database.
;; There's no way how does it know that Zeus is not fallible.
;; What's in there is that humans are fallible.
;; How does it know that Zeus is not mortal?
;; There's nothing in there about that.
;; The only way I can deduce something's mortal is if it's human, and that's all it really knows about mortal.

;; It's not the NOT of logic.
;; What NOT needs in this language is not deducible from things in the database as opposed to not true.

;; So if you ask it is it not true that Zeus likes chocolate ice cream?
;; It will say sure, it's not true.
;; (This "not true" means can not deduce from database, it can't list all the thing which like chocolate ice cream.)

;; From a logical point of view, first of all, it doesn't really makes sense.
;; Because if I don't know anything about x, I'm willing to say not x.
;; But am I willing to say not not x?
;; So (not (not x)) is not necessarily the same as x

(matches-of '(mortal ?x)) ; <------ '((mortal Socrates) (mortal Plato))
(matches-of '(not (mortal ?x))) ; <------ '(), because can't deduce from database
(matches-of '(not (not (mortal ?x)))) ; '((not (not (mortal ?x))))

;; give instantiated data
(matches-of '(mortal Socrates))
(matches-of '(not (mortal Socrates)))
(matches-of '(not (not (mortal Socrates))))
(matches-of '(mortal zeus))
(matches-of '(not (mortal zeus))) ; <------ '((not (mortal zeus))), compare with (matches-of '(not (mortal ?x))) above
(matches-of '(not (not (mortal zeus))))

;; Question:
;; The problem here is you have the definition of something, but you don't have the definition of its opposite.
;; If you include in the database something that says something implies mortal x, something else implies not mortal x, haven't you basically solved the problem?
;;
;; Answer:
;; There are two issues:
;; 1. in a big system, it turns out that might not be a finite number of things.
;; 2. it might be that's not what you want. a good example is about "connectivity".

;; But there are a lot of approaches that explicitly put in NOTs and reason based on that.
;; So it's a very good idea indeed.
;; It's just that then it starts becoming a little cumbersome.
