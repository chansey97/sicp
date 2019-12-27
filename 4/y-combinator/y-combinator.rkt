;; Y-combinator
;; http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/7a-metacircular-evaluator-part-1/ 72:00

;; Now we're looking at a program for computing exponentials, apparently we're going to see that definitions are not necessary

;; (DEFINE EXPT
;;  (LAMBDA (X N)
;;   (COND 
;;    ((= N 0) 1)
;;    (ELSE 
;;     (* X (EXPT X (- N 1)))))))

;; Why does this self referential definition make any sense?

;; For example:
;; x + y = 3 ; this tells you x in terms of y     
;; x - y = 1 ; this tells you y in terms of x
;; They are self referential definition, but they have solution <x,y>

;; 2x + 2y = 6 }
;;             } infinite solutions
;; x + y = 3   }
;;             } unique solution in <x, y>
;; x - y = 1   }
;;             } no solutions
;; x - y = 2   }

;; These three sets of linear equations have different numbers of solutions. Number of solutions is not obvious by looking at the form but content.

;; We can't tell by looking the definition of EXPT how many solutions there are.

;; x = 3-y
;; y = x-1
;; this is linear transformation T
;; <x, y> = T<x, y>  ; 某个具体<x,y>代入T后，产生同一个<x, y>，解方程就是求这个<x,y>，也就是T的定点
;; The solution is a fixed point of T

;; We want to write down the function that corresponds to T whose function is the fixed point

;; F is function, when you take a parameter g, it return a function
;; 这里的F就相当于上面的T，而G就相当于<x, y>，只不过这里返回一个(int, int)->int，而上面返回一个<int ,int>，
;; 如果我找到了一个良好的指数过程g, 我就可以应用F到这个过程g上，返回的也是一个良好的指数过程。现在就是要想办法求这个g，这个g就是我们的目标EXPT
;; 这个良好的指数过程g本质就是F的定点，我们要求出这个定点（也就是方程的解）

;; F = (LAMBDA(G)
;;   (LAMBDA(X N)
;;    (COND
;;     ((= N 0) 1)
;;      (ELSE
;;       (* X
;;        (G X (- N 1)))))))

;; EXPT is a fixed point of F (what we want is to get fixed point of F)

;; Note:
;; F is a transformation from function to function (very like linear transformation T above),
;; we want to find fixed point that exactly actual exponential function (EXPT).

;; Try approach from Section 1 (solve approximation fixed point of sqrt/cosi/...)

;; The following E0 E1 E2 E3 is 尝试的输入g，我们希望用他不断迭代来逼近

;; E0 = ⊥ <guess an procedure, when you give any argument it produce a error. in fact, ⊥ can be anything at all> 

;; E1 = (LAMBDA(X N)
;;        (COND
;;         ((= N 0) 1)
;;          (ELSE
;;           (* X
;;            (E0 X (- N 1)))))))
;; will exponentiate to the zeroth power
 
;; E2 = (LAMBDA(X N)
;;        (COND
;;         ((= N 0) 1)
;;          (ELSE
;;           (* X
;;            (E1 X (- N 1)))))))
;; will exponentiate to the 0 or 1 power.

;; E3 = (LAMBDA(X N)
;;        (COND
;;         ((= N 0) 1)
;;          (ELSE
;;           (* X
;;            (E2 X (- N 1)))))))
;; will exponentiate to the 0, 1, or 2

;; ... 

;; Scott and Strachey prove:

;; EXPT =    LIMIT       Em
;;   N->infinity

;; EXPT = (F (F (F ... (F ⊥)...)))

;; Now we need infinite things

;; How to construct function's infinatle call??

;; The simplest construction is:
;; ((LAMBDA (X) (X X)) (LAMBDA (X) (X X)))

;; This is a simple infinite loop. 

;; Y is an operator. "Curry's paradoxical combinator"
;; Y = (LAMBDA (f)
;;  ((LAMBDA (X) (f (X X)))
;;  (LAMBDA (X) (f (X X)))))

;; (Y F) = ((LAMBDA (X) (F (X X))) ; the F is exacly the F above
;;          (LAMBDA (X) (F (X X))))
;;       = (F((LAMBDA (X) (F (X X))) (LAMBDA (x)(F (X X))))) 
;;       = (F((...)))              ; ((...)) is (Y F)

;; This leads F to infinite nesting, what is the most inside ((...)) is not important

;; (Y F) = (F (Y F)) ; obviously (Y F) is the fixed point of F

;; Y is a magic thing, when applied to some function F, it will produce the object which is the fixed point of that function F.

;; as the example above the fixed point of F is actual EXPT!

#lang racket
(provide (all-defined-out))

;; Note: y-combinator can not be used in eager language
(define y-combinator
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))
