; SICP exercise 2.14
;
; Demonstrate that Lem is right. Investigate the behavior of the system on a
; variety of arithmetic expressions. Make some intervals A and B, and use them
; in computing the expressions A/A and A/B. You will get the most insight by
; using intervals whose width is a small percentage of the center value.
; Examine the results of the computation in center-percent form (see exercise
; 2.12).

; SICP exercise 2.15
;
; Eva Lu Ator, another user, has also noticed the different intervals computed
; by different but algebraically equivalent expressions. She says that a
; formula to compute with intervals using Alyssa's system will produce tighter
; error bounds if it can be written in such a form that no variable that
; represents an uncertain number is repeated. Thus, she says, par2 is a
; "better" program for parallel resistances than par1. Is she right? Why?

; She is definitelly right.
;
; It is easy to see from the results of exercise 2.14 that the more we perform
; operations with uncertain quantities, the bigger error we get. It is
; important to note, that this applies mainly to multiplication and division.
; 2A is exactly the same as AA.

; SICP exercise 2.16
;
; Explain, in general, why equivalent algebraic expressions may lead to
; different answers. Can you devise an interval-arithmetic package that does
; not have this shortcoming, or is this task impossible? (Warning: This problem
; is very difficult.)

; After doing exercise 2.14, it is very easy to see that multiplying and
; dividing uncertain quantities increases the uncertainty. AA/A is equivalent
; to A, but the error margin is three times bigger. The less we use an
; uncertain quantity in an operation, the smaller tolerance we get.
;
; As for addressing the shortcoming, the only way I can figure out is to
; have the program simplify the expression as much as possible before
; calculating it. I assume that this requires some serious mathematical
; and computer science foundations, that I currently lack.
;
; One idea is to calculate this the expressions lazily. We can collect a tree
; that represents the expression, until the result needs to be calculated. At
; that point, we can simplify the expression and return the result. This won't
; reduce the error margin, but may provide a nicer API.

#lang racket
(require "../interval-arithmetic.rkt")

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

;; (define int1 (make-center-width 2 0.05)) 
;; (define int2 (make-center-width 4 0.05)) 

;; (center (par1 int1 int2)) ;;1.3349541539316476
;; (percent (par1 int1 int2)) ;;5.412113643459231

;; (center (par2 int1 int2)) ;;1.3332870241733814
;; (percent (par2 int1 int2)) ;;2.083463582369477

;; so par2 is better


(define (display-interval leading-text interval)
  (display leading-text)
  (display ": center = ")
  (display (center interval))
  (display ", percent = ")
  (display (percent interval))
  (newline))

; Here are the A and B intervals

(define A (make-interval  99.9 100.1))
(define B (make-interval 199.9 200.1))
(define one (make-interval 1.0 1.0))
(define parallel-resistance (/ 1.0 
                               (+ (/ 1.0 (center A)) 
                                  (/ 1.0 (center B)))))

(display-interval "A" A)
(display-interval "B" B)

(display-interval "par1" (par1 A B))
(display-interval "par2" (par2 A B))

(display "The parallel resistance of the two is ")
(display parallel-resistance)
(newline)

(newline)

; So far we have the following output
; 
; A:    center = 100.0,             percent = 0.09999999999999432
; B:    center = 200.0,             percent = 0.04999999999999716
; par1: center = 66.66679629635391, percent = 0.2166663750004245
; par2: center = 66.66666296296133, percent = 0.08333334166667185
;
; The parallel resistance of the two is 66.66666666666667
;
; We can see that par2 has smaller width and more accurate center. Let's check
; out A/A and A/B

(display-interval "A/A" (div-interval A A))
(display-interval "A/B" (div-interval A B))
(display-interval "A+A" (add-interval A A))

(newline)

; This time we get:
;
; A/A: center = 1.000002000002,     percent = 0.19999980000019435
; A/B: center = 0.5000003750000938, percent = 0.14999992500003134
; A+A: center = 200.0,              percent = 0.09999999999999432
; 
; We see that addition preserves the tolerance in percentage, but
; multiplication and division add them together. Let's take a look at the
; parts of par1 and par2

(display "Let's do par1 first:\n")
(display-interval "AB" (mul-interval A B))
(display-interval "A + B" (add-interval A B))
(display-interval "AB/(A + B)" (div-interval (mul-interval A B)
                                             (add-interval A B)))
(newline)

(display "Now par2:\n")
(display-interval "1/A" (div-interval one A))
(display-interval "1/B" (div-interval one B))
(display-interval "1/A + 1/B" (add-interval (div-interval one A)
                                            (div-interval one B)))
(display-interval "1/(1/A + 1/B)" (par2 A B))
(newline)

; This is the output:
;
; Let's do par1 first:
; AB:            center = 20000.010000000002,    percent = 0.14999992500002837
; A + B:         center = 300.0,                 percent = 0.06666666666666288
; AB/(A + B):    center = 66.66679629635391,     percent = 0.2166663750004245
; 
; Now par2:
; 1/A:           center = 0.010000010000009999,  percent = 0.09999999999999962
; 1/B:           center = 0.0050000012500003126, percent = 0.04999999999999587
; 1/A + 1/B:     center = 0.015000011250010312,  percent = 0.08333334166666921
; 1/(1/A + 1/B): center = 66.66666296296133,     percent = 0.08333334166667185
;
; We can see that we loose precision on every multiplication and division - the
; tolerance in percentage is of both factors is added together. Generally,
; addition decreases the tolerance in percentage whe adding positive numbers
; (that's not entirely true).
;
; In par2 we just do one addition, which decreases the tolerance under 0.01%,
; while in par1 we first do a multiplication and then a division, that gets the
; tolerance up to 0.21%.
;
; And just for a final illustration:

(display-interval "A*A/A" (div-interval (mul-interval A A)
                                        A))

; This resuls to:
;
; A*A/A: center = 100.00040000039999, percent = 0.29999920000237845
;
; The real answer he is A, but the arithmetic gymnastics triple the tolerance.
