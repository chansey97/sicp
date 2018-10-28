; SICP exercise 5.28
;
; Modify the definition of the evaluator by changing eval-sequence as
; described in section 5.4.2 so that the evaluator is no longer
; tail-recursive. Rerun your experiments from exercise 5.26 to 5.27 to
; demonstrate that both versions of the factorial procedure now require space
; that grows linearly with their input.

; The results are:
;
; Iterative factorial:
;   1! takes (total-pushes = 70 maximum-depth = 17)
;   2! takes (total-pushes = 107 maximum-depth = 20)
;   3! takes (total-pushes = 144 maximum-depth = 23)
;   4! takes (total-pushes = 181 maximum-depth = 26)
;   5! takes (total-pushes = 218 maximum-depth = 29)
;   6! takes (total-pushes = 255 maximum-depth = 32)
;   7! takes (total-pushes = 292 maximum-depth = 35)
;   8! takes (total-pushes = 329 maximum-depth = 38)
;   9! takes (total-pushes = 366 maximum-depth = 41)
; Recursive factorial:
;   1! takes (total-pushes = 18 maximum-depth = 11)
;   2! takes (total-pushes = 52 maximum-depth = 19)
;   3! takes (total-pushes = 86 maximum-depth = 27)
;   4! takes (total-pushes = 120 maximum-depth = 35)
;   5! takes (total-pushes = 154 maximum-depth = 43)
;   6! takes (total-pushes = 188 maximum-depth = 51)
;   7! takes (total-pushes = 222 maximum-depth = 59)
;   8! takes (total-pushes = 256 maximum-depth = 67)
;   9! takes (total-pushes = 290 maximum-depth = 75)
;
; One can see that both versions are not bound on stack space. The code to
; reproduce the results is below:

#lang racket
(provide (all-defined-out))
(require "../../../simulator/basic-stack-statistics/simulator.rkt")
(require "../controller-text.rkt")
(require "../operations.rkt")

(define (make-explicit-control-machine)
  (make-machine ec-registers ec-operations ec-controller-text))

(define code
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
         product
         (iter (* counter product)
               (+ counter 1))))
     (iter 1 1)))

(define machine (make-explicit-control-machine))

(set-register-contents! machine 'env the-global-environment)

(set-register-contents! machine 'exp code)
(start machine)

(for ([n (in-range 1 10)])
  (printf "~a! takes ~a\n" n (stack-stats-for machine (list 'factorial n))))
