; SICP exercise 4.40
;
; In the multiple dwelling problem, how many sets of assignments are there of
; people to floors, both before and after the requirement that floor
; assignments be distinct? It is very inefficient to generate all possible
; assignments of people to floors and then leave it to backtracking to
; eliminate them. For example, most of the restrictions depend on only one or
; two of the person-floor variables, and can thus be imposed before floors
; have been selected for all the people. Write and demonstrate a much more
; efficient nondeterministic procedure that solves this problem based upon
; generating only those possibilities that are not already ruled out by
; previous restrictions. (Hint: This will require a nest of let expressions).

#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")


; In the first case, there are 5⁵ lists passed to distinct?, which is
; incredibly slow.
;
; Below you will find a faster version when. The comparison is:
;
;   slow-multiple-dwelling: cpu time: 1226 real time: 1241 gc time: 59
;   fast-multiple-dwelling: cpu time: 40 real time: 40 gc time: 2
;
; ...which is way better than the previous exercise.

(define solution
  '((define (distinct? items)
      (cond ((null? items) true)
            ((null? (cdr items)) true)
            ((member (car items) (cdr items)) false)
            (else (distinct? (cdr items)))))
    (define (fast-multiple-dwelling)
      (let ((cooper (amb 1 2 3 4 5)))
        (require (not (= cooper 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((fletcher (amb 1 2 3 4 5)))
            (require (not (= fletcher 1)))
            (require (not (= fletcher 1)))
            (require (not (= (abs (- fletcher cooper)) 1)))
            (let ((smith (amb 1 2 3 4 5)))
              (require (not (= (abs (- smith fletcher)) 1)))
              (let ((baker (amb 1 2 3 4 5)))
                (require (not (= baker 5)))
                (require (distinct? (list baker cooper fletcher miller smith)))
                (list (list 'baker baker)
                      (list 'cooper cooper)
                      (list 'fletcher fletcher)
                      (list 'miller miller)
                      (list 'smith smith))))))))
    (define (slow-multiple-dwelling)
      (let ((baker (amb 1 2 3 4 5))
            (cooper (amb 1 2 3 4 5))
            (fletcher (amb 1 2 3 4 5))
            (miller (amb 1 2 3 4 5))
            (smith (amb 1 2 3 4 5)))
        (require
          (distinct? (list baker cooper fletcher miller smith)))
        (require (not (= baker 5)))
        (require (not (= cooper 1)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (> miller cooper))
        (require (not (= (abs (- smith fletcher)) 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))))

(define (time-procedure name)
  (define (times n proc)
    (if (= n 0)
        'done
        (begin (proc)
               (times (- n 1) proc))))
  (printf "~a: " name)
  (time
    (times 100
           (lambda ()
             (ambeval (list name)
                      solution-environment
                      (lambda (val fail) 'ok)
                      (lambda () 'ok))))))

(define solution-environment
  ((lambda ()
     (define environment (setup-environment))
     (for-each (lambda (definition)
                 (ambeval definition
                          environment
                          (lambda (value fail) 'ok)
                          (lambda () 'ok)))
               solution)
     environment)))

(time-procedure 'slow-multiple-dwelling)
(time-procedure 'fast-multiple-dwelling)
