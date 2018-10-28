; SICP exercise 4.42
;
; Solve the following "Liars" puzzle (from Phillips 1934):
;
;   Five schoolgirls sat for an examination. Their parents - so they thought -
;   showed an undue degree of interest in the result. They therefore agreed
;   that, in writing home about the examination, each girl should make one
;   true statement and one untrue one. The following are the relevant passages
;   from their letters:
;
;    * Betty: Kitty was second in the examination. I was only third.
;    * Ethel: You'll be glad to hear I was on top. Joan was second.
;    * Joan: I was third, and poor old Ethel was bottom.
;    * Kitty: I came out second. Mary was only fourth.
;    * Mary: I was fourth. Top place was taked by Betty.
;
;   What in fact was the order in which the five girls were placed?

#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")

; The solution to the puzzle is:
;
;   ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
;
; The code that discovers it is below.

(define solution
  '((define (distinct? items)
      (cond ((null? items) true)
            ((null? (cdr items)) true)
            ((member (car items) (cdr items)) false)
            (else (distinct? (cdr items)))))
    (define (xor a b)
      (if a (not b) b))
    (define (lairs)
      (let ((betty (amb 1 2 3 4 5))
            (ethel (amb 1 2 3 4 5))
            (joan (amb 1 2 3 4 5))
            (kitty (amb 1 2 3 4 5))
            (mary (amb 1 2 3 4 5)))
        (require (xor (= kitty 2) (= betty 3)))
        (require (xor (= ethel 1) (= joan 2)))
        (require (xor (= joan 3) (= ethel 5)))
        (require (xor (= kitty 2) (= mary 4)))
        (require (xor (= mary 4) (= betty 1)))
        (require (distinct? (list betty ethel joan kitty mary)))
        (list (list 'betty betty)
              (list 'ethel ethel)
              (list 'joan joan)
              (list 'kitty kitty)
              (list 'mary mary))))))

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

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

(define sicp-4.42-tests
  (test-suite
   "Tests for SICP exercise 4.42"

   (check-equal? (all-values '(lairs))
                 '(((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))))

   ))

(run-tests sicp-4.42-tests)
