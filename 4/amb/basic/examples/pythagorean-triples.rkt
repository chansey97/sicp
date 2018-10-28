; SICP exercise 4.35
;
; Write a procedure an-integer-between that returns an integer between two
; given bounds. This can be used to implement a procedure that finds
; Pythagorean triples, i.e., triples of integers (i, j, k) between the given
; bounds such that i ≤ j and i² + j² = k², as follows:
;
;   (define (a-ptyhagorean-triple-between low high)
;     (let ((i (an-integer-between low high)))
;       (let ((j (an-integer-between i high)))
;         (let ((k (an-integer-between j high)))
;           (require (= (+ (* i i) (* j j) (* k k))))
;           (list i j k)))))

; SICP exercise 4.37
;
; Ben Bitdiddle claims that the following method for generating Pythagorean
; triples is more efficient than the one in exercise 4.35. Is he correct?
; (Hint: Consider the number of possibilities that must be explored.)
;
;   (define (a-pythagorean-triple-between low high)
;     (let ((i (an-integer-between low high))
;           (hsq (* high high)))
;       (let ((j (an-integer-between i high)))
;         (let ((ksq (+ (* i i) (* j j))))
;           (require (>= hsq ksq))
;           (let ((k (sqrt ksq)))
;             (require (integer? k))
;             (list i j k))))))

; It seems so. Ben's version is n², while the text version is n³. On the other
; hand, Ben's version uses sqrt which is slower in general, so there might be
; cases where where Ben's version is slower (probably for small values of
; high - low).

#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")

;; Exercise 4.35
(define solution
  '((define (an-integer-between low high)
      (if (> low high)
          (amb)
          (amb low (an-integer-between (+ low 1) high))))
    (define (a-pythagorean-triple-between low high)
      (let ((i (an-integer-between low high)))
        (let ((j (an-integer-between i high)))
          (let ((k (an-integer-between j high)))
            (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k)))))))

;; Exercise 4.37 more efficient
;; (define solution
;;   '((define (an-integer-between low high)
;;       (if (> low high)
;;           (amb)
;;           (amb low (an-integer-between (+ low 1) high))))
;;     (define (a-pythagorean-triple-between low high)
;;       (let ((i (an-integer-between low high))
;;             (hsq (* high high)))
;;         (let ((j (an-integer-between i high)))
;;           (let ((ksq (+ (* i i) (* j j))))
;;             (require (>= hsq ksq))
;;             (let ((k (sqrt ksq)))
;;               (require (integer? k))
;;               (list i j k))))))))

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

(define sicp-4.35-tests
  (test-suite
   "Tests for SICP exercise 4.35"

   (check-equal? (all-values '(begin (a-pythagorean-triple-between 2 10)))
                 '((3 4 5) (6 8 10)))
   ))

(run-tests sicp-4.35-tests)
