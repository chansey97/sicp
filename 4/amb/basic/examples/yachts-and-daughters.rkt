; SICP exercise 4.43
;
; Use the amb evaluator to solve the following puzzle:
;
;   Mary Ann Moore's father has a yacht and so has each of his four friends.
;   Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker. Each of the
;   five also has one daughter and each has named his yacht after a daughter
;   of one of the others. Sir Barnacle's yacht is the Gabrielle, Mr. Moore
;   owns the Lorna; Mr. Hall the Rosalind. The Melissa, owned by Colonel
;   Downing, is named after Sir Barnacle's daughter. Gabrielle's father owns
;   the yacht that is named after Dr. Parker's daughter. Who is Lorna's
;   father?
;
; Try to write the program so that it runs efficiently (see exercise 4.40).
; Also determine how many solutions are there if we are not told that Mary
; Ann's last name is Moore.

#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")

; The solution to the puzzle is Colonel Downing.
;
; If we omit the fact that Mary Ann's last name is Moore, then there would be
; two solutions, the second of which is Dr. Parker.

(define solution
  '((define (map proc items)
      (if (null? items)
          '()
          (cons (proc (car items))
                (map proc (cdr items)))))
    (define (distinct? items)
      (cond ((null? items) true)
            ((null? (cdr items)) true)
            ((member (car items) (cdr items)) false)
            (else (distinct? (cdr items)))))
    (define (xor a b)
      (if a (not b) b))
    (define (yachts-and-daughters mary-ann-is-moore)
      (define (names) (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa))
      (define (daughter pair) (car pair))
      (define (yacht pair) (car (cdr pair)))
      (define (father-of girl fathers)
        (cond ((null? fathers) (error))
              ((eq? (daughter (car fathers)) girl) (car fathers))
              (else (father-of girl (cdr fathers)))))
      (define (daughter-and-yacht)
        (let ((daughter (names))
              (yacht (names)))
          (require (not (eq? daughter yacht)))
          (list daughter yacht)))
      (define (name-of-father daughter results)
        (if (eq? (car (cdr (car results))) daughter)
            (car (car results))
            (name-of-father daughter (cdr results))))
      (let ((moore (daughter-and-yacht)))
        (if mary-ann-is-moore
            (require (eq? (daughter moore) 'mary-ann))
            'ok)
        (require (eq? (yacht moore) 'lorna))
        (let ((barnacle (daughter-and-yacht)))
          (require (eq? (yacht barnacle) 'gabrielle))
          (require (eq? (daughter barnacle) 'melissa))
          (let ((hall (daughter-and-yacht)))
            (require (eq? (yacht hall) 'rosalind))
            (let ((downing (daughter-and-yacht)))
              (require (eq? (yacht downing) 'melissa))
              (let ((parker (daughter-and-yacht)))
                (let ((fathers (list moore barnacle hall downing parker)))
                  (require (distinct? (map yacht fathers)))
                  (require (distinct? (map daughter fathers)))
                  (require (eq? (daughter parker)
                                (yacht (father-of 'gabrielle fathers))))
                  (name-of-father 'lorna
                                  (list (cons 'moore moore)
                                        (cons 'barnacle barnacle)
                                        (cons 'hall hall)
                                        (cons 'downing downing)
                                        (cons 'parker parker))))))))))
    ))

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

(define sicp-4.43-tests
  (test-suite
   "Tests for SICP exercise 4.43"

   (check-equal? (all-values '(yachts-and-daughters true))
                 '(downing))
   (check-equal? (all-values '(yachts-and-daughters false))
                 '(parker downing))
   ))

(run-tests sicp-4.43-tests)
