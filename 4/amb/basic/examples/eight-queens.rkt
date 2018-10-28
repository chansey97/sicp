; SICP exercise 4.44
;
; Exercise 2.42 described the "eight-queens puzzle" of placing queens on a
; chessboard so that no two attack each other. Write a nondeterministic
; program to solve this puzzle.

#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")

; The solution is simpler than the one in exercise 2.42. First, it does not
; have awkward requirements on representing queen positions. Second, the
; algorithm does not require map-ing, flatmap-ing and filtering like crazy.

(define solution
  '((define (map proc items)
      (if (null? items)
          '()
          (cons (proc (car items))
                (map proc (cdr items)))))
    (define (all? proc items)
      (cond ((null? items) true)
            ((proc (car items)) (all? proc (cdr items)))
            (else false)))
    (define (an-integer-between low high)
      (if (> low high)
        (amb)
        (amb low (an-integer-between (+ low 1) high))))
    (define (queens board-size)
      (define (safe? position other-queens)
        (let ((q1r (car position))
              (q1f (cadr position)))
          (all?
            (lambda (queen)
              (let ((q2r (car queen))
                    (q2f (cadr queen)))
                (cond ((= q1r q2r) false)
                      ((= q1f q2f) false)
                      ((= (+ q1r q1f) (+ q2r q2f)) false)
                      ((= (- q1r q1f) (- q2r q2f)) false)
                      (else true))))
            other-queens)))
      (define (place-queens rank queens)
        (if (> rank board-size)
            (reverse (map cadr queens))
            (let ((file (an-integer-between 1 board-size)))
              (let ((position (list rank file)))
                (require (safe? position queens))
              (place-queens (+ rank 1) (cons position queens))))))
      (place-queens 1 '()))
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

(define sicp-4.44-tests
  (test-suite
    "Tests for SICP exercise 4.44"

    (check-equal? (length (all-values '(queens 8)))
                  92)
    (check member '(4 2 8 5 7 1 3 6) (all-values '(queens 8)))
))

(run-tests sicp-4.44-tests)
