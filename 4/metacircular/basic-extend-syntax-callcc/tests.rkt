#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "evaluator.rkt")

(define (run exp)
  (evaluate exp (setup-environment)))

(define evaluator-tests
  (test-suite
    "Tests for the metacircular evaluator"

    (test-suite "Self-evaluating expressions"
      (check-equal? (run '1) 1)
      (check-equal? (run '"something") "something"))

    (test-suite "Variables"
      (check-equal? (evaluate 'x (extend-environment '(x) '(1) the-empty-environment))
                    1)
      (check-exn exn? (lambda () (evaluate 'x the-empty-environment))))

    (test-suite "Quotation"
      (check-equal? (run '(quote foo)) 'foo))

    (test-suite "Begin"
      (check-equal? (run '(begin 1 2)) 2))

    (test-suite "Define"
      (check-equal? (run '(define x 1)) 'ok)
      (check-equal? (run '(begin (define x 1)
                                 x))
                    1)
      (check-equal? (run '(define (x) 1)) 'ok)
      (check-equal? (run '(begin (define (x) 1)
                                 (x)))
                    1))

    (test-suite "Set!"
      (check-equal? (run '(begin (define x 1)
                                 (set! x 2)))
                    'ok)
      (check-equal? (run '(begin (define x 1)
                                 (set! x 2)
                                 x))
                    2))

    (test-suite "If"
      (check-equal? (run '(if true 1 2)) 1)
      (check-equal? (run '(if false 1 2)) 2)
      (check-equal? (run '(if true 1)) 1)
      (check-equal? (run '(if false 1)) false))

    (test-suite "Lambda"
      (check-equal? (run '((lambda () 1))) 1)
      (check-equal? (run '((lambda (x) x) 1)) 1)
      (check-equal? (run '((lambda (a b) (cons a b)) 1 2)) '(1 . 2))
      (check-equal? (run '(begin (define a 1)
                                 (define b 2)
                                 ((lambda (a) (cons a b)) 3)))
                    '(3 . 2)))

    (test-suite "Cond"
      (check-equal? (run '(cond (true 1))) 1)
      (check-equal? (run '(cond (false 1) (true 2))) 2)
      (check-equal? (run '(cond (false 1) (else 2))) 2)
      (check-exn exn? (lambda () (run '(cond (else 1) (true 2))))))

    (test-suite "Procedure application"
      (check-equal? (run '(begin (define (a) 1)
                                 (a)))
                    1)
      (check-equal? (run '(begin (define (pair a b) (cons a b))
                                 (pair 1 2)))
                    '(1 . 2))
      (check-equal? (run '(begin (define a 1)
                                 (define (pair b) (cons a b))
                                 (pair 2)))
                    '(1 . 2)))

    (test-suite "Defining append"
      (check-equal? (run '(begin (define (append x y)
                                   (if (null? x)
                                       y
                                       (cons (car x)
                                             (append (cdr x) y))))
                                 (append '(a b c) '(d e f))))
                    '(a b c d e f)))

    (test-suite "Factorial"
      (check-equal? (run '(begin (define (factorial n)
                                   (if (= n 1)
                                       1
                                       (* n (factorial (- n 1)))))
                                 (factorial 5)))
                    120)
      (check-equal? (run '(begin (define (factorial n)
                                   (define (iter n result)
                                     (if (= n 0)
                                         result
                                         (iter (- n 1) (* result n))))
                                   (iter n 1))
                                 (factorial 5)))
                    120)) 
))

(define sicp-4.04-tests
  (test-suite
    "Tests for SICP exercise 4.04"

    (test-case "And with evaluation procedures"
      (set-logical-operations-implementation! 'syntax-procedures)
      (check-equal? (run '(and)) true)
      (check-equal? (run '(and false)) false)
      (check-equal? (run '(and false false)) false)
      (check-equal? (run '(and 1)) 1)
      (check-equal? (run '(and 1 2)) 2)
      (check-equal? (run '(and false (fail))) false)
      (check-equal? (run '(begin (define (x) 2)
                                 (and 1 (x))))
                    2))

    (test-case "And as a derived form"
      (set-logical-operations-implementation! 'derived-forms)
      (check-equal? (run '(and)) true)
      (check-equal? (run '(and false)) false)
      (check-equal? (run '(and false false)) false)
      (check-equal? (run '(and 1)) 1)
      (check-equal? (run '(and 1 2)) 2)
      (check-equal? (run '(and false (fail))) false)
      (check-equal? (run '(begin (define (x) 2)
                                 (and 1 (x))))
                    2))

    (test-case "Or with evaluation procedures"
      (set-logical-operations-implementation! 'syntax-procedures)
      (check-equal? (run '(or)) false)
      (check-equal? (run '(or false)) false)
      (check-equal? (run '(or false false)) false)
      (check-equal? (run '(or 1)) 1)
      (check-equal? (run '(or 1 2)) 1)
      (check-equal? (run '(or 1 (fail))) 1)
      (check-equal? (run '(or false 2)) 2)
      (check-equal? (run '(begin (define (x) false)
                                 (or (x) 1)))
                    1))

    (test-case "Or as a derived form"
      (set-logical-operations-implementation! 'derived-forms)
      (check-equal? (run '(or)) false)
      (check-equal? (run '(or false)) false)
      (check-equal? (run '(or false false)) false)
      (check-equal? (run '(or 1)) true)
      (check-equal? (run '(or 1 2)) true)
      (check-equal? (run '(or 1 (fail))) true)
      (check-equal? (run '(or false 2)) true)
      (check-equal? (run '(begin (define (x) false)
                                 (or (x) 1)))
                    true))
))

(define sicp-4.05-tests
  (test-suite
    "Tests for SICP exercise 4.05"

    (check-equal? (run '(begin (define (assoc key items)
                                 (cond ((null? items) false)
                                       ((eq? key (car (car items))) (car items))
                                       (else (assoc key (cdr items)))))
                               (define (cadr x) (car (cdr x)))
                               (cond ((assoc 'b '((a 1) (b 2))) => cadr)
                                     (else false))))
                  2)

    (check-equal? (run '(begin (define (assoc key items)
                                 (cond ((null? items) false)
                                       ((eq? key (car (car items))) (car items))
                                       (else (assoc key (cdr items)))))
                               (define (cadr x) (car (cdr x)))
                               (cond ((assoc 'c '((a 1) (b 2))) => cadr)
                                     (else 3))))
                  3)
))
  
(define sicp-4.06-tests
  (test-suite
    "Tests for SICP exercise 4.06"

    (check-equal? (run '(let ((x 1)) x)) 1)
    (check-equal? (run '(let ((a 1) (b 2)) (+ a b))) 3)
    (check-equal? (run '(let ((a 1) (b 2)) a b)) 2)
    (check-equal? (run '(begin (define a 1)
                               (let ((b 2)
                                     (c 3))
                                 (+ a b c))))
                  6)

    ))

(define sicp-4.07-tests
  (test-suite
    "Tests for SICP exercise 4.07"

    (check-equal? (run '(let* ((x 3)
                               (y (+ x 2))
                               (z (+ x y 5)))
                          (* x z)))
                  39)
))

(define sicp-4.08-tests
  (test-suite
    "Tests for SICP exercise 4.08"

    (check-equal? (run '(begin (define (fib n)
                                 (let fib-iter ((a 1)
                                                (b 0)
                                                (count n))
                                   (if (= count 0)
                                     b
                                     (fib-iter (+ a b) a (- count 1)))))
                               (fib 12)))
                  144)
))

(define sicp-4.09-tests
  (test-suite
    "Tests for SICP exercise 4.09"

    (test-suite "For"
      (check-equal? (run '(begin (define sum 0)
                                 (for x '(1 2 3 4 5)
                                      (set! sum (+ sum x)))
                                 sum))
                    15)
      (check-equal? (run '(begin (define sum 0)
                                 (for x '(1 2 3 4 5)
                                      (set! sum (+ sum x))
                                      (set! sum (+ sum x)))
                                 sum))
                    30))

    (test-suite "While"
      (check-equal? (run '(begin (define sum 0)
                                 (define n 1)
                                 (while (< n 10)
                                        (set! sum (+ sum n))
                                        (set! n (+ n 1)))
                                 sum))
                    45))

    (test-suite "Until"
      (check-equal? (run '(begin (define sum 0)
                                 (define n 1)
                                 (until (= n 10)
                                        (set! sum (+ sum n))
                                        (set! n (+ n 1)))
                                 sum))
                    45))
))

(define sicp-4.20-tests
  (test-suite
    "Tests for SICP exercise 4.20"

    (check-equal? (letrec->combination '(letrec ((a 1)
                                                 (b 2))
                                          (+ a b)))
                  '(let ((a '*unassigned*)
                         (b '*unassigned*))
                     (set! a 1)
                     (set! b 2)
                     (+ a b)))
    (check-equal? '(#t #f)
                  (run '(begin (define (f x)
                                 (letrec ((even?
                                            (lambda (n)
                                              (if (= n 0)
                                                true
                                                (odd? (- n 1)))))
                                          (odd?
                                            (lambda (n)
                                              (if (= n 0)
                                                false
                                                (even? (- n 1))))))
                                   (list (odd? x) (even? x))))
                               (f 3))))
    (check-equal? 3628800
                  (run '(letrec ((fact
                                   (lambda (n)
                                     (if (= n 1)
                                       1
                                       (* n (fact (- n 1)))))))
                          (fact 10))))
))

(run-tests evaluator-tests)
(run-tests sicp-4.04-tests)
(run-tests sicp-4.05-tests)
(run-tests sicp-4.06-tests)
(run-tests sicp-4.07-tests)
(run-tests sicp-4.08-tests)
(run-tests sicp-4.09-tests)
(run-tests sicp-4.20-tests)

;; https://stackoverflow.com/questions/63345691/is-there-a-sicp-metacircular-evaluator-that-implements-call-with-current-continu
(define stackoverflow-callcc-tests
  (test-suite
   "Tests for callcc"
   
  (check-equal? 6 (run '(+ 1 (call/cc (lambda (k) (* 10 (throw k 5)))))))
))

(run-tests stackoverflow-callcc-tests)

