#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "evaluator.rkt")

(define (run exp)
  (actual-value exp (setup-environment)))

(define (to-s exp)
  (with-output-to-string (lambda () (run `(begin (define result ,exp)
                                                 (print result))))))

(define evaluator-tests
  (test-suite
    "Tests for the lazy evaluator"

    (test-suite "Lazy evaluation"
      (check-equal? 1 (run '(begin (define (try a b)
                                     (if (= a 0) 1 b))
                                   (try 0 (/ 1 0))))))

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

(define sicp-4.33-tests
  (test-suite
    "Tests for SICP exercise 4.33"

    (check-true (run '(null? '())))
    (check-true (run '(null? (cdr '(a)))))
    (check-true (run '(null? (car '(() a)))))
    (check-equal? 'a (run '(car '(a b c))))
    (check-equal? 'c (run '(car (cdr (car (cdr '(a (b c))))))))
    (check-equal? '() (run '(car '(() a))))
    (check-equal? '() (run '(cdr (cdr '(() a)))))
))

(define sicp-4.34-tests
  (test-suite
    "Tests for SICP exercise 4.34"

    (check-equal? (to-s ''()) "()")
    (check-equal? (to-s ''(() a)) "(() a)")
    (check-equal? (to-s ''(a b c)) "(a b c)")
    (check-equal? (to-s ''(a (b c) d)) "(a (b c) d)")
    (check-equal? (to-s ''(a . b)) "(a . b)")
    (check-equal? (to-s '(begin (define pair (cons 'a pair))
                                pair))
                  "(a (...))")
    (check-equal? (to-s '(begin (define pair (cons 'a (cons pair 'b)))
                                pair))
                  "(a (...) . b)" )
    (check-equal? (to-s '(begin (define pair (cons 'a (cons pair (cons 'b '()))))
                                pair))
                  "(a (...) b)")
))

;(run-tests evaluator-tests)
(run-tests sicp-4.33-tests)
(run-tests sicp-4.34-tests)

;; Note:
;; The evaluator-tests will failed, because the return value of run is an internal lazy structure.
;; For example:
;; The return value of (run '((lambda (a b) (cons a b)) 1 2)) is not '(1 . 2),
;; so (check-equal? (run '((lambda (a b) (cons a b)) 1 2)) '(1 . 2)) failed
;; 
;; You can only use following code to get 1 and 2
;; (define a (run '(cdr ((lambda (a b) (cons a b)) 1 2)))) ; 1
;; (define b (run '(cdr ((lambda (a b) (cons a b)) 1 2)))) ; 2
;;
;; For print this internal lazy structure, see "../basic-stream-as-lazy-list-print"
;;
;; Another potential problem may be the racket<->scheme cons mcons compatible.
