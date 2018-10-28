#lang racket
(require rackunit rackunit/text-ui)
(require "./compiler.rkt")
(require "./operations.rkt")
(require "./helpers.rkt")
(require "../../simulator/basic/simulator.rkt")

(define (run exp)
  (define compiled-code (compiled-instructions exp))
  (define machine (make-machine cm-registers cm-operations compiled-code))
  
  (set-register-contents! machine 'env the-global-environment)
  (start machine)
  (get-register-contents machine 'val)
  )

;; Exercise 5.39
(define (env mappings)
  (if (null? mappings)
      the-empty-environment
      (extend-environment (caar mappings)
                          (cadar mappings)
                          (env (cdr mappings)))))

(define sicp-5.39-tests
  (test-suite
   "Tests for SICP exercise 5.39"

   (check-equal? (lexical-address-lookup '(0 0) (env '(((a b) (1 2)))))
                 1)

   (check-equal? (lexical-address-lookup '(0 1) (env '(((a b) (1 2)))))
                 2)

   (check-equal? (lexical-address-lookup '(1 0) (env '(((a b) (1 2))
                                                       ((c d) (3 4)))))
                 3)

   (check-equal? (lexical-address-lookup '(1 1) (env '(((a b) (1 2))
                                                       ((c d) (3 4)))))
                 4)

   (check-equal? (lexical-address-lookup '(2 0) (env '(((a b) (1 2))
                                                       ((c d) (3 4))
                                                       ((e f) (5 6)))))
                 5)

   (check-equal? (lexical-address-lookup '(2 1) (env '(((a b) (1 2))
                                                       ((c d) (3 4))
                                                       ((e f) (5 6)))))
                 6)

   (check-exn exn? (lambda () (lexical-address-lookup '(0 0) (env '(((a) (*unassigned*)))))))

   (test-case "setting a lexical variable in the top frame"
     (let ((environment (env '(((a b) (1 2))))))
       (lexical-address-set! '(0 1) 3 environment)
       (check-equal? (lexical-address-lookup '(0 1) environment) 3)))

   (test-case "setting a lexical variable in the top frame"
     (let ((environment (env '(((a b) (1 2))
                               ((c d) (3 4))))))
       (lexical-address-set! '(1 1) 5 environment)
       (check-equal? (lexical-address-lookup '(1 1) environment) 5)))
   ;(test-case "setting a lexical variable in a frame, other than the top")
   ))

;; Exercise 5.40
(define sicp-5.40-tests
  (test-suite
   "Tests for SICP exercise 5.40"

   (test-suite "Self-evaluating expressions"
               (check-equal? (run '1) 1)
               (check-equal? (run '"something") "something"))

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
                             120)
               ) 
   ))

;; Exercise 5.41
(define sicp-5.41-tests
  (test-suite
   "Tests for SICP exercise 5.41"

   (check-equal? (find-variable 'z '((y z) (a b c d e) (x y)))
                 '(0 1))

   (check-equal? (find-variable 'c '((y z) (a b c d e) (x y)))
                 '(1 2))

   (check-equal? (find-variable 'x '((y z) (a b c d e) (x y)))
                 '(2 0))

   (check-equal? (find-variable 'w '((y z) (a b c d e) (x y)))
                 'not-found)

   (check-equal? (find-variable 'x '())
                 'not-found)
   ))

;; Exercise 5.42
(define sicp-5.42-tests
  (test-suite
   "Tests for SICP exercise 5.42"

   (check-equal? (run '(begin (define a 42)
                              ((lambda () a))))
                 42)

   (check-equal? (run '((lambda (a b) b) 1 2))
                 2)

   (let ((not-so-simple-expression '(((lambda (x y)
                                        (lambda (a b c d e)
                                          ((lambda (y z) (* x y z))
                                           (* a b x)
                                           (+ c d x))))
                                      3 4)
                                     5 6 7 8 9)))
     (check-equal? (run not-so-simple-expression) (eval not-so-simple-expression (make-base-namespace))))

   (check-equal? (run '(begin (define x 1)
                              (set! x 2)
                              x))
                 2)

   (check-equal? (run '(begin (define (foo x)
                                (set! x 2)
                                x)
                              (foo 1)))
                 2)

   (check-equal? (run '(begin ((lambda (a)
                                 ((lambda (b)
                                    (set! a b))
                                  2)
                                 a)
                               1)))
                 2)
   ))

;; Exercise 5.43
(define sicp-5.43-tests
  (test-suite
    "Tests for SICP exercise 5.43"

    (check-equal? (run '(begin (define (foo)
                                 (define x 1)
                                 (set! x 2)
                                 x)
                               (foo)))
                  2)
))

(run-tests sicp-5.39-tests)
(run-tests sicp-5.40-tests)
(run-tests sicp-5.41-tests)
(run-tests sicp-5.42-tests)
(run-tests sicp-5.43-tests)
