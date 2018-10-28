; SICP exercise 5.50
;
; Use the compiler to compile the metacircular evaluator of section 4.1 and
; run this program using the register-machine simulator. (To compile more than
; one definition at a time, you can package the definitions in a begin.) The
; resulting interpreter will run very slowly because of the multiple levels of
; interpretation, but getting all the details to work is an instructive
; exercise.

; Oh boy. This is going to be so much fun!
;
; r5rs makes a return. It is required in the tests. I can probably work it out
; of this, but it will be too much work.
;
; Running the tests of the evaluator results in a whooping amount of 215 524
; instructions, 8036 pushes and a maximum stack depth of 22. If that is not
; cool, I don't know what is.
;
; Let's start with using the open-coding compiler, just because it's cooler
; that way.

;; Note:
;; I use racket instead of r5rs/init in https://github.com/skanev/playground/blob/master/scheme/sicp/05/50.scm
;; "primitive-procedures" in compiler/operations.rkt use mcons, mcar, mcdr, compatibility/mlist ...,
;; since evaluator support its own set-car! set-cdr!

#lang racket
(require rackunit rackunit/text-ui)
(require compatibility/mlist)
(require "./simulator/simulator.rkt")
(require "./compiler/operations.rkt")
(require "./compiler-interpreter-interface/explicit-evaluator-text.rkt")
(require "./compiler-interpreter-interface/helpers.rkt")
(require "./metacircular/evaluator.rkt")

; This is our machine:

(define machine
  (make-machine
   cm-registers
   cm-operations
   ec-controller-text))

; Finally, let's compile the evaluator in it.

(compile-in-machine machine metacircular-evaluator)

(define (pair->mpair-recursively pair)
  (if (not (pair? pair))
      pair
      (mcons (pair->mpair-recursively (car pair)) (pair->mpair-recursively (cdr pair)))))

(define (run exp)
  (let ((exp (if (pair? exp)
                 (pair->mpair-recursively exp)
                 exp)))
    (compile-in-machine machine `(evaluate ',exp the-global-environment))
    (get-register-contents machine 'val)))


(define sicp-5.50-tests
  (test-suite
    "Tests for SICP exercise 5.50"

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
                    1)
      )

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
      (check-equal? (run '((lambda (a b) (cons a b)) 1 2)) (pair->mpair-recursively '(1 . 2)))
      (check-equal? (run '(begin (define a 1)
                                 (define b 2)
                                 ((lambda (a) (cons a b)) 3)))
                    (pair->mpair-recursively '(3 . 2)))
      )

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
                    (pair->mpair-recursively '(1 . 2)))
      (check-equal? (run '(begin (define a 1)
                                 (define (pair b) (cons a b))
                                 (pair 2)))
                    (pair->mpair-recursively '(1 . 2))))

    (test-suite "Defining append"
      (check-equal? (run '(begin (define (append x y)
                                   (if (null? x)
                                       y
                                       (cons (car x)
                                             (append (cdr x) y))))
                                 (append '(a b c) '(d e f))))
                    (pair->mpair-recursively '(a b c d e f))))

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

    ;; ; Those are just some sanity checks. It is not necessary that it takes
    ;; ; that many instructions and stack operations to run the tests, but makes
    ;; ; sure that if I did not short-circuit something when this number changes.

    (check-equal? (machine 'instruction-count) 215523)
    (check-equal? ((machine 'stack) 'statistics)
                  '(total-pushes = 8036 maximum-depth = 22))

    ; The numbers get way cooler when we do a lame recursive version of
    ; Fibonacci 10.

    (test-case "The tenth Fibonacci number"
      ((machine 'stack) 'initialize)

      (check-equal? (run '(begin (define (fib n)
                                   (if (< n 2)
                                       1
                                       (+ (fib (- n 1)) (fib (- n 2)))))
                                 (fib 10)))
                    89)

      (check-equal? (machine 'instruction-count) 1561470)
      (check-equal? ((machine 'stack) 'statistics)
                    '(total-pushes = 190611 maximum-depth = 76)))
))

;; (define (print-instructions inst)
;;   (println inst))

;; (machine 'trace-on)
;; ((machine 'install-trace-proc) print-instructions)

(run-tests sicp-5.50-tests)

