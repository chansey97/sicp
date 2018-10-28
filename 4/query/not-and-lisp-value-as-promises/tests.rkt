#lang racket
(require rackunit rackunit/text-ui)
(require "evaluator.rkt")
(require "test-helpers.rkt")
(require "database.rkt")

(ns-initialize (module->namespace "evaluator.rkt"))

(define (stream . items)
  (if (null? items)
      empty-stream
      (stream-cons (car items) (apply stream (cdr items)))))

(define (env items)
  (if (null? items)
      '()
      (cons (cons (list '? (caar items))
                  (cadar items))
            (env (cdr items)))))

(define sicp-4.77-tests
  (test-suite
    "Tests for SICP exercise 4.77"

    (check-equal? (matches-of '(and (lisp-value > ?amount 30000)
                                    (salary ?person ?amount)))
                  '((and (lisp-value > 150000 30000)
                         (salary (Warbucks Oliver) 150000))
                    (and (lisp-value > 60000 30000)
                         (salary (Bitdiddle Ben) 60000))
                    (and (lisp-value > 40000 30000)
                         (salary (Hacker Alyssa P) 40000))
                    (and (lisp-value > 35000 30000)
                         (salary (Fect Cy D) 35000))
                    (and (lisp-value > 75000 30000)
                         (salary (Scrooge Eben) 75000))))

    (check-equal? (matches-of '(and (not (job ?x (computer programmer)))
                                    (supervisor ?x (Bitdiddle Ben))))
                  '((and (not (job (Tweakit Lem E) (computer programmer)))
                         (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))
))

(run-tests sicp-4.77-tests)
