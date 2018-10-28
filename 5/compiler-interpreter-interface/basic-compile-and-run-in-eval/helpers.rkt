#lang racket
(provide (all-defined-out))
(require "../../simulator/basic/simulator.rkt")
;; (require "../../simulator/basic-instruction-monitoring-with-label/simulator.rkt")
(require "./compiler.rkt")
(require "./operations.rkt")
(require "./explicit-evaluator-text.rkt")

(define (make-explicit+compile-machine)
  (make-machine ec-registers cm-operations ec-controller-text))

(define (make-explicit+compile-repl-machine)
  (make-machine ec-registers cm-operations ec-repl-controller-text))

(define (compile-in-machine machine expression)
  (let ((instructions (assemble (statements (compile-exp expression 'val 'return))
                                machine)))
    (set-register-contents! machine 'env the-global-environment)
    (set-register-contents! machine 'val instructions)
    (set-register-contents! machine 'flag true)
    ;; (machine 'trace-on)
    ;; ((machine 'install-trace-proc) print-instructions)
    (start machine)))

(define (eval-in-machine machine expression)
  (set-register-contents! machine 'env the-global-environment)
  (set-register-contents! machine 'exp expression)
  (set-register-contents! machine 'flag false)
  ;; (machine 'trace-on)
  ;; ((machine 'install-trace-proc) print-instructions)

  (start machine))

;; (define (print-instructions inst)
;;   (println inst))

(define (compiled-instructions expression)
  (statements (compile-exp expression 'val 'return)))

(define (compile-and-go expression)
  (define ec-repl-machine (make-explicit+compile-repl-machine))
  (initialize-machine ec-repl-machine)
  
  (compile-in-machine ec-repl-machine expression))

(define (go)
  (define ec-repl-machine (make-explicit+compile-repl-machine))
  (initialize-machine ec-repl-machine)
  
  (set-register-contents! ec-repl-machine 'env the-global-environment)
  (set-register-contents! ec-repl-machine 'flag false)
  (start ec-repl-machine))
