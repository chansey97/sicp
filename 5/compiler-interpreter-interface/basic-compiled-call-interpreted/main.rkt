#lang racket
(require "./helpers.rkt")

(compile-and-go '(define (do-factorial)
                   (factorial 5)))

;; ;;; EC-Eval value:
;; ok

;; ;;; EC-Eval input:
;; (define (factorial n)
;;                    (if (= n 1)
;;                        1
;;                        (* (factorial (- n 1)) n)))

;; ;;; EC-Eval value:
;; ok

;; ;;; EC-Eval input:
;; (do-factorial)

;; ;;; EC-Eval value:
;; 120

;; ;;; EC-Eval input:
