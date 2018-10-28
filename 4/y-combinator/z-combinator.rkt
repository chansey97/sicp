#lang racket
(provide (all-defined-out))

;; The Z combinator will work in strict languages (also called eager languages, where applicative evaluation order is applied).
;; Z combinator an eta-expansion of the Y combinator.

;; eta-expansion essentially an unnecessary function wrapping
;; (lambda (X) (M X)) === M

;; (x x) expansion to (lambda (v) ((x x) v))

;; Use lambda is better, because it can represent name are not necessary in recursion

;; (define (z-combinator f)
;;   ((lambda (x) (f (lambda (v) ((x x) v))))
;;    (lambda (x) (f (lambda (v) ((x x) v))))))

;; (define (z-combinator-2args f)
;;   ((lambda (x) (f (lambda (v u) ((x x) v u))))
;;    (lambda (x) (f (lambda (v u) ((x x) v u))))))

;; (define (z-combinator-0args f)
;;   ((lambda (x) (f (lambda () ((x x)))))
;;    (lambda (x) (f (lambda () ((x x)))))))

(define z-combinator
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

(define z-combinator-2args
  (lambda (f)
    ((lambda (x) (f (lambda (v u) ((x x) v u))))
     (lambda (x) (f (lambda (v u) ((x x) v u)))))))

(define z-combinator-0args
  (lambda (f)
    ((lambda (x) (f (lambda () ((x x)))))
     (lambda (x) (f (lambda () ((x x))))))))
