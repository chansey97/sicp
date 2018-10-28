#lang racket
(require "./generic-operations.rkt")

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define n1 (make-scheme-number 10))
(define n2 (make-scheme-number 100))
(add n1 n2)

(define rat1 (make-rational 1 2))
(define rat2 (make-rational 3 5))
(add rat1 rat2)

(define complex1 (make-complex-from-real-imag 10 20))
(define complex2 (make-complex-from-real-imag -1 0))
(define complex2x (make-complex-from-mag-ang 1 3.13159))
(add complex1 complex2)
(add complex1 complex2x)
(mul complex1 complex2)
(mul complex1 complex2x)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;; coercion test
(put-coercion 'scheme-number 'complex scheme-number->complex)
(add n1 complex1)

