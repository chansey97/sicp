#lang racket
(require "./generic-operations.rkt")

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define n1 (make-scheme-number 10))
;; (real-part n1)

(define rat1 (make-rational 1 2))
;; (real-part rat1)

(define complex1 (make-complex-from-real-imag 10 20))
(real-part complex1)
(imag-part complex1)
(magnitude complex1)
