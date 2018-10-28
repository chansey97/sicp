#lang racket
(require "../helpers.rkt")

(define simple-add '(+ a 1))
;; (define simple-add '(+ 1 2 3 4))

(pretty-print (compiled-instructions simple-add))
