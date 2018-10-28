#lang racket
(require "../helpers.rkt")

(define simple-add '(+ a 1))
(pretty-print (compiled-instructions simple-add))
