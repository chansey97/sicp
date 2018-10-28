#lang racket
(require "../helpers.rkt")

(define simple-if '(if true 1 0))

(pretty-print (compiled-instructions simple-if))
