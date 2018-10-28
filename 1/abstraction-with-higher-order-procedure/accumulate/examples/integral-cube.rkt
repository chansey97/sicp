#lang racket
(require "../integral.rkt")

(define (cube x) (* x x x ))
(integral cube 0 1 0.001)
