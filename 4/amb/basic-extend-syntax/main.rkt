; A straightforward runner for the basic metacircular evaluator in chapter 4.
; Running it throws you in a REPL that's quite straightfoward, although it
; does not support good error reporting.
#lang racket
(require "evaluator.rkt")

(driver-loop)
