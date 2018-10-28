; The necessary code to compile the compiled interpreter and run the tests
; withn it
#lang racket
(require racket/runtime-path)
(require "./instruction-to-c.rkt")

(define-runtime-path base-path ".")

(define (relative-path path)
  (find-relative-path
    (current-directory)
    (simplify-path (build-path base-path path))))

(define compiled-code-path (relative-path "./c-evaluator/compiled_interpreter"))

(define (write-interpreter c-code)
  (with-output-to-file compiled-code-path
     (lambda () (display c-code))
     #:mode 'text
     #:exists 'truncate))

(write-interpreter interpreter-in-c)
