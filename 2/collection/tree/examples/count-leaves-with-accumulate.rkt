; SICP exercise 2.35
;
; Redefine count-leaves from section 2.2.2 as an accumulation:
;
; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))

#lang racket
(require rackunit rackunit/text-ui)
(require "../../list/list.rkt")

;; A pattern:
;; Call map in some function, and map's op-lambda recursive call the function.
(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x) (count-leaves x) 1))
                   tree)))

(define sicp-2.35-tests
  (test-suite
    "Tests for SICP exercise 2.35"

    (check-equal? (count-leaves '()) 0)
    (check-equal? (count-leaves '(1 2 3 4)) 4)
    (check-equal? (count-leaves '(1 2 (3 4 (5 6) 7) 8 (9))) 9)
))

(run-tests sicp-2.35-tests)

(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)

(list x x)
(length (list x x))
(count-leaves (list x x))
