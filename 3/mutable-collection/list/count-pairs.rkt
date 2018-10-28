; SICP exercise 3.16
;
; Ben Bitdiddle decides to write a procedure to count the number of pairs in
; any list structure. "It's easy," he reasons. "The number of pairs in any
; structure is the number in the car plus the number in the cdr plus one more
; to count the current pair." So Ben writes the following procedure:
;
;   (define (count-pairs x)
;     (if (not (pair? x))
;         0
;         (+ (count-pairs (car x))
;            (count-pairs (cdr x))
;            1)))
;
; Show that this procedure is not correct. In particular, draw box-and-pointer
; diagrams representing list structures made up of exactly three pairs for
; which Ben's procedure would return 3; return 4; never return at all.

; Here are the diagrams:
;
; 3: +---+---+    +---+---+    +---+---+
;    | . | . -----| . | . -----| . | / |
;    +---+---+    +---+---+    +---+---+
;
; 4: +---+---+    +---+---+
;    | . | . -----| . | / |
;    +-|-+---+    +---+---+
;      |            |
;    +---+---+      |
;    | . | . -------+
;    +---+---+
;
; 7: +---+---+
;    | . | . |
;    +-|-+-|-+
;      |   |
;    +---+---+
;    | . | . |
;    +-|-+-|-+
;      |   |
;    +---+---+
;    | . | / |
;    +---+---+
;
; infinite:
;    +---+---+    +---+---+    +---+---+
;    | . | . -----| . | . -----| . | . |
;    +---+---+    +---+---+    +---+-|-+
;      |                             |
;      +-----------------------------+

; SICP exercise 3.17
;
; Devise a correct version of count-pairs procedure of exercise 3.16 that
; retuns the number of distinct pairs in any structure. (Hint: traverse the
; structure, maintaining an auxiliary data structure that is used to keep
; track of which pairs have already been counted.)

#lang racket
(require rackunit rackunit/text-ui)
(require r5rs/init)

;; only works for non-shared item list
(define (count-pairs-wrong x)
  (if (not (pair? x))
      0
      (+ (count-pairs-wrong (car x))
         (count-pairs-wrong (cdr x))
         1)))

(define (count-pairs x)
  (let ((counted '()))
    (define (count x)
      (cond ((not (pair? x)) 0)
            ((null? x) 0)
            ((memq x counted) 0)
            (else
             (set! counted (cons x counted))
             (+ 1
                (count (car x))
                (count (cdr x))))))
    (count x)))

(define a '(a))
(define b (cons 'b a))
(define c (cons a a))

(define three '(a b c))
(define four (cons b a))
(define seven (cons c c))

(define sicp-3.16-tests
  (test-suite
   "Tests for SICP exercise 3.16"

   (check-equal? (count-pairs-wrong three) 3)
   (check-equal? (count-pairs-wrong four) 4)
   (check-equal? (count-pairs-wrong seven) 7)
   ))

(run-tests sicp-3.16-tests)

(define sicp-3.17-tests
  (test-suite
   "Tests for SICP exercise 3.17"

   (check-equal? (count-pairs three) 3)
   (check-equal? (count-pairs four) 3)
   (check-equal? (count-pairs seven) 3)
   ))

(run-tests sicp-3.17-tests)
