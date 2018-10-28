; SICP exercise 5.04
;
; Specify register machines that implement each of the following procedures.
; For each machine, write a controller instruction sequence and draw a diagram
; showing the data paths.
;
; a. Recursive exponentiation
;
; (define (expt b n)
;   (if (= n 0)
;       1
;       (* b (expt b (- n 1)))))
;
; b. Iterative exponentiation
;
; (define (expt b n)
;   (define (expt-iter counter product)
;     (if (= counter 0)
;         product
;         (expt-iter (- counter 1) (* b product))))
;   (expt-iter n 1))

; a. This is the data path diagram:
;
;                       +-+                       +---------+
;      +--->(  =  )<---/ 0 \                      |  stack  |
;      |              +-----+                     +---------+
;   +-----+     +-+           +-----+   +-----+     |     ^
;   |  n  |    / 1 \---(x)--->| val |   |  b  |    (x)    |
;   +-----+   +-----+         +-----+   +-----+     |    (x)
;    ^   |     |               ^   |     |          V     |
;    |  +-------+              |  +-------+     +------------+
;    |   \  -  /               |   \  *  /      |  continue  |----> controller
;   (x)   +---+               (x)   +---+       +------------+
;    |      |                  |      |           ^        ^
;    +------+                  +------+           |        |
;                                                (x)      (x)
;                                                 |        |
;                                        +---------+      +----------+
;                                       / expt-done \    / after-expt \
;                                      +-------------+  +--------------+

#lang racket
(require rackunit rackunit/text-ui)
(require "../simulator.rkt")

(define recursive-expt-machine
  (make-machine
    '(b n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '(
        (assign continue (label expt-done))
      expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-expt))
        (goto (label expt-loop))
      after-expt
        (assign val (op *) (reg val) (reg b))
        (restore continue)
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      expt-done)))

; b. This is the iterative data path. Notice that it is simpler.
;
;                     +-+
;      +-->(  =  )<--/ 0 \
;      |            +-----+
;      |
;   +-----+   +-+         +-----+ +-----+
;   |  n  |  / 1 \--(x)-->| val | |  b  |
;   +-----+ +-----+       +-----+ +-----+
;    ^   |     |           ^   |     |
;    |  +-------+          |  +-------+
;    |   \  -  /           |   \  *  /
;   (x)   +---+           (x)   +---+
;    |      |              |      |
;    +------+              +------+

(define iterative-expt-machine
  (make-machine
    '(b n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '(
        (assign val (const 1))
      expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label expt-done))
        (assign val (op *) (reg val) (reg b))
        (assign n (op -) (reg n) (const 1))
        (goto (label expt-loop))
        expt-done)))

(define sicp-5.04-tests
  (test-suite
    "Tests for SICP exercise 5.04"

    (test-case "(recursive-expt 2 8)"
      (set-register-contents! recursive-expt-machine 'b 2)
      (set-register-contents! recursive-expt-machine 'n 8)
      (start recursive-expt-machine)

      (check-eq? (get-register-contents recursive-expt-machine 'val)
                256))

    (test-case "(iterative-expt 2 8)"
      (set-register-contents! iterative-expt-machine 'b 2)
      (set-register-contents! iterative-expt-machine 'n 8)
      (start iterative-expt-machine)

      (check-eq? (get-register-contents iterative-expt-machine 'val)
                256))
))

(run-tests sicp-5.04-tests)


