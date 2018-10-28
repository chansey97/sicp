; SICP exercise 5.22
;
; Exercise 3.12 of section 3.3.1 presented an append procedure that appends
; two lists to form a new list and an append! procedure that splices two lists
; together. Design a register machine to implement each of these procedures.
; Assume that the list-structure memory operations are available as primitive
; operations.

;   (define (append x y)
;     (if (null? x)
;         y
;         (cons (car x) (append (cdr x) y))))

#lang racket
(require rackunit rackunit/text-ui)
(require "../simulator.rkt")
(require "../memory.rkt")

(define append-machine
  (make-machine-with-memory '(x y val result continue)
                            '(
                                (assign continue (label append-done))
                              append
                                (test (op null?) (reg x))
                                (branch (label x-empty))
                                (assign val (op vector-ref) (reg the-cars) (reg x))
                                (perform (op vector-set!) (reg the-cars) (reg free) (reg val))
                                (assign val (reg free))
                                (save val)
                                (assign free (op +) (reg free) (const (p 1))) ; + is add-pointers in machine
                                (save continue)
                                (assign continue (label after-append))
                                (assign x (op vector-ref) (reg the-cdrs) (reg x))
                                (goto (label append))
                              after-append
                                (restore continue)
                                (restore val)
                                (perform (op vector-set!) (reg the-cdrs) (reg val) (reg result))
                                (assign result (reg val))
                                (goto (reg continue))
                              x-empty
                                (assign result (reg y))
                                (goto (reg continue))
                              append-done
                              )))

(define append!-machine
  (make-machine-with-memory '(x y val)
                            '(append!
                                (assign val (op vector-ref) (reg the-cdrs) (reg x))
                                (test (op null?) (reg val))
                                (branch (label last-pair))
                                (assign x (op vector-ref) (reg the-cdrs) (reg x))
                                (goto (label append!))
                              last-pair
                                (perform (op vector-set!) (reg the-cdrs) (reg x) (reg y))
                              )))

(define sicp-5.22-tests
  (test-suite
    "Tests for SICP exercise 5.22"

    (test-suite "append"
      (set-register-contents! append-machine 'x (allocate-list append-machine '(1 2 3)))
      (set-register-contents! append-machine 'y (allocate-list append-machine '(4 5 6)))

      (check-equal? (memory-dump append-machine)
                    '((n1 n2 n3 n4 n5 n6 __ __ __ __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 e0 p4 p5 e0 __ __ __ __ __ __ __ __ __ __ __ __ __ __)))

      (check-equal? (list-in-memory append-machine (get-register-contents append-machine 'x))
                    '(1 2 3))
      (check-equal? (list-in-memory append-machine (get-register-contents append-machine 'y))
                    '(4 5 6))

      (start append-machine)

      (check-equal? (list-in-memory append-machine
                                    (get-register-contents append-machine 'result))
                    '(1 2 3 4 5 6))

      (check-equal? (memory-dump append-machine)
                    '((n1 n2 n3 n4 n5 n6 n1 n2 n3 __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 e0 p4 p5 e0 p7 p8 p3 __ __ __ __ __ __ __ __ __ __ __))))

    (test-suite "append!"
      (set-register-contents! append!-machine 'x (allocate-list append!-machine '(1 2 3)))
      (set-register-contents! append!-machine 'y (allocate-list append!-machine '(4 5 6)))

      (check-equal? (memory-dump append!-machine)
                    '((n1 n2 n3 n4 n5 n6 __ __ __ __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 e0 p4 p5 e0 __ __ __ __ __ __ __ __ __ __ __ __ __ __)))

      (start append!-machine)

      (check-equal? (memory-dump append!-machine)
                    '((n1 n2 n3 n4 n5 n6 __ __ __ __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 p3 p4 p5 e0 __ __ __ __ __ __ __ __ __ __ __ __ __ __))))
))

(run-tests sicp-5.22-tests)
