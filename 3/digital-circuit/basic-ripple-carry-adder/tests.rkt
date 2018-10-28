#lang racket
(require sicp)
(require rackunit rackunit/text-ui)
(require "./digital-circuit.rkt")

(define sicp-3.28-tests
  (test-suite
    "Tests for SICP exercise 3.28"

    (test-case "half-adder"
      (define input-1 (make-wire))
      (define input-2 (make-wire))
      (define sum (make-wire))
      (define carry (make-wire))

      (with-output-to-string
        (lambda ()
          (probe 'sum sum)
          (probe 'carry carry)))

      (half-adder input-1 input-2 sum carry)

      (set-signal! input-1 1)
      (check-equal? "sum 8  New-value = 1\n"
                    (with-output-to-string propagate))

      (set-signal! input-2 1)
      (check-equal? (string-append "carry 11  New-value = 1\n"
                                   "sum 16  New-value = 0\n")
                    (with-output-to-string propagate)))

    (test-case "full-adder"
      (define input-1 (make-wire))
      (define input-2 (make-wire))
      (define carry-in (make-wire))
      (define sum (make-wire))
      (define carry-out (make-wire))

      (define (check-adder? a b c-in c-out s)
        (set-signal! input-1 a)
        (set-signal! input-2 b)
        (set-signal! carry-in c-in)
        (propagate)

        (check-equal? (list (get-signal sum) (get-signal carry-out))
                      (list s c-out)))

      (full-adder input-1 input-2 carry-in sum carry-out)

      (check-adder? 0 0 0 0 0)
      (check-adder? 0 1 0 0 1)
      (check-adder? 1 0 0 0 1)
      (check-adder? 1 1 0 1 0)
      (check-adder? 0 0 1 0 1)
      (check-adder? 0 1 1 1 0)
      (check-adder? 1 0 1 1 0)
      (check-adder? 1 1 1 1 1))
))

(define sicp-3.30-tests
  (test-suite
    "Tests for SICP exercise 3.30"

    (test-case "ripple-carry adder"
      (define addend1 (list (make-wire) (make-wire) (make-wire)))
      (define addend2 (list (make-wire) (make-wire) (make-wire)))
      (define sum (list (make-wire) (make-wire) (make-wire)))
      (define carry-in (make-wire))
      (define carry-out (make-wire))

      (define (digits n)
        (define (bits n r)
          (if (= r 0)
              '()
              (cons (remainder n 2)
                    (bits (quotient n 2) (- r 1)))))
        (bits n 3))

      (define (set-signals! wires signals)
        (if (null? signals)
            'done
            (begin (set-signal! (car wires) (car signals))
                   (set-signals! (cdr wires) (cdr signals)))))

      (define (check-adder a b s c-in c-out)
        (set-signals! addend1 (digits a))
        (set-signals! addend2 (digits b))
        (set-signal! carry-in c-in)

        (propagate)

        (check-equal? (map get-signal sum) (digits s))
        (check-equal? (get-signal carry-out) c-out))

      (ripple-carry-adder addend1 addend2 carry-in sum carry-out)

      (check-adder 0 0 0 0 0)
      (check-adder 1 2 3 0 0)
      (check-adder 2 4 6 0 0)
      (check-adder 0 0 1 1 0)
      (check-adder 3 3 7 1 0)
      (check-adder 3 4 0 1 1))


    ))

(run-tests sicp-3.28-tests)
(run-tests sicp-3.30-tests)
