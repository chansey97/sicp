; SICP exercise 3.24
;
; In the table implementations above, the keys are tested for equality using
; equal? (called by assoc). This is not always the appropriate test. For
; instance, we might have a table with numeric keys in which we don't need an
; exact match to the number we're looking up, but only a number within some
; tolerance of it. Design a table constructor make-table that takes as an
; argument a same-key? procedure that will be used to test "equality" of keys.
; make-table should return a dispatch procedure that can be used to access
; appropriate lookup and insert! procedures for a local table.

#lang racket
(require rackunit rackunit/text-ui)
(require r5rs/init)

(define (make-table same-key?)
  (let ((table '(*table*)))
    (define (find-pair key)
      (define (search remaining)
        (cond ((null? remaining) #f)
              ((same-key? key (caar remaining)) (car remaining))
              (else (search (cdr remaining)))))

      (search (cdr table)))

    (define (lookup key)
      (let ((pair (find-pair key)))
        (if pair
            (cdr pair)
            false)))

    (define (insert key value)
      (let ((pair (find-pair key)))
        (cond (pair (set-cdr! pair value))
              (else (set-cdr! table (cons (cons key value)
                                          (cdr table)))))
        dispatch))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert) insert)
            (else (error "Unrecognized message - TABLE" m))))
    dispatch))

(define (lookup key table) ((table 'lookup) key))
(define (insert! key value table) ((table 'insert) key value))

(define sicp-3.24-tests
  (test-suite
    "Tests for SICP exercise 3.24"

    (test-begin "lookup"
      (check-equal? (lookup 'inexistant (make-table equal?)) #f))

    (test-begin "insert!"
      (define table (make-table equal?))

      (insert! 1 'one table)
      (insert! 2 'two table)

      (check-equal? (lookup 1 table) 'one)
      (check-equal? (lookup 2 table) 'two))

    (test-begin "make-table with a lambda"
      (define table (make-table (lambda (a b) (= (remainder a 10) (remainder b 10)))))

      (insert! 11 'one table)

      (check-equal? (lookup 11 table) 'one)
      (check-equal? (lookup 21 table) 'one)
      (check-equal? (lookup 1 table) 'one))
))

(run-tests sicp-3.24-tests)
