; SICP exercise 3.25
;
; Generalizing one- and two- dimensional tables, show how to implement a table
; in which values are stored under an arbitrary number of keys and different
; values may be stored under different numbers of keys. The lookup and insert!
; procedures should take as input a list of keys used to access the table.

; I fairly uncertain why we're not using list as key. In good spirit, however,
; I'm going to let that pass and implement nested tables.
;
#lang racket
(require rackunit rackunit/text-ui)
(require r5rs/init)

(define (make-keyed-table key)
  (list key))

(define (make-table)
  (make-keyed-table '*table))

(define (find-pair key table)
  (define (search records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (search (cdr records)))))
  (search (cdr table)))

(define (lookup keys table)
  (let* ((first-key (car keys))
         (rest-keys (cdr keys))
         (record (find-pair first-key table)))
    (cond ((not record) #f)
          ((null? rest-keys) (cdr record))
          (else (lookup rest-keys record)))))

(define (insert! keys value table)
  (define (prepend-pair! pair)
    (set-cdr! table (cons pair (cdr table))))
  (let* ((first-key (car keys))
         (rest-keys (cdr keys))
         (pair (find-pair first-key table)))
    (cond ((and pair (null? rest-keys))
           (set-cdr! pair value))
          (pair
           (insert! rest-keys value pair))
          ((null? rest-keys)
           (prepend-pair! (cons first-key value)))
          (else
           (let ((new-table (make-keyed-table first-key)))
             (prepend-pair! new-table)
             (insert! rest-keys value new-table))))
    table))

(define sicp-3.25-tests
  (test-suite
    "Tests for SICP exercise 3.25"

    (test-begin "lookup"
      (check-equal? (lookup '(inexistant) (make-table)) #f))

    (test-begin "insert! (simple)"
      (define table (make-table))

      (insert! '(1) 'one table)
      (insert! '(2) 'two table)

      (check-equal? (lookup '(1) table) 'one)
      (check-equal? (lookup '(2) table) 'two))

    (test-begin "insert! (two levels)"
      (define table (make-table))

      (insert! '(1 1) 'eleven table)
      (insert! '(1 2) 'twelve table)
      (insert! '(2 1) 'twenty-one table)
      (insert! '(2 2) 'twenty-two table)

      (check-equal? (lookup '(1 1) table) 'eleven)
      (check-equal? (lookup '(1 2) table) 'twelve)
      (check-equal? (lookup '(2 1) table) 'twenty-one)
      (check-equal? (lookup '(2 2) table) 'twenty-two))

    (test-begin "insert! (mixed levels)"
      (define table (make-table))

      (insert! '(1) 'one table)
      (insert! '(2 1) 'two-one table)
      (insert! '(2 2) 'two-two table)
      (insert! '(3 1 1) 'three-one-one table)
      (insert! '(3 1 2) 'three-one-two table)
      (insert! '(3 2 1) 'three-two-one table)
      (insert! '(3 2 2) 'three-two-two table)

      (check-equal? (lookup '(1) table) 'one)
      (check-equal? (lookup '(2 1) table) 'two-one)
      (check-equal? (lookup '(2 2) table) 'two-two)
      (check-equal? (lookup '(3 1 1) table) 'three-one-one)
      (check-equal? (lookup '(3 1 2) table) 'three-one-two)
      (check-equal? (lookup '(3 2 1) table) 'three-two-one)
      (check-equal? (lookup '(3 2 2) table) 'three-two-two))
))

(run-tests sicp-3.25-tests)

;; test
(define t (make-table))

(insert! '('letters 'a) 97 t)
(insert! '('letters 'b) 98 t)
(insert! '('math '+) 43 t)
(insert! '('math '-) 45 t)
(insert! '('math '*) 42 t)

(lookup '('letters 'a) t)
(lookup '('letters 'b) t)
(lookup '('math '+) t)(insert! '('letters 'a) 97 t)
(insert! '('letters 'b) 98 t)
(insert! '('math '+) 43 t)
(insert! '('math '-) 45 t)
(insert! '('math '*) 42 t)
(lookup '('math '-) t)
(lookup '('math '*) t)
