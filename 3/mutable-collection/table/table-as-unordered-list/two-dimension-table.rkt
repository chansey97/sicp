#lang racket
(require r5rs/init)

(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

; test
(define t (make-table))

(insert! 'letters 'a 97 t)
(insert! 'letters 'b 98 t)
(insert! 'math '+ 43 t)
(insert! 'math '- 45 t)
(insert! 'math '* 42 t)

(lookup 'letters 'a t)
(lookup 'letters 'b t)
(lookup 'math '+ t)
(lookup 'math '- t)
(lookup 'math '* t)
