#lang racket
(require r5rs/init)

(define (make-table)
  (let ((table '(*table*)))
    (define (find-pair key)
      (define (search remaining)
        (cond ((null? remaining) #f)
              ((equal? key (caar remaining)) (car remaining))
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

;; test
(define table (make-table))
(insert! 1 'one table)
(insert! 2 'two table)
(lookup 1 table)
(lookup 2 table)
