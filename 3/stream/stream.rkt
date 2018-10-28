#lang racket
(provide (all-defined-out))

;; stream function name of SICP
(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-take s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (stream-cdr s) (- n 1)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (define (display-line x) (newline) (display x))
  (stream-for-each display-line s))

(define (list->infinite-stream list)
  (define (next items)
    (if (null? items)
        (list->infinite-stream list)
        (stream-cons (car items) (next (cdr items)))))
  (next list))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

;; s1 must be finite
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

;; s1, s2 can be infinite
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))




