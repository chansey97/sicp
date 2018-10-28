; SICP exercise 2.33
;
; Fill in the missing expressions to complete the following definitions of some
; basic list-manipulation operations as accumulations:
;
; (define (map p sequence)
;   (accumulate (lambda (x y) <??>) nil sequence))
;
; (define (append seq1 seq2)
;   (accumulate cons <??> <??>))
;
; (define (length sequence)
;   (accumulate <??> 0 sequence))

#lang racket
(provide (all-defined-out))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(module+ main
  (require rackunit rackunit/text-ui)

  ;; (define (map proc items)
  ;;   (if (null? items)
  ;;       nil
  ;;       (cons (proc (car items))
  ;;             (map proc (cdr items)))))

  ;; Note: The evaluation order of proc is different between the above and below,
  ;; so the semantics of map is order-independent, but the reduce sequence is the same.

  (define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

  (define (append seq1 seq2)
    (accumulate cons seq2 seq1))

  (define (length sequence)
    (accumulate (lambda (_ result) (+ result 1)) 0 sequence))

  (define sicp-2.33-tests
    (test-suite
     "Tests for SICP exercise 2.33"

     (check-equal? (map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16))
     (check-equal? (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
     (check-equal? (length '(1 2 3)) 3)
     ))

  (run-tests sicp-2.33-tests)
  )
