; SICP exercise 2.32
;
; We can represent a set as a list of distinct elements, and we can represent
; the set of all subsets of the set as a list of lists. For example, if the set
; is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2)
; (1 2 3)). Complete the following definition of a procedure that generates the
; set of subsets of a set and give a clear explanation of why it works:
; 
; (define (subsets s)
;   (if (null? s)
;       (list (list))
;       (let ((rest (subsets (cdr s))))
;         (append rest (map <??> rest)))))

#lang racket
(require rackunit rackunit/text-ui)

;; subs :: [a] -> [[a]]
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (subset) (cons (car s) subset))
                     rest)))))

; It works, because the subsets of a set are all the subsets that don't contain
; the first element plus all the subsets that do.

(define sicp-2.32-tests
  (test-suite
    "Tests for SICP exercise 2.32"

    (check-equal? (subsets '())
                  '(()))
    (check-equal? (subsets '(1))
                  '(() (1)))
    (check-equal? (subsets '(1 2))
                  '(() (2) (1) (1 2)))
    (check-equal? (subsets '(1 2 3))
                  '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
))

(run-tests sicp-2.32-tests)



;; (subsets '(1 2))
;; '(() (2) (1) (1 2))

;; (subsets '(2))
;; '(() (2))

;; (subsets '())
;; '(())
