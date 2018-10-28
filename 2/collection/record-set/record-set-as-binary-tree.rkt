; SICP exercise 2.66
;
; Implement the lookup procedure for the case where the set of records is
; structured as a binary tree, ordered by the numerical values of the keys.

#lang racket
(provide (all-defined-out))
(require (only-in "../set/set-as-binary-tree.rkt"
                  entry
                  left-branch
                  right-branch
                  ))

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((= x (entry set)) true)
;;         ((< x (entry set))
;;          (element-of-set? x (left-branch set)))
;;         ((> x (entry set))
;;          (element-of-set? x (right-branch set)))))

;; like element-of-set?
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let* ((record (entry set-of-records))
             (record-key (key record)))
        (cond ((= given-key record-key) record)
              ((< given-key record-key) (lookup given-key (left-branch set-of-records)))
              ((> given-key record-key) (lookup given-key (right-branch set-of-records)))))))

(define (key record) (car record))
(define (value record) (cadr record))

(module+ main
  (require rackunit rackunit/text-ui)

  ;; Note: we can't use list->tree here, because lookup use record-key, but list->tree use whole entry to compare.
  ;; Don't use set operations as well.
  ;; In fact, set (as binary tree) operation dependent a Ord typeclass.

  ;; create binary tree manually
  (define a-tree
    '((99 a) ((50 b) ((25 c) ((12 d) () ())
                             ((42 e) ((30 f) () ())
                                     ()))
                     ((75 g) () ()))
             ()))

  (define (name-for number)
    (let ((record (lookup number a-tree)))
      (if record
          (value record)
          #f)))

  (define sicp-2.66-tests
    (test-suite
     "Tests for SICP exercise 2.66"

     (check-equal? (name-for 99) 'a)
     (check-equal? (name-for 50) 'b)
     (check-equal? (name-for 25) 'c)
     (check-equal? (name-for 12) 'd)
     (check-equal? (name-for 42) 'e)
     (check-equal? (name-for 30) 'f)
     (check-equal? (name-for 75) 'g)

     (check-equal? (name-for 20) #f)
     ))

  (run-tests sicp-2.66-tests)

  )


