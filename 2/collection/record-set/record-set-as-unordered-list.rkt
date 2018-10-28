#lang racket
(provide (all-defined-out))

;; set which as unordered list, but item is record
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (key record) (car record))
(define (value record) (cadr record))

(module+ main

  (define record-set '(("id-1" "a") ("id-2" "b") ("id-3" "c")))
  (println record-set)
  (lookup "id-1" record-set)
  (lookup "id-2" record-set)
  (lookup "id-3" record-set)

  ;; Note: Don't use set operations.
  ;; In fact, set (as unordered tree) operation dependent a Equ typeclass.
  
  ;; (require "../set/set-as-unordered-list.rkt")
  ;; (define record-set1 '(("id-1" "a") ("id-2" "b") ("id-3" "c")))
  ;; (define record-set2 '(("id-3" "d") ("id-5" "e") ("id-6" "f")))
  ;; (define record-set3 (intersection-set record-set1 record-set2))
  ;; (println record-set3)
  ;; (lookup "id-1" record-set3) ; #f 
  ;; (lookup "id-3" record-set3) ; #f ("id-3" "c") is not equal? with ("id-3" "d")

  ;; (define record-set4 (union-set record-set1 record-set2))
  ;; (println record-set4) ; has duplicate key "id-3"
  ;; (lookup "id-3" record-set4) ;; find the first key = "id-3"
  )


