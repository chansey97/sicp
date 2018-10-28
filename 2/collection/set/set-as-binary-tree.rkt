; SICP exercise 2.63
;
; Each of the following two procedures converts a binary tree to a list.
;
; (define (tree->list-1 tree)
;   (if (null? tree)
;       '()
;       (append (tree->list-1 (left-branch tree))
;               (cons (entry tree)
;                     (tree->list-1 (right-branch tree))))))
;
; (define (tree->list-2 tree)
;   (define (copy-to-list tree result-list)
;     (if (null? tree)
;         result-list
;         (copy-to-list (left-branch tree)
;                       (cons (entry tree)
;                             (copy-to-list (right-branch tree) result-list)))))
;   (copy-to-list tree '()))
;
; a. Do the two procedures produce the same result for every tree? If not, how
;    do the results differ? What lists do the two procedures produce for the
;    trees in Figure 2.16?
; b. Do the two procedures have the same order of growth in the number of steps
;    required to convert a balanced tree with n elements to a list? If not,
;    which one grows more slowly?

; a. Yes. They don't. All six variants generate (1 3 5 7 9 11)
;
; b. No. tree->list-2 tends to grow slower, both in space and time.
;
; First of all, it is recursive only on the right branches, but iterative on
; the left ones, and second, it does not invlove any calls to append (which is
; linear to the size of the first list). In all cases tree->list-2 finishes in
; Θ(n).

; SICP exercise 2.64
;
; The following procedure list->tree converts an ordered list to a balanced
; binary tree. The helper procedure partial-tree takes as arguments an integer n
; and a list of at least n elements and constructs a balanced tree containing
; the first n elements of the list. The result returned by partial-tree is a pair
; (formed with cons) whose car is the constructed tree and whose cdr is the list
; of elements not included in the tree.
;
; (define (list->tree elements)
;   (car (partial-tree elements (length elements))))
;
; (define (partial-tree elts n)
;   (if (= n 0)
;       (cons '() elts)
;       (let* ((left-size (quotient (- n 1) 2))
;              (left-result (partial-tree elts left-size))
;              (left-tree (car left-result))
;              (non-left-elts (cdr left-result))
;              (right-size (- n (+ left-size 1)))
;              (this-entry (car non-left-elts))
;              (right-result (partial-tree (cdr non-left-elts) right-size))
;              (right-tree (car right-result))
;              (remaining-elts (cdr right-result)))
;         (cons (make-tree this-entry left-tree right-tree) remaining-elts))))
;
; a. Write a short paragraph explaining as clearly as you can how partial-tree
;    works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11)
; b. What is the order of growth in the number of steps required by list->tree
;    to convert a list of n elements?

; a. The procedure works in a fairly simple fashion.
;
; It splits the list in three parts - a left sub-list, a right sub-list and the
; element between them. The parts are roughly equal in size. The result is a
; tree whose entry is the middle element and whose branches are the sub-lists
; transformed to trees with the same procedure (recursively).
;
; Once the procedure arrives to a list with size <= 3, it is trivial to
; visualize how the tree would look like. Lists of sizes > 3 get reduced to
; those cases with recursion.
;
; The final tree is binary, because the left branch contains elements that are
; smaller than the middle element and the right branch contains only elements
; that are greater than the middle element. It is balanced, because the
; algorithm halves the list size on each step, which means that the maximum
; depth of the tree will be log(n).
;
; FYI, (list->tree '(1 3 5 7 9 11)) produces:
;
;       5
;     /   \
;   1       9
;    \     / \
;     3   7   11
;
; b. Θ(n)
;
; Each list item is visited only once and each visit performs a single cons.

; SICP exercise 2.65
;
; Use the results of exercise 2.63 and 2.64 to give Θ(n) implementations of
; union-set and intersection-set for sets implemented as (balanced) binary
; trees.

; SICP exercise 2.65
;
; Use the results of exercise 2.63 and 2.64 to give Θ(n) implementations of
; union-set and intersection-set for sets implemented as (balanced) binary
; trees.

#lang racket
(provide (all-defined-out))

(define empty-set '())

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

;; subtree also is list
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list set1)
                              (tree->list set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list set1)
                                     (tree->list set2))))

;; Exercise 2.63

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; Exercise 2.64

;; Note: input list must be ordered
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (union-set-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        ((= (car list1) (car list2))
         (cons (car list1) (union-set-list (cdr list1) (cdr list2))))
        ((< (car list1) (car list2))
         (cons (car list1) (union-set-list (cdr list1) list2)))
        ((> (car list1) (car list2))
         (cons (car list2) (union-set-list list1 (cdr list2))))))

(define (intersection-set-list list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (let ((x1 (car list1))
            (x2 (car list2)))
        (cond ((= x1 x2) (cons x1 (intersection-set-list (cdr list1) (cdr list2))))
              ((< x1 x2) (intersection-set-list (cdr list1) list2))
              ((> x1 x2) (intersection-set-list list1 (cdr list2)))))))

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-2.65-tests
    (test-suite
     "Tests for SICP exercise 2.65"

     (check-equal?
      (tree->list (intersection-set (list->tree '(1 3 5 7 9 11))
                                    (list->tree '(2 3 5 9 10))))
      '(3 5 9))

     (check-equal?
      (tree->list (union-set (list->tree '(1 3 5 7 9 11))
                             (list->tree '(2 3 5 9 10))))
      '(1 2 3 5 7 9 10 11))
     ))

  (run-tests sicp-2.65-tests)
  )

;; convert list to balanced binary tree
;; (list->tree '(1))
;; (list->tree '(1 2))
;; (list->tree '(1 2 3))
;; (list->tree '(1 2 3 4))

