; SICP exercise 2.37
;
; Suppose we represent vectors v = (vᵢⱼ) as sequences of numbers, and matrices
; m = (mᵢⱼ) as sequences of vectors (rows of the matrix). For example, the
; matrix
;
;   1 2 3 4
;   4 5 6 6
;   6 7 8 9
;
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this
; representation, we can use sequence operations to concisely express the basic
; matrix and vector operations. These operations (which are described in any
; book on matrix algebra) are the following:
;
;     (dot-product v w) returns the sum Σᵢvᵢwᵢ
; (matrix-*-vector m v) returns the vector t, where tᵢ = Σⱼmᵢⱼvⱼ
; (matrix-*-matrix m n) returns the matrix p, where pᵢⱼ = Σᵤmᵢᵤvᵤⱼ
;         (transpose m) returns the matrix n, where nᵢⱼ = mⱼᵢ
;
; We can define the dot product as
;
; (define (dot-product v w)
;   (accumulate + 0 (map * v w)))
;
; Fill in the missing expressions in the following procedures for computing the
; other matrix operations. (The procedure accumulate-n is defined exercise 2.36.)
;
; (define (matrix-*-vector m v)
;   (map <??> m))
;
; (define (transpose mat)
;   (accumulate-n <??> <??> mat))
;
; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;     (map <??> m)))

#lang racket
(require rackunit rackunit/text-ui)
(require "../accumulate.rkt"
         "../accumulate-n.rkt")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define sicp-2.37-tests
  (test-suite
    "Tests for SICP exercise 2.37"

    (check-equal?
      (matrix-*-vector '((1 2)
                         (3 4))
                       '(5 6))
      '(17 39))

    (check-equal?
      (transpose '((1 2 3)
                   (4 5 6)
                   (7 8 9)))
      '((1 4 7)
        (2 5 8)
        (3 6 9)))

    (check-equal?
      (matrix-*-matrix '((1 2)
                         (3 4))
                       '((5 6)
                         (7 8)))
      '((19 22)
        (43 50)))
))

(run-tests sicp-2.37-tests)

;; (transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
;; ((1 4 6)
;;  (2 5 7)
;;  (3 6 8)
;;  (4 6 9))

;; (matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9)))
;   1 2 3   1 2 3   30 36 42
;   4 5 6 * 4 5 6 = 66 81 96
;   7 8 9   7 8 9   102 126 150
