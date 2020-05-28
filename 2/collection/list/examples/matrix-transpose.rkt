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

(define (transpose mat)
  (if (null? (car mat))
      '()
      (cons (map car mat)
        (transpose (map cdr mat)))))

(transpose '((1 2 3)
             (4 5 6)
             (7 8 9)))
(transpose '((3)
             (6)
             (9)))
(transpose '(()
             ()
             ()))

(transpose '(()))

(define (transpose-iter mat)
  (define (iter result rest)
    (if (null? (car rest))
        result
        (iter (append result (list (map car rest))) (map cdr rest))))
  (iter '() mat))

(transpose-iter '((1 2 3)
                  (4 5 6)
                  (7 8 9)))
(transpose-iter '((3)
                  (6)
                  (9)))
(transpose-iter '(()
                  ()
                  ()))

(transpose-iter '(()))
