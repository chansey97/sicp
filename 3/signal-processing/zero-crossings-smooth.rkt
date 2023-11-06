; SICP exercise 3.75
;
; Unfortunately, Alyssa's zero-crossing detector in exercise 3.74 provides to
; be insufficient, because the nosiy signal from the sensor leads to spurious
; zero-crossings. Lem E. Tweakit, a hardware specialist, suggests that Alyssa
; smooth the signal to filter out the noise before extracting the zero
; crossings. Alyssa takes his advice and decides to extract the zero crossings
; from the signal constructed by averaging each value of the sense data with
; the previous value. She explains the problem to her assistant, Louis
; Reasoner, who attempts to implement the idea, altering Alyssa's program as
; follows:
;
;   (define (make-zero-crossings input-stream last-value)
;     (let ((avpt (/ (+ (stream-car input-stream)
;                       last-value)
;                    2)))
;       (cons-stream (sign-change-detector avpt last-value)
;                    (make-zero-crossings
;                      (stream-cdr input-stream) avpt))))
;
; This does not correctly implement Alyssa's plan. Fidn the bug that Louis has
; installed and fix it without changing the structure of the program. (Hint:
; You will need to increase the number of arguments to make-zero-crossings.)

; The problem is that it does not average each element with the previous, but
; with the average of the previous and its previous' average. This offsets the
; whole calculation. Here's the real implementation. For a change, no tests,
; since testing this is tricky.

; SICP exercise 3.76
;
; Eva Lu Ator has a criticism of Louis's approach in exercise 3.75. The
; program he wrote is not modular, because it intermixes the operation of
; smoothing with the zero-crossing extraction. For example, the extractor
; should not have to be changed if Alyssa finds a better way to condition her
; input signal. Help Louis by writing a procedure smooth that takes a stream
; as input and produces a stream in which each element is the average of two
; succesive input stream elements. Then use smooth as a component to implement
; the zero-crossing detector in a more modular style.

#lang racket
(provide make-zero-crossings)
(require "../stream/stream.rkt"
         "../stream/stream-map-2.rkt")

(define (sign-change-detector a b)
  (cond ((and (< b 0) (< 0 a)) 1)
        ((and (< a 0) (< 0 b)) -1)
        (else 0)))

;; Exercise 3.75
;; (define (make-zero-crossings input-stream last-value last-avpt)
;;   (let ((avpt (/ (+ (stream-car input-stream)
;;                     last-value)
;;                  2)))
;;     (cons-stream (sign-change-detector avpt last-value)
;;                  (make-zero-crossings
;;                    (stream-cdr input-stream)
;;                    (stream-car input-stream)
;;                    avpt))))

;; Exercise 3.76

;; smooth input-stream first
(define (smooth stream)
  (define (smooth-stream stream prev)
    (let ((first (stream-car stream))
          (rest (stream-cdr stream)))
      (stream-cons (/ (+ first prev) 2.0)
                   (smooth-stream rest prev))))
  (stream-cons (stream-car stream)
               (smooth-stream (stream-cdr stream) (stream-car stream))))

(define (make-zero-crossings input-stream last-value)
  (let ((smooth-stream (smooth input-stream)))
    (stream-map2 sign-change-detector smooth-stream (stream-cons 0 smooth-stream))))


(module+ main
  (require rackunit rackunit/text-ui)

  (define sense-data (list->infinite-stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

  (println (stream-take sense-data 3))
  (println (stream-take (smooth sense-data) 3))
  ;; (println (stream-take (make-zero-crossings sense-data 0) 13))
  
  (define sicp-3.76-tests
    (test-suite
     "Tests for SICP exercise 3.76"

     (check-equal? (stream-take (smooth sense-data) 13)
                   '(1 1.5 1.25 1.0 0.75 0.45 -0.5 -1.0 -0.5 0.25 0.6 2.0 2.5))
     (check-equal? (stream-take (make-zero-crossings sense-data 0) 13)
                   '(0 0 0 0 0 0 -1 0 0 1 0 0 0))
     ))

  (run-tests sicp-3.76-tests)
  )


