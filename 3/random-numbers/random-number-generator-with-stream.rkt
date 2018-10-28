; SICP exercise 3.81
;
; Exercise 3.6 discussed generalizing the random-number generator to allow one
; to reset the random-number sequence so as to produce repeatable sequences of
; "random" numbers. Produce a stream formulation of this same generator that
; operates on an input stream of requests to generate a new random number or
; to reset the sequence to a specified value and that produces the desired
; stream of random numbers. Don't use assignment in your solution.

#lang racket
(provide random-numbers)
(require "../stream/stream.rkt"
         "./random.rkt")

;; random-numbers stream
;; (define random-numbers
;;   (cons-stream
;;    random-init
;;    (stream-map rand-update random-numbers)))

(define (random-numbers requests)
  (define (next seed requests)
    (cond ((stream-null? requests) the-empty-stream)
          ((eq? (stream-car requests) 'generate)
           (let ((generated (rand-update seed))
                 (rest (stream-cdr requests)))
             (stream-cons generated (next generated rest))))
          ((eq? (stream-car requests) 'reset)
           (let ((new-seed (stream-car (stream-cdr requests)))
                 (rest (stream-cdr (stream-cdr requests))))
             (next new-seed rest)))))
  (next random-init requests))

;; (stream-take
;;  (random-numbers
;;   (list->infinite-stream
;;    '(reset 1 generate generate generate generate
;;            reset 2 generate generate generate generate)))
;;  8)

(module+ main
  (require rackunit rackunit/text-ui)
  
  (define sicp-3.81-tests
    (test-suite
     "Tests for SICP exercise 3.81"

     (check-equal? (stream-take
                    (random-numbers
                     (list->infinite-stream
                      '(reset 1 generate generate generate generate
                              reset 2 generate generate generate generate)))
                    8)
                   '(1015568748 1586005467 2165703038 3027450565
                                1017233273 1975575172 811535379 3186434646))
     ))

  (run-tests sicp-3.81-tests)
  )
