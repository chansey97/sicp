#lang racket
(require "../../random-numbers/random.rkt"
         "../../monte-carlo/monte-carlo-with-stream.rkt"
         "../../stream/stream.rkt")

(define random-init (rand-update 0))
(define random-numbers
  (stream-cons random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (stream-cons
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))

(println (stream-ref pi 99999)) ; equal no stream version: (estimate-pi 100000)

;; Note:
;; (1) There very low probability to occur div 0 error in estimate-pi, since experiment maybe all failed.
;; (2) (estimate-pi 100000) return 2.7207, it's not Pi. This because the sequence (rand) alternates odd/even, so you never get a common factor of 2. That will increase your cesar-test successes. See https://math.stackexchange.com/questions/455558/probability-sequential-terms-of-a-linear-congruential-generator-are-coprime/455608#_=_

;; Fixed: /8 instead of /6:
;; (define pi-fixed
;;   (stream-map
;;    (lambda (p) (sqrt (/ 8 p)))
;;    (monte-carlo cesaro-stream 0 0)))

;;  (println (stream-ref pi-fixed 99999))

;; Or using Blum Blum Shub generator instead of Linear congruential generator
