#lang racket
(require "../../random-numbers/random.rkt"
         "../monte-carlo.rkt")

(define random-init 0)
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(estimate-pi 100000)

;; Note:
;; (1) There very low probability to occur div 0 error in estimate-pi, since experiment maybe all failed.
;; (2) (estimate-pi 100000) return 2.7207, it's not Pi. This because the sequence (rand) alternates odd/even, so you never get a common factor of 2. That will increase your cesar-test successes. See https://math.stackexchange.com/questions/455558/probability-sequential-terms-of-a-linear-congruential-generator-are-coprime/455608#_=_

;; Fixed: /8 instead of /6:
;; (define (estimate-pi-fixed trials)
;;   (sqrt (/ 8 (monte-carlo trials cesaro-test))))

;; (estimate-pi-fixed 100000)

;; Or using Blum Blum Shub generator instead of Linear congruential generator
