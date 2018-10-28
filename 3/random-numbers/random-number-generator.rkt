; SICP exercise 3.06
;
; It is useful to be able to reset a random-number generator to produce a
; sequence starting from a given value. Design a new rand procedure that is
; called with an argument that is either the symbol generate or the symbol
; reset and behaves as follows: (rand 'generate) produces a new random number;
; ((rand 'reset) <new-value>) resets the internal state variable to the
; designated <new-value>. Thus, by resetting the state, one can generate
; repeateble sequences. These are very handy to have when testing and
; debugging programs that use random numbers.

#lang racket
(provide rand)
(require "./random.rkt")

(define rand
  (let ((x random-init))
    (lambda (message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))
            (else (error "Unknown request - RAND" message))))))

(module+ main
  (rand 'generate)
  (rand 'generate)
  ((rand 'reset) 0)
  (rand 'generate)
  (rand 'generate)  
  )

