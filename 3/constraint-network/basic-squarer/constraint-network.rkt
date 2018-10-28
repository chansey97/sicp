; SICP exercise 3.34
;
; Louis Reasoner wants to build a squarer, a constraint device with two
; terminals such that the value of connector b on the second terminal will
; always be the square of the value a on the first terminal. He proposes the
; following simple device made from a multiplier:
;
;   (define (squarer a b)
;     (multiplier a a b))
;
; There is a serious flaw in this idea. Explain.

; This will work alright when a is known and b is unknown. In the reverse
; case, however, we have a multiplier with a known product and two unknown
; multiplicands. There is no way to calculate what they are. Thus, it will
; simply not work when a is unknown.

; SICP exercise 3.35
;
; Ben Bitdiddle tells Louis that one way to avoid the trouble in exercise 3.34
; is to define a squarer as a new primitive constraint. Fill in the missing
; portions in Ben's outline for a procedure to implement such a constraint:
;
;   (define (squarer a b)
;     (define (process-new-value)
;       (if (has-value? b)
;           (if (< (get-value b) 0)
;               (error "square less than 0 - SQUARER" (get-value b))
;               <alternative1>)
;           <alternative2>))
;     (define (process-forget-value) <body1>)
;     (define (me request) <body2>)
;     <rest-of-definition>>
;     me)

; I think a cond would have been nicer, but whatever. Here it is:

#lang racket
(provide (all-defined-out))

; Constraints

; Exercise 3.35 squarer
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 - SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (if (< (get-value a) 0)
                (error "does not work with negative numbers - SQUARER" (get-value a))
                (set-value! b (square (get-value a)) me))
            'ignored)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define (square x)
  (* x x))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request - CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint) (constraint 'i-have-a-value))
(define (inform-about-no-value constraint) (constraint 'i-lost-my-value))

; Connectors

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints))
          #t)
      (if (has-value? me)
          (inform-about-value new-constraint)
          #t)
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operator - CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant) ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))

; Probe

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - PROBE" request))))
  (connect connector me)
  me)

