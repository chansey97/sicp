#lang racket
(provide (all-defined-out))

(define deriv-rules
  '(
    ( (dd (?c c) (? v))                                                  0 )
    
    ( (dd (?v v) (? v))                                                  1 )

    ;; Additional rule:
    ;; Derivative of a variable called u with respect to a different variable v (it's like const above)
    ;; Here ?v only means that the matched object is a symbol not a number, it does not mean the variable where the derivative respect
    ( (dd (?v u) (? v))                                                  0 )

    ( (dd (+ (? x1) (? x2)) (? v))
      (+  (dd (: x1) (: v))
          (dd (: x2) (: v)))                                               )

    ( (dd (* (? x1) (? x2)) (? v))
      (+ (* (: x1) (dd (: x2) (: v)))
         (* (dd (: x1) (: v)) (: x2)))                                     )

    ( (dd (** (? x) (?c n)) (? v))
      (* (* (: n)
            (** (: x) (: (- n 1))))
         (dd (: x) (: v)))                                                 )
    ))
