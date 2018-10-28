#lang racket
(provide (all-defined-out))

(define algebra-rules
  '(
    ( ((? op) (?c e1) (?c e2))
      (: (op e1 e2))                                                 )

    ( ((? op) (? e1) (?c e2))
      ((: op) (: e2) (: e1))                                         )

    ( (+ 0 (? e))                                              (: e) )

    ( (* 1 (? e))                                              (: e) )

    ( (* 0 (? e))                                                  0 )

    ( (* (?c e1) (* (?c e2) (? e3)))
      (* (: (* e1 e2)) (: e3))                                       )

    ;; e1*(e2*e3) which e1 and e2 are constant => (e1*e2)*e3
    ( (* (?c e1) (* (?c e2) (? e3)))
      (* (: (* e1 e2)) (: e3))                                       )

    ;; e1*(e2*e3) which e2 is constant => e2*(e1*e3)
    ( (* (? e1) (* (?c e2) (? e3)))
      (* (: e2) (* (: e1) (: e3)))                                   )

    ;; (e1*e2)*e3 => e1*(e2*e3)
    ( (* (* (? e1) (? e2)) (? e3))
      (* (: e1) (* (: e2) (: e3)))                                   )

    ;; e1+(e2+e3) which e1 e2 are constant => (e1+e2)+e3 
    ( (+ (?c e1) (+ (?c e2) (? e3)))
      (+ (: (+ e1 e2)) (: e3))                                       )

    ;; e1+(e2+e3) which e2 is constant => e2+(e1+e3)
    ( (+ (? e1) (+ (?c e2) (? e3)))
      (+ (: e2) (+ (: e1) (: e3)))                                   )

    ;; (e1+e2)+e3 => e1+(e2+e3)
    ( (+ (+ (? e1) (? e2)) (? e3))
      (+ (: e1) (+ (: e2) (: e3)))                                   )

    ;; c*a+d*a which c d are constant => (c+d)*a
    ( (+ (* (?c c) (? a)) (* (?c d) (? a)))
      (* (: (+ c d)) (: a))                                          )

    ;; c*(d+e) => c*d+c*e
    ( (* (? c) (+ (? d) (? e)) )
      (+ (* (: c) (: d)) (* (: c) (: e)))                            )

    ;; Commutative law, stuck loop...
    
    ;; ( (* (? e1) (? e2))
    ;;   (* (: e2) (: e1))                                              )
    
    ;; ( (+ (? e1) (? e2))
    ;;   (+ (: e2) (: e1))                                              )
    
    ))
