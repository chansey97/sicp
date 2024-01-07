#lang racket

;;  SICP 中文版 p.161
;; (define (factorial n)
;;   (define (iter product counter)
;;     (if (> counter n )
;;         product
;;         (iter (* counter product)
;;               (+ counter 1))))
;;   (iter 1 1))

;; Corresponding Haskell code

;; factorialM :: Int -> IO Int
;; factorialM n = do
;;   productRef <- newIORef 1
;;   counterRef <- newIORef 2
;;   let iter _ = do
;;         product <- readIORef productRef
;;         counter <- readIORef counterRef
;;         if counter > n
;;         then return product
;;         else do
;;           _ <- writeIORef productRef (counter * product)
;;           _ <- writeIORef counterRef (counter + 1)
;;           iter ()
;;   iter ()


(define (factorial n)
  (define product 1)
  (define counter 2)
  (define (iter)
    (if (> counter n)
        product
        (begin (set! product (* counter product))
               (set! counter (+ counter 1))
               (iter))))
  (iter))

;; (factorial 0) ; 1
;; (factorial 1) ; 1
;; (factorial 5) ; 120
;; (factorial 6) ; 720

(define (factorialM n)
  (let ((productRef (box 1))
        (counterRef (box 2)))
    (letrec ((iter (λ ()
                     (let ((product (unbox productRef))
                           (counter (unbox counterRef)))
                       ;; (println product)
                       (if (> counter n)
                           product
                           (let* ((_ (set-box! productRef (* counter product)))
                                  (_ (set-box! counterRef (+ counter 1))))
                             (iter))
                           ))
                     )))
      (iter))))

;; (factorialM 0) ; 1
;; (factorialM 1) ; 1
;; (factorialM 5) ; 120
;; (factorialM 6) ; 720


