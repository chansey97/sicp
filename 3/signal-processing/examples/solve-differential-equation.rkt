#lang racket
(require "../../stream/stream.rkt"
         "../signal-processing.rkt")

;; This is procedure is not guaranteed to work in all Scheme implementations, although
;; for any implementation there is a simple variation that will work. The problem has to
;; do with subtle differences in the ways that Scheme implementations handle internal
;; definitions. (See Section 4.1.6.)

;; Note: r5rs can't work, but racket can.

;; Solving y' = f(y), the initial condition y(0) = y0

(define (solve f y0 dt)
  ;; Remark: if in Haskell, no need explicitly (delay dy) and integral-delayed, thanks laziness! 
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; For example, let f(x) = x, y(0) = 1,
;; i.e. solve y' = y with y(0) = 1
;; Solved: y(t) = e^t, see https://www.wolframalpha.com/input?i2d=true&i=y%27+%3D+y%5C%2844%29+y%5C%2840%290%5C%2841%29+%3D+1

;; 注1：原书有一个 typo，应该是 t = 1 而不是 y = 1, see errata https://www.math.pku.edu.cn/teachers/qiuzy/books/sicp/errata.htm
;; > 希望计算微分方程 dy/dt = y 在初始条件为 y(0) = 1 的情况下在 t = 1处的解。（原书错为 y = 1）

;; 注2：这里必须使用 take 第 1000 才行，因为 dt 是 0.001, 1000 代表 t=1
;; Recommand see Haskell 代码 (a lot of examples)
;; https://github.com/chansey97/experiments/blob/main/haskell/stream/SICP/DifferentialEquation.hs

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            1000)
;; 2.716924

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            4000)
;; 54.4891354543037 = e^4
