#lang racket
(require "../operations.rkt")
(require "../helpers.rkt")
;; (require "../../../simulator/basic/simulator.rkt")
(require "../../../simulator/basic-instruction-monitoring-with-label/simulator.rkt")

(define (print-instructions inst)
  (println inst))

(define (run-compiled-code compiled-code)
  (define machine (make-machine cm-registers cm-operations compiled-code))

  ;; monitor
  (machine 'trace-on)
  ((machine 'install-trace-proc) print-instructions)
  
  (set-register-contents! machine 'env the-global-environment)
  (start machine)
  (get-register-contents machine 'val)
  )

(define compiled-code
  (compiled-instructions '(begin
                            (define (func n)
                              (define (iter)
                                (if (= n 0) 11 22))
                              (iter))
                            (func 0))))

(println "compiled code:")
(pretty-print compiled-code)

(println "trace:")
(run-compiled-code compiled-code)

; Unbound variable iter
