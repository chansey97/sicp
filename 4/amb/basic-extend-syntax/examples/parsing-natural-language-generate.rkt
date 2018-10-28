#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")

;; Exercise 4.49 4.50
(define solution
  '((define nouns '(noun student professor cat class))
    (define verbs '(verb studies lectures eats sleeps))
    (define articles '(article the a))
    (define prepositions '(prep for to in by with))

    (define (parse-sentence)
      (list 'sentence
            (parse-noun-phrase)
            (parse-verb-phrase)))

    (define (parse-noun-phrase)
      (define (maybe-extend noun-phrase)
        (ramb noun-phrase
             (maybe-extend (list 'noun-phrase
                                 noun-phrase
                                 (parse-prepositional-phrase)))))
      (maybe-extend (parse-simple-noun-phrase)))

    (define (parse-simple-noun-phrase)
      (list 'simple-noun-phrase
            (parse-word articles)
            (parse-word nouns)))

    (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
        (ramb verb-phrase
             (maybe-extend (list 'verb-phrase
                                 verb-phrase
                                 (parse-prepositional-phrase)))))
      (maybe-extend (parse-word verbs)))

    (define (parse-prepositional-phrase)
      (list 'prep-phrase
            (parse-word prepositions)
            (parse-noun-phrase)))

    (define (parse-word word-list)
      (if (null? (cdr word-list))
          (ramb)
          (ramb (list (car word-list) (cadr word-list))
                (parse-word (cons (car word-list) (cddr word-list))))))

    (define (generate-sentence)
      (parse-sentence))

    (define (terminals sentence)
      (cond ((null? sentence) '())
            ((pair? (car sentence)) (append (terminals (car sentence))
                                            (terminals (cdr sentence))))
            ((memq (car sentence) '(noun verb article prep)) (list (cadr sentence)))
            (else (terminals (cdr sentence)))))

    ))

(define (a-value-of exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) value)
           (lambda () '())))

(define solution-environment
  ((lambda ()
     (define environment (setup-environment))
     (for-each (lambda (definition)
                 (ambeval definition
                          environment
                          (lambda (value fail) 'ok)
                          (lambda () 'ok)))
               solution)
     environment)))

(displayln (a-value-of '(terminals (generate-sentence))))
