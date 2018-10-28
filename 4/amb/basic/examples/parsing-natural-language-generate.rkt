; SICP exercise 4.49
;
; Alyssa P. Hacker is more interested in generating interesting sentences than
; in parsing them. She reasons that by simply changing the procedure
; parse-word so that it ignores the "input sentence" and instead always
; succeeds and generates an appropriate word, we can use the programs we had
; built for parsing to do generation instead. Implement Alyssa's idea, and
; show the first half-dozen or so sentences generated.

#lang racket
(require r5rs/init)
(require rackunit rackunit/text-ui)
(require "../evaluator.rkt")

; The modification is below. There is an additional procedure terminals that
; extracts the terminal symbols of the parsed sentence to form a list that
; looks like a sentence. Here are the first half-dozen sentences:
;
; (the student studies)
; (the student studies for the student)
; (the student studies for the student for the student)
; (the student studies for the student for the student for the student)
; (the student studies for the student for the student for the student for the student)
; (the student studies for the student for the student for the student for the student for the student)
;
; Obviously, there are not very useful, even if they are "gramatically
; correct" for some definition of the term.

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
        (amb noun-phrase
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
        (amb verb-phrase
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
          (amb)
          (amb (list (car word-list) (cadr word-list))
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

(define (first-n-values n exp)
  (define (take n results)
    (if (= n 0)
        '()
        (cons (car results)
              (take (- n 1) (force (cdr results))))))
  (take n
        (ambeval exp
                 solution-environment
                 (lambda (value fail) (cons value (delay (fail))))
                 (lambda () '()))))

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

(pretty-display (first-n-values 4 '(terminals (generate-sentence))))
