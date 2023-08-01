#lang racket
(require rackunit)

;; hello : String -> String
;; Greets you hello!
(define (hello s)
  (string-append "Hello " s "!"))

;; goodbye : String -> String 
;; Greets you goodbye!
(define (goodbye s)
  (string-append "Goodbye " s "!"))

;; hello-goodbye : [List-of String] -> [List-of String]
;; Greets everyone hello and goodbye.
(define (hello-goodbye l)
  (double-do-it hello goodbye l))

;; double-do-it : (X Y) [X -> Y] [X -> Y] [List-of X] -> [List-of Y]
;; Helper for hello-goodbye.
(define (double-do-it f g l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons (f (first l))
           (cons (g (first l))
                 (double-do-it f g (rest l))))]))
                 
;; <tests>
(check-equal? (hello-goodbye '()) '())
(check-equal?
 (hello-goodbye (cons "Alice" (cons "Bob" '())))
 (cons "Hello Alice!" (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))
(check-equal? (double-do-it sqr sqrt '()) '())
(check-equal? (double-do-it sqr sqrt (cons 25 (cons 9 '())))
              (cons 625 (cons 5 (cons 81 (cons 3 '())))))