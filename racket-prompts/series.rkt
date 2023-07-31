#lang racket
(require rackunit)

;; A Computable is:
;; - (make-computable X [X -> X])
;; Interpretation: A value that can be processed by the function to make a series
(define-struct computable [value func] #:transparent)

;; update: Computable -> Computable
;; Runs the computable to make the next computable in the series
(define (update c)
;; <solution>
    (make-computable ((computable-func c) (computable-value c)) (computable-func c)))

;; <tests>
(define (add-one x) (+ x 1))
(check-equal? (update (make-computable 5 add-one)) (make-computable 6 add-one))
(define (repeat-string s) (string-append s s))
(check-equal? (update (make-computable "hi" repeat-string)) (make-computable "hihi" repeat-string))
(define (divide-2 x) (/ x 2))
(check-equal? (update (make-computable 6 divide-2)) (make-computable 3 divide-2))