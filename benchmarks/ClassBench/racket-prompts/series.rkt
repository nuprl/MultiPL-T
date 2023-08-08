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
(define 0-add-one (make-computable 5 add-one))
(define 1-add-one (update 0-add-one))
(define 2-add-one (update 1-add-one))
(check-equal? (computable-value (update 0-add-one)) 6)
(check-equal? (computable-value (update 1-add-one)) 7)
(check-equal? (computable-value (update 2-add-one)) 8)
(define (repeat-string s) (string-append s s))
(define 0-repeat-string (make-computable "hi" repeat-string))
(define 1-repeat-string (update 0-repeat-string))
(define 2-repeat-string (update 1-repeat-string))
(check-equal? (computable-value (update 0-repeat-string)) "hihi")
(check-equal? (computable-value (update 1-repeat-string)) "hihihihi")
(check-equal? (computable-value (update 2-repeat-string)) "hihihihihihihihi")
(define (divide-2 x) (/ x 2))
(define 0-divide-2 (make-computable 40 divide-2))
(define 1-divide-2 (update 0-divide-2))
(define 2-divide-2 (update 1-divide-2))
(check-equal? (computable-value (update 0-divide-2)) 20)
(check-equal? (computable-value (update 1-divide-2)) 10)
(check-equal? (computable-value (update 2-divide-2)) 5)