;; #lang racket
;; 
;; ;; fizz-buzz: [Number] -> [String]
;; ;; Replaces each multiple of 3 with "fizz", each multiple of 5 with "buzz", and each multiple of both with "fizzbuzz"
;; (define (fizz-buzz lo)
;;     (map (lambda (n)
;;         (cond
;;             [(= n 0) "fizzbuzz"]
;;             [(= (remainder n 3) 0) "fizz"]
;;             [(= (remainder n 5) 0) "buzz"]
;;             [else (number->string n)])) lo))


--------------------

#lang racket

;; fibonacci: Number -> Number
;; The nth fibonacci number
(define (fibonacci n)
    (cond
        [(= n 0) 1]
        [(