;; #lang racket
;; (is-palindrome? 1)
;; (is-palindrome? 12321)
;; (is-palindrome? 12341234)

--------------------

#lang racket

;; smallest-palindrome: Number -> Number
;; Finds the smallest palindrome that is greater than or equal to the number
(define (smallest-palindrome num)
    (define (loop n pal)
        (if (>=