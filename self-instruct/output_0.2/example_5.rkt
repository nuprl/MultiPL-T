;; #lang racket
;; 
;; ;; is-palindrome?: Number -> Boolean
;; ;; Checks if a number is a palindrome
;; (define (is-palindrome? num)
;;     (equal? (number->string num) (reverse (string->list (number->string num)))))


--------------------

#lang racket

;; is-palindrome?: Number -> Boolean
;; Checks if a number is a palindrome
(define (is-palindrome? num)
    (equal? (number->string num) (reverse (string->list (number->string num))))))
