#lang racket

;; is-palindrome?: Number -> Boolean
;; Checks if a number is a palindrome
(define (is-palindrome? num)
    (equal? (number->string num) (list->string (reverse (string->list (number->string num))))))

;; <tests>
(require rackunit)
(check-equal? (is-palindrome? 500) #false)
(check-equal? (is-palindrome? 505) #true)
(check-equal? (is-palindrome? 0) #true)
(check-equal? (is-palindrome? 5005) #true)