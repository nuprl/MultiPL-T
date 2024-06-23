;; #lang racket
;; 
;; ;; is-palindrome?: Number -> Boolean
;; ;; Checks if a number is a palindrome
;; (define (is-palindrome? num)
;;     (define (is-palindrome?-helper num)
;;         (if (zero? num) #t
;;         (if (not (= (remainder num 10) (remainder (quotient num 10) 10))) #f
;;         (is-palindrome?-helper (quotient num 10)))))
;;     (is-palindrome?-helper num))


--------------------

#lang racket

;; is-palindrome?: Number -> Boolean
;; Checks if a number is a palindrome
(define (is-palindrome? num)
    (define (is-palindrome?-helper num)
        (if (zero? num) #t
        (if (not (= (remainder num 10) (remainder (quotient num 10) 10))) #f
        (is-palindrome?-helper (quotient num 10)))))
    (is-palindrome?-helper num))
