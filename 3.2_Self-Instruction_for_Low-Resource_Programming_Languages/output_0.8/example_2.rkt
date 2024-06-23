;; is-palindrome-sum?: Number -> Boolean
;; Checks if a number is a palindrome sum of its digits
(define (is-palindrome-sum? num)
    (is-palindrome? (sum-remainders (map char->integer (string->list (number->string num))) 10)))
