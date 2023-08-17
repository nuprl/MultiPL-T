;; sum-palindromes: [List-of Numbers] -> Number
;; Sums the palindrome numbers in the list
(define (sum-palindromes lon)
    (foldr + 0 (filter is-palindrome? lon)))
