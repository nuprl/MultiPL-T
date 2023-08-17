;; sum-digits-of-number: Number -> Number
;; Sums the digits of a number
(define (sum-digits-of-number num)
    (foldr + 0 (map string->number (string-split (number->string num) ""))))
