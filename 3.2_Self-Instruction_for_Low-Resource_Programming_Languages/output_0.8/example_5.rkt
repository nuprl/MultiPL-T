;; sum-of-digits: Number -> Number
;; Sums up the digits of a number
(define (sum-of-digits num)
    (if (= num 0) 0
    (+ (remainder num 10) (sum-of-digits (quotient num 10)))))
