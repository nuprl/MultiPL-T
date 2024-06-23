#lang racket

;; sum-remainders: [List-of Numbers] Number -> Number
;; Sums the remainder of the numbers in the list when divided by num
(define (sum-remainders lon num)
    (foldr + 0 (map (lambda (n) (remainder n num)) lon)))

;; <tests>
(require rackunit)
(check-equal? (sum-remainders (list 5 6 7) 5) 3)
(check-equal? (sum-remainders (list 5 4 3) 2) 2)
(check-equal? (sum-remainders (list 5 9 7) 9) 12)