#lang racket
(require rackunit)

;; collatz-n: Number Number -> Number
;; Apply the Collatz conjecture to the first number
;; n times. Divide by 2 if its even, otherwise
;; multiply by 3 and add 1
(define (collatz num n)
;; <solution>
    (if (= n 0) 
        num
        (collatz 
            (if (odd? num) 
                (+ (* 3 num) 1)
                (/ num 2)) (- n 1))))

;; <tests>
(check-equal? (collatz 5 0) 5)
(check-equal? (collatz 5 1) 16)
(check-equal? (collatz 5 2) 8)
(check-equal? (collatz 5 3) 4)
(check-equal? (collatz 6 0) 6)
(check-equal? (collatz 6 1) 3)
(check-equal? (collatz 6 2) 10)
(check-equal? (collatz 6 3) 5)