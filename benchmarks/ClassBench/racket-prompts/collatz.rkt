#lang racket
(require rackunit)

;; collatz: Number ->  Number
;; Counts how many steps it takes a number to converge
;; to 1 through the collatz sequence. The collatz sequence
;; divides by 2 if the number is even, otherwise if the number
;; is odd, it multiplies by 3 and adds 1
(define (collatz num)
;; <solution>
    (if (= num 1) 0
        (+ 1 (collatz (if (odd? num) (+ (* num 3) 1) (/ num 2))))))
;; <tests>
(check-equal? (collatz 1) 0)
(check-equal? (collatz 2) 1)
(check-equal? (collatz 4) 2)
(check-equal? (collatz 3) 7)
(check-equal? (collatz 12) 9)