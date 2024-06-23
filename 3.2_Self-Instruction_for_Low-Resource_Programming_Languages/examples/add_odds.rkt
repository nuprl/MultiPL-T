#lang racket

;; add-odds: [List-of Numbers] -> Number
;; Adds all the odd numbers in a list
(define (add-odds lon)
    (foldr + 0 (filter odd? lon)))

;; <tests>
(require rackunit)
(check-equal? (add-odds (list 5 6 7)) 12)
(check-equal? (add-odds (list 7 9 14)) 16)
(check-equal? (add-odds (list 8 9 14)) 9)
(check-equal? (add-odds (list 12 16 14)) 0)