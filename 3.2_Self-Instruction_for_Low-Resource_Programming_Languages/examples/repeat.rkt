#lang racket

;; repeat: String Number -> [List-of Strings]
;; Repeats a string num amount of times
(define (repeat str num)
    (if (= num 0) ""
    (string-append str (repeat str (- num 1)))))

;; <tests>
(require rackunit)
(check-equal? (repeat "hi" 3) "hihihi")
(check-equal? (repeat "hello" 0) "")
(check-equal? (repeat "yeah" 2) "yeahyeah")
