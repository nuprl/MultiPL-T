#lang racket

;; watching-you?: Number -> Boolean
;; Sees if the number has a "00" in it
(define (watching-you? num)
    (string-contains? (number->string num) "00"))

;; <tests>
(require rackunit)
(check-equal? (watching-you? 5005) #true)
(check-equal? (watching-you? 5005005) #true)
(check-equal? (watching-you? 505) #false)
(check-equal? (watching-you? 0) #false)