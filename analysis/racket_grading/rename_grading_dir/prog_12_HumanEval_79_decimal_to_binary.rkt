#lang racket

;; You will be given a number in decimal form and your task is to convert it to
;; binary format. The function should return a string, with each character representing a binary
;; number. Each character in the string will be '0' or '1'.
;; There will be an extra couple of characters 'db' at the beginning and at the end of the string.
;; The extra characters are there to help with the format.
;; Examples:
;; >>> (decimal_to_binary 15)
;; "db1111db"
;; >>> (decimal_to_binary 32)
;; "db100000db"
(define (decimal_to_binary decimal)

  (letrec ([decimal->binary (lambda (decimal)
                              (cond
                                [(= decimal 0) "0"]
                                [(= decimal 1) "1"]
                                [else (string-append (decimal->binary (quotient decimal 2)) (decimal->binary (remainder decimal 2)))]))])
    (string-append "db" (decimal->binary decimal) "db")))
