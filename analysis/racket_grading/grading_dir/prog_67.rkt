#lang racket

;; Return true if a given number is prime, and false otherwise.
;; >>> (is_prime 6)
;; #f
;; >>> (is_prime 101)
;; #t
;; >>> (is_prime 11)
;; #t
;; >>> (is_prime 13441)
;; #t
;; >>> (is_prime 61)
;; #t
;; >>> (is_prime 4)
;; #f
;; >>> (is_prime 1)
;; #f
(define (is_prime n)

  (cond
    [(= n 1) #f]
    [(= n 2) #t]
    [(even? n) #f]
    [else
      (define (iter i)
        (cond
          [(> i (sqrt n)) #t]
          [(= (remainder n i) 0) #f]
          [else (iter (+ i 1))]))
      (iter 3)]))
