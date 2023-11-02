#lang racket

;; Return n-th Fibonacci number.
;; >>> (fib 10)
;; 55
;; >>> (fib 1)
;; 1
;; >>> (fib 8)
;; 21
(define (fib n)

  (cond ((= n 1) 1)
        ((= n 2) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
