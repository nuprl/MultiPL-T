;; Example A
#lang racket

;; The FibFib number sequence is a sequence similar to the Fibbonacci sequnece that's defined as follows:
;; fibfib(0) == 0
;; fibfib(1) == 0
;; fibfib(2) == 1
;; fibfib(n) == fibfib(n-1) + fibfib(n-2) + fibfib(n-3).
;; Please write a function to efficiently compute the n-th element of the fibfib number sequence.
;; >>> (fibfib 1)
;; 0
;; >>> (fibfib 5)
;; 4
;; >>> (fibfib 8)
;; 24
(define (fibfib n)

  (cond ((= n 0) 0)
        ((= n 1) 0)
        ((= n 2) 1)
        (else (+ (fibfib (- n 1))
                 (fibfib (- n 2))
                 (fibfib (- n 3))))))

;; Example B
#lang racket

;; The FibFib number sequence is a sequence similar to the Fibbonacci sequnece that's defined as follows:
;; fibfib(0) == 0
;; fibfib(1) == 0
;; fibfib(2) == 1
;; fibfib(n) == fibfib(n-1) + fibfib(n-2) + fibfib(n-3).
;; Please write a function to efficiently compute the n-th element of the fibfib number sequence.
;; >>> (fibfib 1)
;; 0
;; >>> (fibfib 5)
;; 4
;; >>> (fibfib 8)
;; 24
(define (fibfib n)

	(cond
		((= n 0) 0)
		((= n 1) 0)
		((= n 2) 1)
		((= n 3) 1)
		(else (+ (fibfib (- n 1)) (fibfib (- n 2)) (fibfib (- n 3))))))