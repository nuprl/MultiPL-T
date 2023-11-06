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
		((< n 2) #f)
		((= n 2) #t)
		((= n 3) #t)
		((= 0 (modulo n 2)) #f)
		((= 0 (modulo n 3)) #f)
		(else
			(define (loop n i)
				(if (<= (* i i) n)
					(if (= 0 (modulo n i)) #f (loop n (+ i 2)))
					#t
				)
			)
			(loop n (+ 2 3))
		)
	)
)
