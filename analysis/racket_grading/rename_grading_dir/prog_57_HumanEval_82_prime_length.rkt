#lang racket

;; Write a function that takes a string and returns #t if the string
;; length is a prime number or #f otherwise
;; Examples
;; >>> (prime_length "Hello")
;; #t
;; >>> (prime_length "abcdcba")
;; #t
;; >>> (prime_length "kittens")
;; #t
;; >>> (prime_length "orange")
;; #f
(define (prime_length string)

	(define (prime? n)
		(cond
			((= n 1) #f)
			((= n 2) #t)
			((= (modulo n 2) 0) #f)
			(else
				(define (iter i)
					(if (> (* i i) n)
						#t
						(if (= (modulo n i) 0)
							#f
							(iter (+ i 2))
						)
					)
				)
				(iter 3)
			)
		)
	)
	(prime? (string-length string))
)
