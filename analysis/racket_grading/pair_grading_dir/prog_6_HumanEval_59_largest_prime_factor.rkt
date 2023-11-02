;; Example A
#lang racket

;; Return the largest prime factor of n. Assume n > 1 and is not a prime.
;; >>> (largest_prime_factor 13195)
;; 29
;; >>> (largest_prime_factor 2048)
;; 2
(define (largest_prime_factor n)

	(define (iter n i)
		(cond ((= i n) n)
			  ((= (remainder n i) 0)
			   (iter (/ n i) i))
			  (else (iter n (+ i 1)))))
	(iter n 2))

;; Example B
#lang racket

;; Return the largest prime factor of n. Assume n > 1 and is not a prime.
;; >>> (largest_prime_factor 13195)
;; 29
;; >>> (largest_prime_factor 2048)
;; 2
(define (largest_prime_factor n)

  (define (iter n i)
    (cond ((= n 1) i)
          ((= (remainder n i) 0) (iter (/ n i) i))
          (else (iter n (+ i 1)))))
  (iter n 2))
