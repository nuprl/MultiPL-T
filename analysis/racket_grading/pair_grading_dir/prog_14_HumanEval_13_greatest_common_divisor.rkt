;; Example A
#lang racket

;; Return a greatest common divisor of two integers a and b
;; >>> (greatest_common_divisor 3 5)
;; 1
;; >>> (greatest_common_divisor 25 15)
;; 5
(define (greatest_common_divisor a b)

  (if (= b 0)
      a
      (greatest_common_divisor b (remainder a b))))

;; Example B
#lang racket

;; Return a greatest common divisor of two integers a and b
;; >>> (greatest_common_divisor 3 5)
;; 1
;; >>> (greatest_common_divisor 25 15)
;; 5
(define (greatest_common_divisor a b)

	(if (= b 0)
		a
		(greatest_common_divisor b (remainder a b))))
