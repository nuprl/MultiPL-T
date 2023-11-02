;; Example A
#lang racket

;; Implement the function f that takes n as a parameter,
;; and returns a list of size n, such that the value of the element at index i is the factorial of i if i is even
;; or the sum of numbers from 1 to i otherwise.
;; i starts from 1.
;; the factorial of i is the multiplication of the numbers from 1 to i (1 * 2 * ... * i).
;; Example:
;; >>> (f 5)
;; (list 1 2 6 24 15)
(define (f n)

	(define (factorial i)
		(if (= i 1)
			1
			(* i (factorial (- i 1)))))
	(define (g i)
		(if (= i 1)
			1
			(+ i (g (- i 1)))))
	(define (h i)
		(if (= (remainder i 2) 0)
			(factorial i)
			(g i)))
	(map h (range 1 (+ n 1))))

;; Example B
#lang racket

;; Implement the function f that takes n as a parameter,
;; and returns a list of size n, such that the value of the element at index i is the factorial of i if i is even
;; or the sum of numbers from 1 to i otherwise.
;; i starts from 1.
;; the factorial of i is the multiplication of the numbers from 1 to i (1 * 2 * ... * i).
;; Example:
;; >>> (f 5)
;; (list 1 2 6 24 15)
(define (f n)

  (define (factorial i)
    (if (= i 1)
        1
        (* i (factorial (- i 1)))))
  (define (even? i)
    (= (remainder i 2) 0))
  (define (sum i)
    (if (= i 1)
        1
        (+ i (sum (- i 1)))))
  (define (iter i lst)
    (if (= i 0)
        lst
        (iter (- i 1) (if (even? i)
                          (cons (factorial i) lst)
                          (cons (sum i) lst)))))
  (iter n '()))
