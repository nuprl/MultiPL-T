;; Example A
#lang racket

;; Write a function that returns #t if the object q will fly, and #f otherwise.
;; The object q will fly if it's balanced (it is a palindromic list) and the sum of its elements is less than or equal the maximum possible weight w.
;; Example:
;; >>> (will_it_fly (list 1 2) 5)
;; #f
;; # 1+2 is less than the maximum possible weight, but it's unbalanced.
;; >>> (will_it_fly (list 3 2 3) 1)
;; #f
;; # it's balanced, but 3+2+3 is more than the maximum possible weight.
;; >>> (will_it_fly (list 3 2 3) 9)
;; #t
;; # 3+2+3 is less than the maximum possible weight, and it's balanced.
;; >>> (will_it_fly (list 3) 5)
;; #t
;; # 3 is less than the maximum possible weight, and it's balanced.
(define (will_it_fly q w)

  (define (sum l)
    (if (null? l)
        0
        (+ (car l) (sum (cdr l)))))
  (define (palindrome? l)
    (define (reverse-list l)
      (if (null? l)
          '()
          (append (reverse-list (cdr l)) (list (car l)))))
    (equal? l (reverse-list l)))
  (if (palindrome? q)
      (<= (sum q) w)
      #f))

;; Example B
#lang racket

;; Write a function that returns #t if the object q will fly, and #f otherwise.
;; The object q will fly if it's balanced (it is a palindromic list) and the sum of its elements is less than or equal the maximum possible weight w.
;; Example:
;; >>> (will_it_fly (list 1 2) 5)
;; #f
;; # 1+2 is less than the maximum possible weight, but it's unbalanced.
;; >>> (will_it_fly (list 3 2 3) 1)
;; #f
;; # it's balanced, but 3+2+3 is more than the maximum possible weight.
;; >>> (will_it_fly (list 3 2 3) 9)
;; #t
;; # 3+2+3 is less than the maximum possible weight, and it's balanced.
;; >>> (will_it_fly (list 3) 5)
;; #t
;; # 3 is less than the maximum possible weight, and it's balanced.
(define (will_it_fly q w)

	(define (sum lst)
		(if (null? lst)
			0
			(+ (car lst) (sum (cdr lst)))
		)
	)
	(define (is-palindrome? lst)
		(define (helper lst1 lst2)
			(cond
				((null? lst1) #t)
				((null? lst2) #f)
				((equal? (car lst1) (car lst2)) (helper (cdr lst1) (cdr lst2)))
				(else #f)
			)
		)
		(helper lst (reverse lst))
	)
	(if (is-palindrome? q)
		(<= (sum q) w)
		#f
	)
)
