;; Example A
#lang racket

;; For a given list of integers, return a list consisting of a sum and a product of all the integers in a list.
;; Empty sum should be equal to 0 and empty product should be equal to 1.
;; >>> (sum_product (list ))
;; (list 0 1)
;; >>> (sum_product (list 1 2 3 4))
;; (list 10 24)
(define (sum_product numbers)

	(if (not (list? numbers))
		(error "Not a list")
		(if (null? numbers)
			(list 0 1)
			(let ((sum 0)
				(product 1))
				(for-each (lambda (x)
					(set! sum (+ sum x))
					(set! product (* product x)))
					numbers)
				(list sum product)))))

;; Example B
#lang racket

;; For a given list of integers, return a list consisting of a sum and a product of all the integers in a list.
;; Empty sum should be equal to 0 and empty product should be equal to 1.
;; >>> (sum_product (list ))
;; (list 0 1)
;; >>> (sum_product (list 1 2 3 4))
;; (list 10 24)
(define (sum_product numbers)

  (if (null? numbers)
      (list 0 1)
      (let ((sum (car numbers))
            (product (car numbers)))
        (for-each (lambda (x)
                    (set! sum (+ sum x))
                    (set! product (* product x)))
                  (cdr numbers))
        (list sum product))))
