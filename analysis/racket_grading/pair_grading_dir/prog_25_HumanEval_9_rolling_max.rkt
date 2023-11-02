;; Example A
#lang racket

;; From a given list of integers, generate a list of rolling maximum element found until given moment
;; in the sequence.
;; >>> (rolling_max (list 1 2 3 2 3 4 2))
;; (list 1 2 3 3 3 4 4)
(define (rolling_max numbers)

  (define (max_list numbers)
    (if (null? numbers)
        '()
        (cons (apply max numbers) (max_list (cdr numbers)))))
  (reverse (max_list (reverse numbers))))

;; Example B
#lang racket

;; From a given list of integers, generate a list of rolling maximum element found until given moment
;; in the sequence.
;; >>> (rolling_max (list 1 2 3 2 3 4 2))
;; (list 1 2 3 3 3 4 4)
(define (rolling_max numbers)

	(define (max_in_list list)
		(foldl max 0 list))
	(define (max_in_list_with_index list)
		(let loop ((list list) (index 0) (max_value 0) (max_index 0))
			(if (empty? list)
				(list max_index max_value)
				(let ((value (car list)))
					(if (> value max_value)
						(loop (cdr list) (+ index 1) value index)
						(loop (cdr list) (+ index 1) max_value max_index))))))
	(let loop ((numbers numbers) (max_numbers '()))
		(if (empty? numbers)
			max_numbers
			(let* ((number (car numbers)) (max_number (max_in_list max_numbers)))
				(loop (cdr numbers) (append max_numbers (list (if (> number max_number) number max_number))))))))
