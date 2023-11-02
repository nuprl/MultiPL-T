;; Example A
#lang racket

;; Return maximum element in the list.
;; >>> (max_element (list 1 2 3))
;; 3
;; >>> (max_element (list 5 3 -5 2 -3 3 9 0 123 1 -10))
;; 123
(define (max_element l)

  (if (null? l)
      0
      (if (null? (cdr l))
          (car l)
          (if (> (car l) (max_element (cdr l)))
              (car l)
              (max_element (cdr l))))))

;; Example B
#lang racket

;; Return maximum element in the list.
;; >>> (max_element (list 1 2 3))
;; 3
;; >>> (max_element (list 5 3 -5 2 -3 3 9 0 123 1 -10))
;; 123
(define (max_element l)

	(if (= (length l) 1)
		(car l)
		(if (> (car l) (max_element (cdr l)))
			(car l)
			(max_element (cdr l))
		)
	)
)
