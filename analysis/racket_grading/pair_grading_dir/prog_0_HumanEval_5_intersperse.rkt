;; Example A
#lang racket

;; Insert a number 'delimeter' between every two consecutive elements of input list `numbers'
;; >>> (intersperse (list ) 4)
;; (list )
;; >>> (intersperse (list 1 2 3) 4)
;; (list 1 4 2 4 3)
(define (intersperse numbers delimeter)

	(cond
		[(null? numbers) '()]
		[(null? (cdr numbers)) numbers]
		[else (cons (car numbers) (cons delimeter (intersperse (cdr numbers) delimeter)))]
	)
)

;; Example B
#lang racket

;; Insert a number 'delimeter' between every two consecutive elements of input list `numbers'
;; >>> (intersperse (list ) 4)
;; (list )
;; >>> (intersperse (list 1 2 3) 4)
;; (list 1 4 2 4 3)
(define (intersperse numbers delimeter)

  (cond
    [(null? numbers) '()]
    [(null? (cdr numbers)) numbers]
    [else (cons (car numbers) (cons delimeter (intersperse (cdr numbers) delimeter)))]))
