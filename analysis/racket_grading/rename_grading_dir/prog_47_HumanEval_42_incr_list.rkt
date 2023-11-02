#lang racket

;; Return list with elements incremented by 1.
;; >>> (incr_list (list 1 2 3))
;; (list 2 3 4)
;; >>> (incr_list (list 5 3 5 2 3 3 9 0 123))
;; (list 6 4 6 3 4 4 10 1 124)
(define (incr_list l)

	(map (lambda (x) (+ x 1)) l))
