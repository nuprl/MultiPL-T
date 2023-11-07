#lang racket

;; Filter given list of any rktthon values only for integers
;; >>> (filter_integers (list "a" 3.14 5))
;; (list 5)
;; >>> (filter_integers (list 1 2 3 "abc" #hash() (list )))
;; (list 1 2 3)
(define (filter_integers values)

	(filter (lambda (x) (and (integer? x) (exact? x))) values))
