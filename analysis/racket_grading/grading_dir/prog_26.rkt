#lang racket

;; Return only positive numbers in the list.
;; >>> (get_positive (list -1 2 -4 5 6))
;; (list 2 5 6)
;; >>> (get_positive (list 5 3 -5 2 -3 3 9 0 123 1 -10))
;; (list 5 3 2 3 9 123 1)
(define (get_positive l)

  (cond [(null? l) '()]
        [(> (car l) 0) (cons (car l) (get_positive (cdr l)))]
        [else (get_positive (cdr l))]))
