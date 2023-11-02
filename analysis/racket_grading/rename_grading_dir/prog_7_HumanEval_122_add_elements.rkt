#lang racket

;; Given a non-empty list of integers arr and an integer k, return
;; the sum of the elements with at most two digits from the first k elements of arr.
;; Example:
;; >>> (add_elements (list 111 21 3 4000 5 6 7 8 9) 4)
;; 24
;; Constraints:
;; 1. 1 <= len(arr) <= 100
;; 2. 1 <= k <= len(arr)
(define (add_elements arr k)

	(apply + (map (lambda (x) (if (< x 100) x 0)) (take arr k))))
