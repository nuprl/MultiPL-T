#lang racket

;; Given the lengths of the three sides of a triangle. Return #t if the three
;; sides form a right-angled triangle, #f otherwise.
;; A right-angled triangle is a triangle in which one angle is right angle or 
;; 90 degree.
;; Example:
;; >>> (right_angle_triangle 3 4 5)
;; #t
;; >>> (right_angle_triangle 1 2 3)
;; #f
(define (right_angle_triangle a b c)

	(define (square x) (* x x))
	(if (or (= (+ (square a) (square b)) (square c)) (= (+ (square b) (square c)) (square a)) (= (+ (square c) (square a)) (square b)))
		#t
		#f
	)
)
