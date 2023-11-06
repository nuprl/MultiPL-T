#lang racket

;; Given length of a side and high return area for a triangle.
;; >>> (triangle_area 5 3)
;; 7.5
(define (triangle_area a h)

  (* a h 0.5))
