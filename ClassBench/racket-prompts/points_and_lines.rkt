#lang racket
(require rackunit)

;; A Point is:
;; - (make-point Number Number)
;; Interpretation: A point on a cartesian plane
(define-struct point [x y] #:transparent)

;; A Line is:
;; - (make-line Point Point)
;; Interpretation: A straight line between two points
(define-struct line [start end] #:transparent)

;; manhattan-distance: Line -> Number
;; Computes the manhattan distance that a line covers
;; manhattan distance = |x_1 - x_2| + |y_1 - y_2|
(define (manhattan-distance l)
;; <solution>
    (+ 
        (abs (- (point-x (line-start l)) (point-x (line-end l))))
        (abs (- (point-y (line-start l)) (point-y (line-end l))))))

;; <tests>
(check-equal? (manhattan-distance (make-line (make-point 0 0) (make-point 4 3))) 7)
(check-equal? (manhattan-distance (make-line (make-point 1 3) (make-point 4 3))) 3)
(check-equal? (manhattan-distance (make-line (make-point 4 2) (make-point 4 3))) 1)
(check-equal? (manhattan-distance (make-line (make-point 5 8) (make-point 4 3))) 6)
(check-equal? (manhattan-distance (make-line (make-point 7 0) (make-point 7 -5))) 5)
(check-equal? (manhattan-distance (make-line (make-point 4 0) (make-point -4 0))) 8)