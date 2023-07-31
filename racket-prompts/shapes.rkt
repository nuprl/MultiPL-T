#lang racket
(require rackunit)

;; A Shape is one of:
;; - (make-circle Number)
;; - (make-rectangle Number Number)
;; - (make-triangle Number Number Number)
;; Interpretation: A closed shape
(define-struct circle [radius] #:transparent)
(define-struct rectangle [w l] #:transparent)
(define-struct triangle [s1 s2 s3] #:transparent)

;; circle-perimeter: Circle -> Number
;; Computes the perimeter of a circle
(define (circle-perimeter c)
    (* 6.28 (circle-radius c)))

;; rectangle-perimeter: Rectangle -> Number
;; Computes the perimeter of a rectangle
(define (rectangle-perimeter r)
    (+ (* 2 (rectangle-w r)) (* 2 (rectangle-l r))))

;; triangle-perimeter: Triangle -> Number
;; Computes the perimeter of a triangle
(define (triangle-perimeter t)
    (+ (triangle-s1 t) (+ (triangle-s2 t) (triangle-s3 t))))

;; shape-perimeter: Shape -> Number
;; Computes the perimeter of any Shape
(define (shape-perimeter s)
;; <solution>
    (cond
        [(triangle? s) (triangle-perimeter s)]
        [(rectangle? s) (rectangle-perimeter s)]
        [(circle? s) (circle-perimeter s)]))

;; <tests>
(check-equal? (shape-perimeter (make-circle 2)) 12.56)
(check-equal? (shape-perimeter (make-rectangle 2 4)) 12)
(check-equal? (shape-perimeter (make-triangle  3 3 3)) 9)