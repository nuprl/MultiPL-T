#lang racket

;; Given list of numbers (of at least two elements), apply a linear transform to that list,
;; such that the smallest number will become 0 and the largest will become 1
;; >>> (rescale_to_unit (list 1.0 2.0 3.0 4.0 5.0))
;; (list 0.0 0.25 0.5 0.75 1.0)
(define (rescale_to_unit numbers)

  (define (rescale_to_unit_helper numbers min max)
    (if (null? numbers)
        '()
        (cons (/ (- (car numbers) min) (- max min))
              (rescale_to_unit_helper (cdr numbers) min max))))
  (rescale_to_unit_helper numbers (apply min numbers) (apply max numbers)))
