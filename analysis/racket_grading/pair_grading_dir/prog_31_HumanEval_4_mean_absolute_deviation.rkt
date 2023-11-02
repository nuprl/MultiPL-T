;; Example A
#lang racket

;; For a given list of input numbers, calculate Mean Absolute Deviation
;; around the mean of this dataset.
;; Mean Absolute Deviation is the average absolute difference between each
;; element and a centerpoint (mean in this case):
;; MAD = average | x - x_mean |
;; >>> (mean_absolute_deviation (list 1.0 2.0 3.0 4.0))
;; 1.0
(define (mean_absolute_deviation numbers)

  (define (mean numbers)
    (/ (apply + numbers) (length numbers)))
  (define (absolute-deviation number)
    (abs (- number (mean numbers))))
  (define (absolute-deviations numbers)
    (map absolute-deviation numbers))
  (mean (absolute-deviations numbers)))

;; Example B
#lang racket

;; For a given list of input numbers, calculate Mean Absolute Deviation
;; around the mean of this dataset.
;; Mean Absolute Deviation is the average absolute difference between each
;; element and a centerpoint (mean in this case):
;; MAD = average | x - x_mean |
;; >>> (mean_absolute_deviation (list 1.0 2.0 3.0 4.0))
;; 1.0
(define (mean_absolute_deviation numbers)

	(define (mean numbers)
		(/ (apply + numbers) (length numbers)))
	(define (absolute_deviation number mean)
		(abs (- number mean)))
	(define (average-absolute-deviations numbers mean)
		(/ (apply + (map (lambda (x) (absolute_deviation x mean)) numbers))
		   (length numbers)))
	(average-absolute-deviations numbers (mean numbers)))
