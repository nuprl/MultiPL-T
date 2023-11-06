#lang racket

;; From a given list of integers, generate a list of rolling maximum element found until given moment
;; in the sequence.
;; >>> (rolling_max (list 1 2 3 2 3 4 2))
;; (list 1 2 3 3 3 4 4)
(define (rolling_max numbers)

  (define (max_list numbers)
    (if (null? numbers)
        '()
        (cons (apply max numbers) (max_list (cdr numbers)))))
  (reverse (max_list (reverse numbers))))
