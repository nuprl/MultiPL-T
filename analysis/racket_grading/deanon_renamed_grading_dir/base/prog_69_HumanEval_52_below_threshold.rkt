#lang racket

;; Return #t if all numbers in the list l are below threshold t.
;; >>> (below_threshold (list 1 2 4 10) 100)
;; #t
;; >>> (below_threshold (list 1 20 4 10) 5)
;; #f
(define (below_threshold l t)

  (cond
    [(empty? l) #t]
    [(< (first l) t) (below_threshold (rest l) t)]
    [else #f]))
