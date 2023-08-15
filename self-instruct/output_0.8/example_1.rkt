;; https://github.com/racket-tw/code/blob/master/racket/sum-lists.rkt
;; https://github.com/racket-tw/code/blob/master/racket/sum-lists.rkt
;; https://github.com/racket-tw/code/blob/master/racket/sum-lists.rkt

#lang racket

;; sum-lists: [List-of Lists-of Numbers] -> [List-of Numbers]
;; Adds all the numbers in the lists together
(define (sum-lists lls)
    (map (lambda (l) (foldr + 