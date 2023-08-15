;; 6. Strings and Characters
;; --------------------------

#lang racket

;; count-characters: [List-of Strings] -> [List-of Pairs]
;; Counts all the characters in the list
(define (count-characters lon)
    (foldr (lambda (str acc)
            (foldr