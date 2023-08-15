;; Problem #2
;; 1. Define a function that takes a string and returns the string reversed
(define (reverse-string str)
    (foldl string-append "" (reverse (string->list