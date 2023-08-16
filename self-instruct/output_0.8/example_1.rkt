;; add-rev-nums: [List-of Numbers] -> Number
;; Adds the reverse of all the numbers in a list
(define (add-rev-nums lon)
    (foldr + 0 (map (lambda (n) (string->number (list->string (reverse (string->list (number->string n)))))) lon)))