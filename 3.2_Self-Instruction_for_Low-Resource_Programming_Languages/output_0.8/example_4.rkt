;; is-prime?: Number -> Boolean
;; Checks if a number is prime
(define (is-prime? num)
    (cond
        ((= num 2) #t)
        ((= num 1) #f)
        (else (zero? (remainder (add-odds (repeat "2" (/ num 2))) num)))))
