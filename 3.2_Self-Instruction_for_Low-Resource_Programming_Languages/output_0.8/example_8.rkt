;; is-prime?: Number -> Boolean
;; Checks if a number is a prime
(define (is-prime? num)
    (cond
        [(= num 0) #f]
        [(= num 1) #f]
        [(= num 2) #t]
        [(= (remainder num 2) 0) #f]
        [else
            (define prime?
                (lambda (n)
                    (define (divides? a b)
                        (= (remainder a b) 0))

                    (define (iter count)
                        (if (> count (sqrt n))
                            #t
                            (if (divides? n count)
                                #f
                                (iter (+ 1 count)))))
                    (iter 3)))
            (prime? num)]))
