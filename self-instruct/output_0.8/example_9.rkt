(define (double-even nums)
    (filter (lambda (n) (= (remainder n 2) 0)) nums))
