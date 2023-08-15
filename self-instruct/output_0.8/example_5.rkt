(define (palindrome-check-helper list-of-nums)
    (cond
        [(null? list-of-nums) '()]
        [(= (length list-of-nums) 1) (list (car list-of-nums))]
        [(watching-you? (+ (car list-of-nums) (cadr list-of-nums)))
            (cons (+ (car list-of-nums) (cadr list-of-nums))
                (palindrome-check-helper (cddr list-of-nums)))]
        [else (palindrome-check-helper (cdr list-of-nums))]))
