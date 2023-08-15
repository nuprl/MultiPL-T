(define (is-palindrome? num)
    (equal? (number->string num) (string-reverse (number->string num))))
