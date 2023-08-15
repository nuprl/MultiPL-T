(define (is-palindrome? num)
    (string=? (number->string num) (reverse (number->string num))))
