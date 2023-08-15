(define (sum-proper-divisors num)
  (foldr + 0 (filter (lambda (n) (not (zero? (remainder num n)))) (proper-divisors num))))