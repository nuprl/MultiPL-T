;; is-palindrome-char?: Character -> Boolean
;; Checks if a character is a palindrome
(define (is-palindrome-char? char)
    (equal? char (string-ref (number->string (char->integer char)) 0)))
