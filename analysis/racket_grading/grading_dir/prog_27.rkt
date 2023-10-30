#lang racket

;; Checks if given string is a palindrome
;; >>> (is_palindrome "")
;; #t
;; >>> (is_palindrome "aba")
;; #t
;; >>> (is_palindrome "aaaaa")
;; #t
;; >>> (is_palindrome "zbcd")
;; #f
(define (is_palindrome text)

  (define (is_palindrome_helper text i)
    (cond
      [(= i (string-length text)) #t]
      [(not (char=? (string-ref text i) (string-ref text (- (string-length text) 1 i)))) #f]
      [else (is_palindrome_helper text (+ i 1))]
    )
  )
  (is_palindrome_helper text 0)
)
