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

	(define (aux text start end)
		(if (< start end)
			(and (char=? (string-ref text start) (string-ref text end))
				 (aux text (+ start 1) (- end 1)))
			#t
		)
	)
	(aux text 0 (- (string-length text) 1))
)
