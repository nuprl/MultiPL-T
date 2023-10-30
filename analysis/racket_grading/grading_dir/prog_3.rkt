#lang racket

;; For a given string, flip lowercase characters to uppercase and uppercase to lowercase.
;; >>> (flip_case "Hello")
;; "hELLO"
(define (flip_case string)

	(define (flip_case-helper char-list)
		(if (null? char-list)
			'()
			(let ((char (car char-list)))
				(if (char-lower-case? char)
					(cons (char-upcase char) (flip_case-helper (cdr char-list)))
					(cons (char-downcase char) (flip_case-helper (cdr char-list)))))))
	(list->string (flip_case-helper (string->list string))))
