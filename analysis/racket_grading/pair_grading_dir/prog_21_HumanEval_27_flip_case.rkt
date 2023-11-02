;; Example A
#lang racket

;; For a given string, flip lowercase characters to uppercase and uppercase to lowercase.
;; >>> (flip_case "Hello")
;; "hELLO"
(define (flip_case string)

  (define (flip_case_helper string)
    (cond
      [(empty? string) '()]
      [(char-lower-case? (first string)) (cons (char-upcase (first string)) (flip_case_helper (rest string)))]
      [(char-upper-case? (first string)) (cons (char-downcase (first string)) (flip_case_helper (rest string)))]
      [else (cons (first string) (flip_case_helper (rest string)))]))
  (list->string (flip_case_helper (string->list string))))

;; Example B
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
