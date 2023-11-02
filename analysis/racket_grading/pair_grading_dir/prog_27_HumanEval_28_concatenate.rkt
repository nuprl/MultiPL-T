;; Example A
#lang racket

;; Concatenate list of strings into a single string
;; >>> (concatenate (list ))
;; ""
;; >>> (concatenate (list "a" "b" "c"))
;; "abc"
(define (concatenate strings)

	(cond
		((null? strings) "")
		((null? (cdr strings)) (car strings))
		(else (string-append (car strings) (concatenate (cdr strings))))))

;; Example B
#lang racket

;; Concatenate list of strings into a single string
;; >>> (concatenate (list ))
;; ""
;; >>> (concatenate (list "a" "b" "c"))
;; "abc"
(define (concatenate strings)

  (apply string-append strings))
