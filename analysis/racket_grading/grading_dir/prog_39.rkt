#lang racket

;; Out of list of strings, return the longest one. Return the first one in case of multiple
;; strings of the same length. Return #f in case the input list is empty.
;; >>> (longest (list ))
;; #f
;; >>> (longest (list "a" "b" "c"))
;; "a"
;; >>> (longest (list "a" "bb" "ccc"))
;; "ccc"
(define (longest strings)

	(if (null? strings)
		#f
		(let ((max_length 0) (max_string ""))
			(for-each (lambda (s)
				(when (> (string-length s) max_length)
					(set! max_length (string-length s))
					(set! max_string s)))
				strings)
			max_string)))
