;; Example A
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

;; Example B
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
      (let loop ((strings strings) (longest-so-far (first strings)) (longest-so-far-length (string-length (first strings))))
        (if (null? (rest strings))
            longest-so-far
            (let ((current-string (first (rest strings))))
              (if (> (string-length current-string) longest-so-far-length)
                  (loop (rest strings) current-string (string-length current-string))
                  (loop (rest strings) longest-so-far longest-so-far-length)))))))
