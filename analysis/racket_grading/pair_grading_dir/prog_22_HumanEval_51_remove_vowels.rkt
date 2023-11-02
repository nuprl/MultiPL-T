;; Example A
#lang racket

;; remove_vowels is a function that takes string and returns string without vowels.
;; >>> (remove_vowels "")
;; ""
;; >>> (remove_vowels "abcdef")
;; "bcdf"
;; >>> (remove_vowels "aaaaa")
;; ""
;; >>> (remove_vowels "aaBAA")
;; "B"
;; >>> (remove_vowels "zbcd")
;; "zbcd"
(define (remove_vowels text)

	(define vowels '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
	(define new_text '())
	(for ([letter (in-string text)])
		(unless (member letter vowels)
			(set! new_text (append new_text (list letter)))))
	(list->string new_text))

;; Example B
#lang racket

;; remove_vowels is a function that takes string and returns string without vowels.
;; >>> (remove_vowels "")
;; ""
;; >>> (remove_vowels "abcdef")
;; "bcdf"
;; >>> (remove_vowels "aaaaa")
;; ""
;; >>> (remove_vowels "aaBAA")
;; "B"
;; >>> (remove_vowels "zbcd")
;; "zbcd"
(define (remove_vowels text)

  (define vowels '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
  (define (remove_vowels_helper text vowels)
    (cond
      [(empty? text) '()]
      [(member (first text) vowels) (remove_vowels_helper (rest text) vowels)]
      [else (cons (first text) (remove_vowels_helper (rest text) vowels))]
    )
  )
  (list->string (remove_vowels_helper (string->list text) vowels))
)
