#lang racket

;; Concatenate list of strings into a single string
;; >>> (concatenate (list ))
;; ""
;; >>> (concatenate (list "a" "b" "c"))
;; "abc"
(define (concatenate strings)

  (apply string-append strings))
