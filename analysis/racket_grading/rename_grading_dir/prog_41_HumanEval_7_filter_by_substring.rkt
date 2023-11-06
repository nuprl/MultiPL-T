#lang racket

;; Filter an input list of strings only for ones that contain given substring
;; >>> (filter_by_substring (list ) "a")
;; (list )
;; >>> (filter_by_substring (list "abc" "bacd" "cde" "array") "a")
;; (list "abc" "bacd" "array")
(define (filter_by_substring strings substring)

  (filter (lambda (s) (string-contains? s substring)) strings))
