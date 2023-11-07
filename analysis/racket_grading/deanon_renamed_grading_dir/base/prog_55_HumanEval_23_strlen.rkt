#lang racket

;; Return length of given string
;; >>> (strlen "")
;; 0
;; >>> (strlen "abc")
;; 3
(define (strlen string)

  (if (string=? string "")
      0
      (+ 1 (strlen (substring string 1)))))
