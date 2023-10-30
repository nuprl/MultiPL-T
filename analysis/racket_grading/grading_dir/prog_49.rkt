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
