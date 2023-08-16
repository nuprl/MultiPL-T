;; palindrome-substrings: String -> [List-of Strings]
;; Returns all the palindrome substrings
(define (palindrome-substrings str)
    (let ((s (string->list str)) (ret '()))
        (let pal-helper ((i 0) (j 0) (c '()))
            (if (= i (length s)) (append ret (list (list->string c)))
            (if (= i j) (pal-helper (+ i 1) (+ i 1) (append c (list (list-ref s i))))
            (if (equal? (list-ref s i) (list-ref s j)) (pal-helper i (+ j 1) (append c (list (list-ref s i))))
            (pal-helper i (+ j 1) (append c (list (list-ref s i))))))))))
