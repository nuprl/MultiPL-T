#lang racket
(require rackunit)

;; A NumTree is one of:
;; - (make-numtree Number NumTree NumTree)
;; - Number
;; Interpretation: A set of numbers arranged in a tree shape.
(define-struct numtree [num left right] #:transparent)

;; add-left: NumTree -> Number
;; Adds only the numbers on the leftmost side of the tree.
(define (add-left tree)
;; <solution>
    (cond 
        [(number? tree) tree]
        [(numtree? tree) (+ (numtree-num tree) (add-left (numtree-left tree)))]))

;; <tests>
(check-equal? (add-left 4) 4)
(check-equal? (add-left (make-numtree 5 6 7)) 11)
(check-equal? (add-left (make-numtree -3 (make-numtree 3 0 9) (make-numtree 4 9 10))) 0)