#lang racket
(require rackunit)

;; A NumTree is one of:
;; - (make-numtree Number NumTree NumTree)
;; - Number
;; Interpretation: A set of numbers arranged in a tree shape.
(define-struct numtree [num left right] #:transparent)

;; mirror: NumTree -> NumTree
;; Mirrors the tree around the center point
(define (mirror tree)
;;<solution>
    (cond
        [(number? tree) tree]
        [(numtree? tree) 
        (make-numtree (numtree-num tree) (mirror (numtree-right tree)) (mirror (numtree-left tree)))]))

;; <tests>
(check-equal? (mirror 5) 5)
(check-equal? (mirror (make-numtree 5 6 7)) (make-numtree 5 7 6))
(check-equal? (mirror (make-numtree 5 (make-numtree 8 9 10) (make-numtree 4 3 2))) (make-numtree 5 (make-numtree 4 2 3) (make-numtree 8 10 9)))