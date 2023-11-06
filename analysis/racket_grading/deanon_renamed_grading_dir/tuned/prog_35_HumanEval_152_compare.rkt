#lang racket

;; I think we all remember that feeling when the result of some long-awaited
;; event is finally known. The feelings and thoughts you have at that moment are
;; definitely worth noting down and comparing.
;; Your task is to determine if a person correctly guessed the results of a number of matches.
;; You are given two lists of scores and guesses of equal length, where each index shows a match. 
;; Return a list of the same length denoting how far off each guess was. If they have guessed correctly,
;; the value is 0, and if not, the value is the absolute difference between the guess and the score.
;; example:
;; >>> (compare (list 1 2 3 4 5 1) (list 1 2 3 4 2 -2))
;; (list 0 0 0 0 3 3)
;; >>> (compare (list 0 5 0 0 0 4) (list 4 1 1 0 0 -2))
;; (list 4 4 1 0 0 6)
(define (compare game guess)

	(map (lambda (g s) (if (= g s) 0 (abs (- g s)))) guess game)
)
