#lang racket
(require rackunit)
;; CompassDirection is one of:
;; - "North"
;; - "Northeast"
;; - "East"
;; - "Southeast"
;; - "South"
;; - "Southwest"
;; - "West"
;; - "Northwest"

(define NORTH "North")
(define NORTHEAST "Northeast")
(define EAST "East")
(define SOUTHEAST "Southeast")
(define SOUTH "South")
(define SOUTHWEST "Southwest")
(define WEST "West")
(define NORTHWEST "Northwest")

;; is-cardinal? : CompassDirection -> Boolean
;; Produces #true when a compass direction is a cardinal direction.
;; The cardinal directions are North, South, East, and West
(define (is-cardinal? cd)
;; <solution>
  (cond
    [(string=? cd NORTH) #true]
    [(string=? cd NORTHEAST) #false]
    [(string=? cd EAST) #true]
    [(string=? cd SOUTHEAST) #false]
    [(string=? cd SOUTH) #true]
    [(string=? cd SOUTHWEST) #false]
    [(string=? cd WEST) #true]
    [(string=? cd NORTHWEST) #false]))

;; <tests>
(check-equal? (is-cardinal? "North") #true)
(check-equal? (is-cardinal? "Northeast") #false)
(check-equal? (is-cardinal? "East") #true)
(check-equal? (is-cardinal? "Southeast") #false)
(check-equal? (is-cardinal? "South") #true)
(check-equal? (is-cardinal? "Southwest") #false)
(check-equal? (is-cardinal? "West") #true)
(check-equal? (is-cardinal? "Northwest") #false)
