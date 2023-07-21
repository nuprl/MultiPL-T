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

;; opposite-direction : CompassDirection -> CompassDirection
;; Produces the opposite direction of a compass direction.
;; i.e North is the opposite of South, and East is the opposite of West
(define (opposite-direction cd)
;; <solution>
  (cond
    [(string=? cd NORTH) SOUTH]
    [(string=? cd NORTHEAST) SOUTHWEST]
    [(string=? cd EAST) WEST]
    [(string=? cd SOUTHEAST) NORTHWEST]
    [(string=? cd SOUTH) NORTH]
    [(string=? cd SOUTHWEST) NORTHEAST]
    [(string=? cd WEST) EAST]
    [(string=? cd NORTHWEST) SOUTHEAST]))

;; <tests>
(check-expect (opposite-direction "North") "South")
(check-expect (opposite-direction "Northeast") "Southwest")
(check-expect (opposite-direction "East") "West")
(check-expect (opposite-direction "Southeast") "Northwest")
(check-expect (opposite-direction "South") "North")
(check-expect (opposite-direction "Southwest") "Northeast")
(check-expect (opposite-direction "West") "East")
(check-expect (opposite-direction "Northwest") "Southeast")