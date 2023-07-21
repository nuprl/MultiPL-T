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
(check-expect (is-cardinal? "North") #true)
(check-expect (is-cardinal? "Northeast") #false)
(check-expect (is-cardinal? "East") #true)
(check-expect (is-cardinal? "Southeast") #false)
(check-expect (is-cardinal? "South") #true)
(check-expect (is-cardinal? "Southwest") #false)
(check-expect (is-cardinal? "West") #true)
(check-expect (is-cardinal? "Northwest") #false)
