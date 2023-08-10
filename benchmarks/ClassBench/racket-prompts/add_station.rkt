#lang racket
(require rackunit)

;; A Station is a (make-station String [List-of String])
;; Interpretation: The name of a station and the names of stations that directly
;; connect to it.
(define-struct station [name connections] #:transparent)

;; A Subway is a [List-of Station]
;; Interpretation: A list of stations that make a subway network.

;; add-edge-to-station : String Station -> Station
;; Adds a single edge to a station if it does not already exist.
(define (add-edge-to-station t s)
  (if (ormap (lambda (t2) (string=? t t2)) (station-connections s))
      s
      (make-station (station-name s) (cons t (station-connections s)))))

;; make-connection : String String Subway -> Subway
;; Adds a connection to the subway, if it does not
;; already exist. Assumes that both from and to stations in the Subway.
(define (make-connection from to subway)
;; <solution>
  (cond
    [(empty? subway) subway]
    [(cons? subway)
     (if (string=? from (station-name (first subway)))
         (cons (add-edge-to-station to (first subway)) (rest subway))
         (cons (first subway) (make-connection from to (rest subway))))]))

;; <tests>
(check-equal?
 (make-connection "Fenway" "Kenmore" '()) '())

(check-equal?
 (make-connection "Fenway" "Kenmore" (list 
        (make-station "Newton Centre" (list "Fenway" "Kenmore"))
        (make-station "Fenway" (list "Newton Highlands" "Newton Centre"))
        (make-station "Kenmore" (list "Newton Centre"))
        (make-station "Newton Highlands" (list "Fenway"))))
 (list (make-station "Newton Centre" (list "Fenway" "Kenmore"))
       (make-station "Fenway" (list "Kenmore" "Newton Highlands" "Newton Centre"))
       (make-station "Kenmore" (list "Newton Centre"))
       (make-station "Newton Highlands" (list "Fenway"))))

(check-equal? (make-connection "Kenmore" "Newton Centre" (list 
            (make-station "Newton Centre" (list "Fenway" "Kenmore"))
            (make-station "Fenway" (list "Newton Highlands" "Newton Centre"))
            (make-station "Kenmore" (list "Newton Centre"))
            (make-station "Newton Highlands" (list "Fenway")))) 
                (list 
                    (make-station "Newton Centre" (list "Fenway" "Kenmore"))
                    (make-station "Fenway" (list "Newton Highlands" "Newton Centre"))
                    (make-station "Kenmore" (list "Newton Centre"))
                    (make-station "Newton Highlands" (list "Fenway"))))

(check-equal? (make-connection "A" "C" (list (make-station "A" (list "B" "D"))
                          (make-station "B" (list "C" "A"))
                          (make-station "C" (list "D" "B"))
                          (make-station "D" (list "A" "C"))))
              (list (make-station "A" (list "C" "B" "D"))
                    (make-station "B" (list "C" "A"))
                    (make-station "C" (list "D" "B"))
                    (make-station "D" (list "A" "C"))))