#lang racket
(require rackunit)

;; A Station is a (make-station String [List-of String])
;; Interpretation: The name of a station and the names of stations that directly
;; connect to it.
(define-struct station [name connections] #:transparent)

;; A Subway is a [List-of Station]
;; Interpretation: A list of stations that make a subway network.

;; A [List-of-2-or-more X] is one of:
;; - (cons X (cons X '()))
;; - (cons X [List-of-2-or-more X])
;; A list with two or more items.

;; add-forward-edges : Subway [List-of-2-or-more String] -> Subway
;; Adds the list of stations to the subway, in one direction.
(define (add-forward-edges subway lo2om)
  (cond
    [(empty? (rest (rest lo2om))) 
     (add-edge (first lo2om) (first (rest lo2om)) subway)]
    [(cons? (rest (rest lo2om)))
     (add-forward-edges
      (add-edge (first lo2om) (first (rest lo2om)) subway)
      (rest lo2om))]))

;; add-edge : String String Subway -> Subway
;; (add-edge from to station) adds a connection to the subway, if it does not
;; already exist. Assumes that both from and to stations in the Subway.
(define (add-edge from to subway)
  (cond
    [(empty? subway) subway]
    [(cons? subway)
     (if (string=? from (station-name (first subway)))
         (cons (add-edge-to-station to (first subway)) (rest subway))
         (cons (first subway) (add-edge from to (rest subway))))]))

;; line->subway : [List-of-2-or-more String] -> Subway
;; Given a list of stations on a line, produces a Subway.
(define (line->subway line)
;; <solution>
  (local ([define disconnected-stations
            (map (λ (n) (make-station n '())) line)]
          
          ;; Each connects to the next station
          [define forward-connections-added
            (add-forward-edges disconnected-stations line)]
          
          ;; Each connects to the previous station
          [define backward-connections-added
            (add-forward-edges forward-connections-added (reverse line))])
    backward-connections-added))

;; <tests>
(check-equal? 
 (line->subway (list "A" "B"))
 (list (make-station "A" (list "B"))
       (make-station "B" (list "A"))))

(check-equal? 
 (line->subway (list "A" "B" "C"))
 (list (make-station "A" (list "B"))
       (make-station "B" (list "A" "C"))
       (make-station "C" (list "B"))))