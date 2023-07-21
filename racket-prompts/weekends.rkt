#lang racket
(require rackunit)

;; A CallType is one of:
;; - "zoom"
;; - "teams"
;; - "phone"
;; Interpretation: a type of call

(define CT-ZOOM "zoom")
(define CT-TEAMS "teams")
(define CT-PHONE "phone")

;; An Event is one of:
;; - (make-call CallType PosInt [NEList-of String] String)
;; - (make-mtg PosInt [NEList-of String] String)
;; - (make-alone PosInt String)
;; Interpretation: an event in some period of time, which is either:
;; - A call using some technology, lasting some number of minutes with attendees
;;  (by name), and a description;
;; - An in-person meeting lasting some number of minutes
;;   with attendees (by name) and a description; or
;; - Time spent alone for some number of minutes with a description.
(define-struct call [type duration attendees description] #:transparent)
(define-struct mtg [duration attendees description] #:transparent)
(define-struct alone [duration description] #:transparent)

;; half-hour-rest : Int -> Event
;; produces a 30min rest alone event.
(define (half-hour-rest _)
  (make-alone 30 "rest"))

;; weekend : PosInt -> [NEList-of Event]
;; produces a list of 30min rest alone events of a supplied size
(define (weekend n)
;; <solution>
  (build-list n half-hour-rest))

;; <tests>
(check-equal? (weekend 1) (list (make-alone 30 "rest")))
(check-equal?
 (weekend 3)
 (list (make-alone 30 "rest") (make-alone 30 "rest") (make-alone 30 "rest")))
