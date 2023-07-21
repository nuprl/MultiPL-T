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

;; small-list? : [List-of String] -> Boolean
;; does the list contain fewer than 3 elements?
(define (small-list? los)
  (< (length los) 3))

;; small-group? : Event -> Boolean
;; does the event have fewer than 3 participants?
(define (small-group? e)
  (small-list?
   (cond
     [(call? e) (cons "me" (call-attendees e))]
     [(mtg? e) (cons "me" (mtg-attendees e))]
     [(alone? e) (cons "me" '())])))

;; small-group : [List-of Event] -> [List-of Event]
;; produces the list of only those events that have up to 3 participants
;; (including the implied author).
(define (small-group loe)
;; <solution>
  (filter small-group? loe))

;; <tests>
(check-equal? (small-group '()) '())
(check-equal? (small-group 
    (list (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :(") 
             (make-alone 25 "The Three-Body Problem isn't going to read itself!") 
             (make-call CT-PHONE 1 (list "Unknown") "Who calls!? I think it was a scam...")
                (make-alone 34 "Lunch") 
                (make-call CT-TEAMS 7 (list "Mike" "Tajel")
             "Office hours with my partner to ask clarifying questions about the Design Recipe!") 
                (make-mtg 28 (list "Ali")
            "Meeting with advisor to talk about a combined major")
                 (make-mtg 62 (list "Rachel" "Ross" "Joey" "Phoebe" "Chandler" "Monica")
            "Getting ahead on studying for Exam 2!")))
              (list (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
                "Doctor appointment about a stomach ache after some bad seafood :(") 
                (make-alone 25 "The Three-Body Problem isn't going to read itself!") 
                (make-call CT-PHONE 1 (list "Unknown")
                "Who calls!? I think it was a scam...")
                        (make-alone 34 "Lunch") (make-mtg 28 (list "Ali")
            "Meeting with advisor to talk about a combined major")))