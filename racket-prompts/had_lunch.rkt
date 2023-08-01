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

;; had-lunch? : [List-of Event] -> Boolean
;; Is there an event containing the word lunch?
(define (had-lunch? loe)
;; <solution>
  (local [(define (is-lunch? e)
    (string-contains? (string-downcase (cond
      [(call? e) (call-description e)]
      [(mtg? e) (mtg-description e)]
      [(alone? e) (alone-description e)])) "lunch"))]
    (ormap is-lunch? loe)))

;; <tests>
(check-equal? (had-lunch? '()) #false)
(check-equal? (had-lunch? (list (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :(") 
             (make-alone 25 "The Three-Body Problem isn't going to read itself!") 
             (make-call CT-PHONE 1 (list "Unknown") "Who calls!? I think it was a scam...")
                (make-alone 34 "Lunch") 
                (make-call CT-TEAMS 7 (list "Mike" "Tajel")
             "Office hours with my partner to ask clarifying questions about the Design Recipe!") 
                (make-mtg 28 (list "Ali")
            "Meeting with advisor to talk about a combined major")
                 (make-mtg 62 (list "Rachel" "Ross" "Joey" "Phoebe" "Chandler" "Monica")
            "Getting ahead on studying for Exam 2!"))) #true)
(check-equal? (had-lunch? (list (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :("))) #false)
(check-equal?
 (had-lunch? (list (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :(")
                   (make-call CT-ZOOM 20 (list "Mom") "Virtual lunch")))
 #true)
(check-equal?
 (had-lunch? (list (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :(")
                   (make-mtg 50 (list "Alice" "Bob") "GROUP LUNCH")))
 #true)