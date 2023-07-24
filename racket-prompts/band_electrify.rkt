#lang racket
(require rackunit)
;; A Guitar is a (make-guitar String String Boolean)
;; Interpretation: A (make-guitar brand-name color electric?) represents a 
;; guitar where:
;; - brand-name is the brand name of the guitar
;; - color is the color of the guitar
;; - electric? is a boolean indicating whether the guitar is electric
(define-struct guitar [brand-name color electric?] #:transparent)

;; A DrumKit is a (make-drum-kit String Boolean)
;; Interpretation: A (make-drum-kit brand-name electric?) represents
;; a drum kit where:
;; - brand-name is the brand name of the drum kit
;; - electric? is a boolean indicating whether the drum kit is electric
(define-struct drum-kit [brand-name electric?] #:transparent)

;; A Saxophone is a (make-saxophone String)
;; Interpretation: A (make-saxophone brand-name) represents a saxophone where:
;; - brand-name is the brand name of the saxophone
(define-struct saxophone [brand-name] #:transparent)

;; A Piano is a (make-piano String)
;; Interpretation: A (make-piano brand-name) represents a piano where:
;; - brand-name is the brand name of the piano
(define-struct piano [brand-name] #:transparent)

;; An Instrument is one of:
;; - Guitar
;; - DrumKit
;; - Saxophone
;; - Piano
;; Interpretation: An Instrument is one of the four musical instruments.

;; A Band is one of:
;; - (make-one-piece-band Instrument)
;; - (make-two-piece-band Instrument Instrument)
;; - (make-three-piece-band Instrument Instrument Instrument)
;; Interpretation: A Band represents a band with one, two, or three members.
(define-struct one-piece-band (instrument) #:transparent)
(define-struct two-piece-band (instrument1 instrument2) #:transparent)
(define-struct three-piece-band (instrument1 instrument2 instrument3) #:transparent)

;; electrify-guitar : Guitar -> Guitar
;; Makes this an electric guitar!
(define (electrify-guitar g)
  (make-guitar (guitar-brand-name g) (guitar-color g) #true))

;; electrify-drum-kit : DrumKit -> DrumKit
;; Makes this an electric drum kit!
(define (electrify-drum-kit d)
  (make-drum-kit (drum-kit-brand-name d) #true))

;; electrify-instrument : Instrument -> Instrument
;; Makes this an electric instrument!
(define (electrify-instrument i)
  (cond
    [(guitar? i) (electrify-guitar i)]
    [(drum-kit? i) (electrify-drum-kit i)]
    [(saxophone? i) i]
    [(piano? i) i]))

;; electrify-band : Band -> Band
;; Makes this an electric band!
(define (electrify-band b)
;; <solution>
  (cond
    [(one-piece-band? b)
     (make-one-piece-band (electrify-instrument (one-piece-band-instrument b)))]
    [(two-piece-band? b)
     (make-two-piece-band
      (electrify-instrument (two-piece-band-instrument1 b))
      (electrify-instrument (two-piece-band-instrument2 b)))]
    [(three-piece-band? b)
     (make-three-piece-band
      (electrify-instrument (three-piece-band-instrument1 b))
      (electrify-instrument (three-piece-band-instrument2 b))
      (electrify-instrument (three-piece-band-instrument3 b)))]))


;; <tests>
(check-equal? (electrify-band 
                (make-one-piece-band (make-guitar "Stratocaster" "red" #true))) 
                    (make-one-piece-band (make-guitar "Stratocaster" "red" #true)))
(check-equal? (electrify-band 
                (make-two-piece-band (make-drum-kit "Korg" #true) (make-drum-kit "Korg" #false)))
                    (make-two-piece-band (make-drum-kit "Korg" #true)
                                   (make-drum-kit "Korg" #true)))
(check-equal? (electrify-band 
                (make-three-piece-band (make-guitar "Stratocaster" "red" #true) (make-drum-kit "Korg" #false) (make-saxophone "Yamaha")))
                    (make-three-piece-band (make-guitar "Stratocaster" "red" #true)
                                     (make-drum-kit "Korg" #true)
                                     (make-saxophone "Yamaha")))