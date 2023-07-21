;; A DNASeq is one of:
;; - (make-adenine DNASeq)
;; - (make-guanine DNASeq)
;; - (make-cytosine DNASeq)
;; - (make-thymine DNASeq)
;; - "empty sequence"
;; Interpretation: A DNASeq represents a sequence of nucleotides.
(define-struct adenine [rest])
(define-struct guanine [rest])
(define-struct cytosine [rest])
(define-struct thymine [rest])

;; Every  DNA sequence has a complementary sequence, which substitutes
;; As with Ts, Ts with As, Cs with Gs, and Gs with Cs. Design a function
;; to calculate the complement of a DNA sequence.

;; dna-complement : DNASeq -> DNASeq
;; Computes the complement of a DNA sequence.
(define (dna-complement d)
;; <solution>
  (cond
    [(adenine? d) (make-thymine (dna-complement (adenine-rest d)))]
    [(guanine? d) (make-cytosine (dna-complement (guanine-rest d)))]
    [(cytosine? d) (make-guanine (dna-complement (cytosine-rest d)))]
    [(thymine? d) (make-adenine (dna-complement (thymine-rest d)))]
    [(string? d) "empty sequence"]))

;; <tests>
(check-expect (dna-complement "empty sequence") "empty sequence")
(check-expect (dna-complement (make-adenine "empty sequence")) (make-thymine "empty sequence"))
(check-expect (dna-complement (make-thymine (make-cytosine (make-guanine (make-adenine "empty sequence")))))
              (make-adenine (make-guanine (make-cytosine (make-thymine "empty sequence")))))