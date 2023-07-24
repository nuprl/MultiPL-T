#lang racket
(require rackunit)

; An OrgChart is one of:
; - (make-person String String [List-of OrgChart])
; - (make-group String [List-of OrgChart])
; Interpretation: an organizational chart
(define-struct person [name title reports] #:transparent)
(define-struct group [name members] #:transparent)

; full-title: OrgChart String -> OrgChart
; Adds the name of the organization in parenthesis
; to every person in the org chart.
(define (full-title org name)
;; <solution>
  (local [(define F (λ (o) (full-title o name)))]
    (cond
      [(person? org)
       (make-person
        (person-name org)
        (string-append
         (person-title org) " (" name ")")
        (map F (person-reports org)))]
      [(group? org)
       (make-group
        (group-name org)
        (map F (group-members org)))])))

;; <tests>
(check-equal?
 (full-title (make-person "Joseph E. Aoun"
               "President"
               (list (make-group "Cabinet"
              (list (make-person "Karl Reid"
                                 "Senior Vice Provost and Inclusion Officer"
                                 '())
                    (make-person "Madeleine Estabrook"
                                 "Senior Advisor for Global Student Experience"
                                 '())
                    (make-person "David Madigan"
               "Provost and Senior Vice President for Academic Affairs"
               (list
                (make-group "Administration"
                            (list (make-person "Thomas Sheahan"
                                               "Senior Vice Provost, Curriculum and Programs" '())))
                (make-group "Academic Deans"
                            (list (make-person "Alan Mislove"
                                               "Interim Dean, Khoury College of Computer Sciences"
                                               '())
                                  (make-person "Carmen Sceppa"
                                               "Dean, Bouvé College of Health Sciences"
                                               '())
                                  (make-person "Uta G. Poiger"
                                               "Dean, College of Social Sciences & Humanities"
                                               '()))))))))) "Northeastern University")
 (make-person
  "Joseph E. Aoun"
  "President (Northeastern University)"
  (list
   (make-group
    "Cabinet"
    (list
     (make-person "Karl Reid"
                  "Senior Vice Provost and Inclusion Officer (Northeastern University)"
                  '())
     (make-person "Madeleine Estabrook"
                  "Senior Advisor for Global Student Experience (Northeastern University)"
                  '())
     (make-person
      "David Madigan"
      "Provost and Senior Vice President for Academic Affairs (Northeastern University)"
      (list
       (make-group
        "Administration"
        (list (make-person
               "Thomas Sheahan"
               "Senior Vice Provost, Curriculum and Programs (Northeastern University)" '())))
       (make-group
        "Academic Deans"
        (list (make-person
               "Alan Mislove"
               "Interim Dean, Khoury College of Computer Sciences (Northeastern University)"
               '())
              (make-person
               "Carmen Sceppa"
               "Dean, Bouvé College of Health Sciences (Northeastern University)"
               '())
              (make-person
               "Uta G. Poiger"
               "Dean, College of Social Sciences & Humanities (Northeastern University)"
               '()))))))))))