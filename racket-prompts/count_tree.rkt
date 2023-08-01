#lang racket
(require rackunit)

; An OrgChart is one of:
; - (make-person String String [List-of OrgChart])
; - (make-group String [List-of OrgChart])
; Interpretation: an organizational chart
(define-struct person [name title reports]  #:transparent)
(define-struct group [name members] #:transparent)

; num-peeps : OrgChart -> Nat
; counts people in an org chart
(define (num-peeps org)
;; <solution>
  (local [; num-peeps/loo : [List-of OrgChart] -> Nat
          ; how many people are in the list?
          (define (num-peeps/loo loo)
            (foldr (lambda (o sum) (+ (num-peeps o) sum))
                   0
                   loo))]
    (cond
      [(person? org)
       (add1 (num-peeps/loo (person-reports org)))]
      [(group? org) (num-peeps/loo (group-members org))])))

;; <tests>
(check-equal?
 (num-peeps (make-group "test" '())) 0)

(check-equal?
 (num-peeps (make-person "David Madigan"
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
                                               '())))))) 5)

(check-equal?
 (num-peeps (make-group "Cabinet"
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
                                               '())))))))) 7)

(check-equal?
 (num-peeps (make-person "Joseph E. Aoun"
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
                                               '())))))))))) 8)