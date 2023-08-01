#lang racket
(require rackunit)

;; A Student is a (make-student String Number)
;; Interpretation: (make-student name nuid) represents a student.
(define-struct student [name nuid] #:transparent)

;; A Grade is a (make-grade Number String Number)
;; Interpretation: (make-grade nuid course grade) represents the grade that
;; a student received in a course.
(define-struct grade [nuid course value] #:transparent)

;; A StudentGrades is a (make-student-grades String [List-of Number]).
;; Interpretation: (make-student-grades name grades) represents the grades
;; that a student has received in all courses.
(define-struct student-grades [name grades] #:transparent)

;; student-grades: [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;; Produces a StudentGrade for each student, with the list of grades that
;; student received.
(define (students->student-grades students grades)
;; <solution>
    (local [;; grades-for-student : Student -> StudentGrades
            ;; looks up all the grades for the supplied student
            (define (grades-for-student student)
              (make-student-grades
                (student-name student)
                (map grade-value
                    (filter
                      (lambda (g) (= (grade-nuid g) (student-nuid student)))
                      grades))))]
    (map grades-for-student students)))

;; <tests>
(check-equal?
 (students->student-grades '() '())
 '())

(check-equal?
 (students->student-grades '() (list  (make-grade 1 "Fundies 1" 95) (make-grade 1 "Psychoceramics" 65) (make-grade 2 "Programming Languages" 85) (make-grade 2 "Fundies 1" 75)))
 '())

(check-equal?
 (students->student-grades (list (make-student "Alice" 1) (make-student "Bob" 2)) '())
 (list (make-student-grades "Alice" '()) (make-student-grades "Bob" '())))

(check-equal?
 (students->student-grades (list (make-student "Alice" 1))
                           (list  (make-grade 1 "Fundies 1" 95) (make-grade 1 "Psychoceramics" 65) (make-grade 2 "Programming Languages" 85) (make-grade 2 "Fundies 1" 75)))
 (list (make-student-grades "Alice" (list 95 65))))

(check-equal?
 (students->student-grades (list (make-student "Alice" 1) (make-student "Bob" 2) (make-student "Carol" 3))
                           (list  (make-grade 1 "Fundies 1" 95) (make-grade 1 "Psychoceramics" 65) (make-grade 2 "Programming Languages" 85) (make-grade 2 "Fundies 1" 75)
                                 (make-grade 3 "Fundies 1" 68) (make-grade 3 "Cybernetics" 82) (make-grade 3 "Phonology" 89) (make-grade 4 "Fundies 1" 55)))
 (list (make-student-grades "Alice" (list 95 65)) (make-student-grades "Bob" (list 85 75)) (make-student-grades "Carol" (list 68 82 89))))
