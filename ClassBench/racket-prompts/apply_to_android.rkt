#lang racket
(require rackunit)

;; A Phone is one of:
;; - (make-iphone Number)
;; - (make-android String)
;; - (make-pixel Number)
;; Interpretation: A type of smartphone
(define-struct iphone [version] #:transparent)
(define-struct android [version] #:transparent)
(define-struct pixel [version] #:transparent)

;; apply-to-android: [Phone -> Phone] [List-of Phones] -> [List-of Phones]
;; Applies a func to the androids in the list of phones
(define (apply-to-android func lop)
;; <solution>
    (if (empty? lop) '()
    (cons (if (android? (first lop)) (func (first lop)) (first lop)) (apply-to-android func (rest lop)))))

;; <tests>
(check-equal? (apply-to-android (lambda (phone) (make-android (string-upcase (android-version phone)))) 
    '()) '())
(check-equal? (apply-to-android (lambda (phone) (make-android (string-upcase (android-version phone)))) 
    (list (make-iphone 5) (make-pixel 3) (make-android "Cupcake") (make-pixel 7) (make-android "Grape") (make-iphone 10) (make-android "Orange"))) 
    (list (make-iphone 5) (make-pixel 3) (make-android "CUPCAKE") (make-pixel 7) (make-android "GRAPE") (make-iphone 10) (make-android "ORANGE")))
(check-equal? (apply-to-android (lambda (phone) (make-android (list->string (reverse (string->list (android-version phone)))))) 
    (list (make-iphone 5) (make-pixel 3) (make-android "Cupcake"))) (list (make-iphone 5) (make-pixel 3) (make-android "ekacpuC")))
(check-equal? (apply-to-android (lambda (phone) (make-android (string-append (android-version phone) "-v2"))) 
    (list (make-iphone 5) (make-pixel 3))) (list (make-iphone 5) (make-pixel 3)))