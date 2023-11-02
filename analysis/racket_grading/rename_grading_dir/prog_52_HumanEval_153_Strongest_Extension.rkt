#lang racket

;; You will be given the name of a class (a string) and a list of extensions.
;; The extensions are to be used to load additional classes to the class. The
;; strength of the extension is as follows: Let CAP be the number of the uppercase
;; letters in the extension's name, and let SM be the number of lowercase letters 
;; in the extension's name, the strength is given by the fraction CAP - SM. 
;; You should find the strongest extension and return a string in this 
;; format: ClassName.StrongestExtensionName.
;; If there are two or more extensions with the same strength, you should
;; choose the one that comes first in the list.
;; For example, if you are given "Slices" as the class and a list of the
;; extensions: ['SErviNGSliCes', 'Cheese', 'StuFfed'] then you should
;; return 'Slices.SErviNGSliCes' since 'SErviNGSliCes' is the strongest extension 
;; (its strength is -1).
;; Example:
;; >>> (Strongest_Extension "my_class" (list "AA" "Be" "CC"))
;; "my_class.AA"
(define (Strongest_Extension class_name extensions)

	(define (strength ext)
		(let* ([upper (length (filter char-upper-case? (string->list ext)))]
			   [lower (length (filter char-lower-case? (string->list ext)))]
			   [strength (- upper lower)])
			strength))
	(define (strongest? ext1 ext2)
		(if (= (strength ext1) (strength ext2))
			(string<? ext1 ext2)
			(< (strength ext1) (strength ext2))))
	(let* ([strongest (apply max (map strength extensions))]
		   [exts (filter (lambda (ext) (= (strength ext) strongest)) extensions)])
		(string-append class_name "." (car exts))))
