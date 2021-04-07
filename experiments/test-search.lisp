#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(asdf:load-system :braid-algebra)

; WORKS

;(defparameter *test-braid* '(-4 -2 -3 -2 -3 1 3 2 3 1 1 -5 -1 -1 2))
;(defparameter *test-rank* 3)
;(pprint (braid:equivp *test-braid* (append '(-4) '(-5) '(-2 -3 -2 -3 1 3 2 3 2))))
;(pprint (braid:to-cycles '(-2 -3 -2 -3 1 3 2 3 2)))
 
;(defparameter *test-braid* '(2 3 1 -2 1 2 -3))
;(defparameter *test-rank* 3)
 

;(defparameter *test-braid* '(-2 4 -5 -4 -6 2 1 1 -2 -3 2 -4 -1))
;(defparameter *test-rank* 5)

;(defparameter *test-braid* '(2 3 -3 -1 -2 -1 3 1 -4 -1 -3 1 2 1))
;(defparameter *test-rank* 2)

; (defparameter *test-braid* '(-3 -1 -2 3 -1 3 1 -3 2 1 3 1 3 3 -3 -1 -1 -2 1 3 3 1 -3 -3 -1 2 1))
; (defparameter *test-rank* 3)
 
;(defparameter *test-braid* '(-5 -3 -1 4 -6 -4 1 1 -4 3))
;(defparameter *test-rank* 4)

(defparameter *test-braid* '(1 -3 -1 3 1 3 2 -3 -2 1 2 3 -2 -3 1 3))
(defparameter *test-rank* 4)

;(defparameter *test-braid* '(-1 3 1 -3 1 3 3 1 2 -1 2))
;(defparameter *test-rank* 5)

 ;(defparameter *test-braid* '(2 3 -3 -1 -2 -1 3 1 -4 -1 -3 1 2 1))
;(defparameter *test-rank* 2)
 
 
; TEST

;(defparameter *test-braid* '(2 3 -3 -1 -2 -1 3 1 -4 -1 -3 1 2 1))
;(defparameter *test-rank* 2)
 

;12n 512
;(defparameter *test-braid* '(-1 -2 -3 1 -3 -2 -2 1 -2 3 -2 -1 2))
;(defparameter *test-rank* 5)


; 12n 239
;(defparameter *test-braid* '(1 -2 1 2 -3 -3 -3 -3 2 2 3 3 3))
;(defparameter *test-rank* 5)

(pprint (rank:find-band-presentation *test-braid* *test-rank* 3))

