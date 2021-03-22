#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(asdf:load-system :braid-algebra)

;(defparameter *test-braid* '(-3 -3 2 -3 2 1 1 1 -2 1 -2))
(defparameter *test-braid* '(-2 -2 1 -2 1))
(defparameter *guessed-rank* 3)
(defparameter *conjugate-length* 3)

(format t "~a \leq x \leq ~a ~%"
    (rank:lower-bound *test-braid*)
    (rank:upper-bound *test-braid*))

(loop for presentation in (rank:find-band-presentation *test-braid* *guessed-rank* *conjugate-length*) do

 (format t "~{~a~^, ~}" (mapcar #'braid:to-tex-string presentation))
 (format t "~%"))
