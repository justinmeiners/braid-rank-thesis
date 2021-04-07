#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(asdf:load-system :braid-algebra)
(load "../paper/data/lisa.lisp")
(load "../paper/data/hughes.lisp")


(in-package :cl-user)

(defun rank-report (braid actual)
  (multiple-value-bind (lower sum perm) (rank:lower-bound braid)
    (let ((upper (rank:upper-bound braid)))
      (format t "~a, ~a. ~a <= ~a <= ~a. ~a. length: ~a.~%"
              sum
              perm
              lower
              (if actual actual "?")
              upper
              (if (evenp perm) "even" "odd")
              (length braid)
              )
      (- upper lower))))


(defun report-info-line ()
  (format t "sum, S^n. L <= rk <= U. ~%"))

(defun make-random-braids (N strands length &key (state (make-random-state t)))
  (loop for i from 0 below N collect
        (list nil (braid:make-random strands length state))))

(defun do-rank-test (all)
  (report-info-line)
  (let ((total (loop for entry in all sum
                     (progn
                       (if (>= (length entry) 2)
                           (format t "~a: " (third entry)))
                       (rank-report (second entry) (first entry)))
                       )))
    (values (float (/ (floor total 2) (length all))) total)))


;(format t "average: ~a" (do-rank-test (make-random-braids 150 7 36)))

(pprint (do-rank-test *hughes-data*))


;(pprint (do-rank-test (remove-if-not (lambda (item) (nth 3 item)) *genus-data*)))

;(pprint (do-rank-test (remove-if-not (lambda (entry)
;                               (= (first entry) (braid:rank-upper-bound (second entry)))
;                               ) *genus-data*)))

;(show-codes)


