(defpackage :lawrence-krammer
    (:nicknames :lk)
    (:use :cl)
    (:export identityp))

(in-package :lk)


; LAWRENCE KRAMMER MATRIX REP

; https://en.wikipedia.org/wiki/Algebraic_independence
(defparameter *q* pi)
; Gamma(1/3)
(defparameter *t* 2.67893853470774763365569294097d0)
;(defparameter *t* (exp pi))

(declaim (type double-float *q* *t*))


(declaim (ftype (function (* * *) fixnum) row-index))

(defun row-index (n j k)
  (declare (type (signed-byte 30) n j k))
  (- (+
       (* (- j 1) (- n (/ j 2)))
       (- k j))
     1))

(deftype lk-matrix () '(simple-array double-float (* *)))


(declaim (ftype (function (fixnum fixnum) lk-matrix) lk-matrix-rep))

; Lawrence Krammer
; https://en.wikipedia.org/wiki/Lawrence%E2%80%93Krammer_representation
(defun lk-matrix-rep (i n)
  "i is braid generator index. n is number of strands in braid group"

  (declare (type fixnum i n))

  (if (>= i n )
      (error "braid index must be smaller than strands")
      )

  (let* ((rank (perm:n-choose-2 n))
         (m (make-array (list rank rank) :initial-element 0d0 :element-type 'double-float)))

    (loop for j from 1 below n do
          (loop for k from (+ j 1) to n do
                (let ((IN (row-index n j k)))
                  (cond 
                        ((= i (- j 1))
                         (setf (aref m (row-index n i k) IN) *q*)
                         (setf (aref m (row-index n i j) IN) (* *q* (- *q* 1d0)) )
                         (setf (aref m IN IN) (- 1d0 *q*))) 
                        ((= i j (- k 1))
                         (setf (aref m IN IN) (* -1d0 *t* *q* *q*))) 
                        ((= i j)
                         (setf (aref m (row-index n (+ j 1) k) IN) 1d0))
                        ((and (= i (- k 1)) (not (= i j)))
                         (setf (aref m (row-index n j i) IN) *q*)
                         (setf (aref m (row-index n i k) IN) (* -1d0 *t* *q* (- *q* 1d0)) )
                         (setf (aref m IN IN) (- 1d0 *q*))  
                         )
                        ((= i k)
                         (setf (aref m (row-index n j (+ k 1)) IN) 1d0)
                         )
                        (t 
                         (setf (aref m IN IN) 1d0)
                         )
                        ))
                ))

    m))





(defun lk-matrix-zerop (A)
  (declare (type (simple-array *) A))

  (< (loop for i from 0 below (array-total-size A) maximize
        (abs (row-major-aref A i)))
        0.001d0
     ))

(declaim (ftype (function (fixnum) lk-matrix) lk-matrix-identity))

(defun lk-matrix-identity (strands)
  (the lk-matrix (matrix:make-identity (perm:n-choose-2 strands) 0d0 1d0)))


(defun gen-to-matrix (gen strands)
  (declare (type fixnum gen strands))

  (let ((M (lk-matrix-rep (abs gen) strands)))
    (if (< gen 0)
        (let ((M-inverted (make-array (array-dimensions M)
                                      :initial-element 0d0
                                      :element-type 'double-float)))
          (matrix:invert M M-inverted)
          M-inverted)
        M
        )))


(defun gen-to-matrix-cached (gen strands cache)
  (multiple-value-bind (matrix present) (gethash (cons gen strands) cache)
    (when (not present)
        (setf matrix (gen-to-matrix gen strands))
        (setf (gethash (cons gen strands) cache) matrix))

    matrix))

(declaim (ftype (function (* *) lk-matrix) braid-to-matrix))

(defun to-matrix (braid strands cache)
  (let ((temp (lk-matrix-identity strands)))
    (reduce (lambda (acc gen)
            (prog1 (matrix:mult acc (gen-to-matrix-cached gen strands cache) temp)
              (setf temp acc)
              )
            )
          braid
          :initial-value (lk-matrix-identity strands))))


(defun identityp (braid &key (cache (make-hash-table :test #'equalp)))
  "the word problem for braid groups"

  (let ((N (braid:strands braid)))
    (lk-matrix-zerop (matrix:sub (to-matrix braid N cache)
                                 (lk-matrix-identity N)
                                 (lk-matrix-identity N)
                                 ))))

 
