(defpackage :group
  (:use :cl)
  (:export
    iota
    invert
    free-reduce
    to-powers
    merge-adjacent
    distinct-generators
    total-generators
    rank
    ))

(in-package :group)



(declaim (optimize speed))

(defun iota (N)
  (loop for i from 1 to N collect i))
 
(defun invert (a)
  (nreverse (mapcar (lambda (x) (- x)) a)))


(defun merge-adjacent (predicate merge list)
  (let ((result '()))
    (push (reduce
            (lambda (acc x)
              (if (funcall predicate x acc)
                  (funcall merge acc x)
                  (progn
                    (push acc result)
                    x)))
            list) result)
    (nreverse result)))
 

(defun car= (a b) (= (car a) (car b)))

(defun to-powers (word)
  (merge-adjacent 
     #'car=
     (lambda (a b)
       (setf (cdr a) (+ (cdr a) (cdr b)))
       a)
   (mapcar (lambda (x) (cons (abs x) (signum x))) word)))
 

(defun free-reduce (word)
  (let ((result nil))
    (loop for x in word do
          (cond ((= x 0) nil)
                ((and (not (null result))
                      (= (car result) (- x)))
                 (pop result))
                (t (push x result))
                )
          )
    (nreverse result)))

;(defun free-reduce (word)
; (if (null word) nil
;      (let ((head (car word))
;            (reduced (free-reduce (cdr word))))
;        (cond ((null reduced) (cons head reduced))
;              ((= head 0) reduced)
;              ((= (- head) (car reduced)) (cdr reduced))
;              (t (cons head reduced))
;              )
;        )))
 



(defun distinct-generators (word)
  "abelinization"

  (let* ((N (+ 1 (loop for x in word maximize (abs x))))
         (sums (make-array N :initial-element 0)))

    (loop for x in word do
          (incf (aref sums (abs x))
                (signum x)))
    sums))

(defun total-generators (word)
  (reduce (lambda (acc x)
            (+ acc (abs x)))
          (distinct-generators word)
          :initial-value 0))


(defun rank-rule (word start end memo)
  (min 
    (if  (= (aref word start)
            (- (aref word (- end 1))))
         (funcall memo (+ start 1) (- end 1))
         (+ 1 (- end start)))

    (loop for k from (+ start 1) below end minimize 
          (+ (funcall memo start k)
             (funcall memo k end)))))

(defun rank (free-word)
  "exact rank in free group."
  (let* ((word (coerce free-word 'vector))
         (N (length word))
         (matrix (make-array (list N N)
                             :element-type 'fixnum
                             :initial-element 0))
         (memo-lookup (lambda (start end)
                        (aref matrix start (- end 1))))) 

    (loop for i from 0 below N do
          (setf (aref matrix i i) 1))

    (loop for length from 2 to N do
          (do ((i 0 (+ i 1)))
              ((> (+ i length) N) nil)

              (let ((j (+ i (- length 1))))
                (setf (aref matrix i j)
                      (rank-rule word i (+ j 1) memo-lookup)))))

    (values (aref matrix 0 (- N 1)) matrix)))



(defun test ()
  (assert (equalp '(3) (free-reduce '(3 -1 2 -2 1))))

  (assert (= 3 (total-generators '(4 6 -2 6 -4 6 2))))
  (assert (= 3 (rank '(-1 2 2 1 -2 -1 2)))))

(test)

