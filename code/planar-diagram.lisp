
(defpackage :planar-diagram
    (:nicknames :pd)
    (:use :cl :laurent-series)
    (:export
      from-braid
      resolve
      crossing-signs
      jones-series
      *miller-knot*
      )
    )

(in-package :planar-diagram)

(defun iota-array (N)
  (make-array N
              :initial-contents (loop for i from 0 below N collect i)
              :element-type 'fixnum
              ))

(defun all-labels (diagram)
  (let ((all (make-array (* 4 (length diagram))
                         :fill-pointer 0
                         :element-type 'fixnum)))

    (loop for crossing across diagram do
          (loop for x across crossing do
                (vector-push x all)
                ))

    (sort (delete-duplicates all) #'<)
    all
    ))

(defun max-label (diagram)
  (reduce #'max (map 'vector (lambda (crossing)
                               (reduce #'max crossing))
                     diagram)))

(defun crossing-signs (diagram)
  (let* ((N (+ 1 (max-label diagram)))
         (outgoing (make-array N
                               :initial-element -1
                               :element-type 'fixnum)))

    ; mark outgoing
    (map nil (lambda (crossing)
               (setf (aref outgoing (aref crossing 2))
                     1))
         diagram)

    ; infer signs
    (map 'vector
         (lambda (crossing)
            (aref outgoing (aref crossing 3)))
         diagram)))
 

(defun relabel-crossing (cross mapping)
  (map 'vector (lambda (x)
            (cdr (or (assoc x mapping)
                (cons x x))))
          cross))

(defun relabel-strand (diagram mapping)
  (let* ((N (length diagram))
         (result (make-array N :initial-element nil)))
    (dotimes (i N)
      (setf (aref result i) 
            (relabel-crossing (aref diagram i) mapping)
            )
      )
    result))

(defun close-off-braid (top-labels bottom-labels diagram)
  "attach strands at the end back to the origin to form an annular knot"
  (let ((N (length top-labels))
        (d diagram))
    (dotimes (i N)
      (let* ((x (aref top-labels i))
            (y (aref bottom-labels i))
            (replace (list (cons y x))))

        (if (not (= x y))
            (progn
              (setf d (relabel-strand
                        d
                        replace))
              ; replace in top labels
              (if (< y N)
                  (setf top-labels (relabel-crossing top-labels replace))))
            )))
    d))


(defun from-braid (braid)
  (let* ((strands (braid:strands braid))
         (l strands)
         (labeling (iota-array l)))

    (close-off-braid
      (iota-array strands)
      labeling
      (map 'vector (lambda (x)
                     (let ((crossing (if (> x 0)
                                         ; \  /
                                         ;   /
                                         ;  /  \ 
                                         (list (aref labeling (- x 1))
                                               l
                                               (+ l 1)
                                               (aref labeling x))
                                         ;  \  /
                                         ;   \
                                         ; /  \ 
                                         (list
                                           (aref labeling (abs x))
                                           (aref labeling (- (abs x) 1))
                                           l
                                           (+ l 1) ))))

                       (setf x (abs x))
                       (setf (aref labeling (- x 1)) l)
                       (incf l)
                       (setf (aref labeling x) l)
                       (incf l)
                       (make-array 4 :initial-contents crossing)
                       )) 
           braid)
      )))


(defun pair-equalp (pair)
  (= (car pair) (cdr pair)))

(defun fix-labels (diagram)
  "Replace a PD diagram labeling with a canonical one"
  (let ((all (all-labels diagram)))
    ; relabel: sort all labels, and replace them with their index
    (relabel-strand diagram
                    (loop for i from 0 upto (length all) collect
                          (cons (aref all i)
                                i
                                ))
                    )))

(defun smooth-crossing-replacements (crossing type)
  ; 0 \  /  3
  ;     /
  ; 1  /  \ 2


  ; 0 and 1 appear reversed from the paper
  ; because pd diagrams have a different orientation

  (let ((joins
          (if (= type 1)
              ; 1 smoothing
              ; \_/
              ;  _
              ; / \
              (list (cons 1 2)
                    (cons 3 0))

              ; 0 smoothing
              ;\   /
              ; | |
              ;/   \
              (list (cons 0 1)
                    (cons 2 3)))))

    (remove-if #'pair-equalp (mapcar (lambda (pair)
              (let 
                ; lookup strands in crossing
                ((replacement (cons (aref crossing (car pair))
                                    (aref crossing (cdr pair)))))

                ; replace bigger with smaller
                (if (< (car replacement) (cdr replacement))
                    (rotatef (car replacement) (cdr replacement)))

                replacement))
            joins))))

(defun smooth-crossing (diagram type)
  (let ((replacements (smooth-crossing-replacements (aref diagram 0) type)))
    (values (relabel-strand (subseq diagram 1) replacements)
            replacements
            )))


; TOOD: probably slow..
; vector: component (smallest index)
(defun replace-connections (connectivity replacements)
  (let ((output (map 'vector #'identity connectivity))
         (N (length connectivity)))
    (map nil
         (lambda (pair)
           (let* ((replacing (car pair))
                  (old (aref output (car pair)))
                  (smaller (min old (cdr pair)))
                  (larger (max old (cdr pair))))

             (setf (aref output replacing) smaller)
             ;(setf larger smaller)

             (dotimes (i N)
               (if (or (= (aref output i) replacing)
                       (= (aref output i) larger))
                   (setf (aref output i) smaller)
                   )
               )
             ))

         replacements)
    output))


(defun flatten-helper (tree path result)
  (if (consp tree)
      (progn (flatten-helper (car tree) (cons 0 path) result)
             (flatten-helper (cdr tree) (cons 1 path) result))

        (vector-push-extend (cons (reverse path) tree) result)
    ))

(defun flatten-tree (tree)
  (let ((result (make-array 24
                            :adjustable t
                            :fill-pointer 0
                            :initial-element nil
                            )))

    (flatten-helper tree '() result)
    result
    ))

(defun resolve-helper (diagram connectivity)
  (if (= 0 (length diagram)) 
      connectivity
      (cons 
        (multiple-value-bind (smoothed replacements)
          (smooth-crossing diagram 0)
          (resolve-helper smoothed
                          (replace-connections connectivity replacements)))

        (multiple-value-bind (smoothed replacements)
          (smooth-crossing diagram 1)
          (resolve-helper smoothed
                          (replace-connections connectivity replacements)))
        )))

(defun resolve (diagram)
  (flatten-tree (resolve-helper
                  diagram
                  (iota-array (+ 1 (max-label diagram))) )))



(defun jones-part (smoothing)
  (let ((height (reduce #'+ (car smoothing)))
        (cycles (length (remove-duplicates (cdr smoothing)))))

    (series*
        (make-series :order height
                   :coef
                   (make-array 1
                               :initial-element (if (evenp height) 1 -1)
                               :element-type 'number))

        (series-expt (make-series :order -1 :coef #(1 0 1))
                   (- cycles 1)))))

(defun kauffman-normalizer (N_P N_N)
  (let* ((exponent (- N_P (* 2 N_N)))
         (order (min 0 exponent))
         (degree (max 0 exponent))
         (p (make-series-simple degree order)))
    (series-set p exponent (if (evenp N_N) 1 -1))
   p))

(defun jones-series (diagram)
  (let* ((smoothing (resolve diagram))
         (signs (crossing-signs diagram))
         (N (length diagram))
         (N_P (count-if (lambda (x) (> x 0)) signs))
         (N_N (- N N_P)))

    (series-condense
      (series*
        (kauffman-normalizer N_P N_N)
        (series-condense (reduce #'series+
                               (map 'vector #'jones-part smoothing)))))))

; traditional joenes seriesnomial is in terms of t
; ours is in terms of q.
; relationship: q = -t^(1/2)
; then we multiply by normalizing factor

(defun to-unnormalized (p) 
    (series-substitute-var p 1 2))

; trefoil
(defparameter *3_1*
  #(
    #(3 1 4 0)
    #(1 5 2 4)
    #(5 3 0 2)))

(defparameter *4_1*
  #(
    #(3 1 4 0)
    #(7 5 0 4)
    #(5 2 6 3)
    #(1 6 2 7)
    ))

(defparameter *5_1*
  #(
    #(1 7 2 6)
    #(3 9 4 8)
    #(5 1 6 0)
    #(7 3 8 2)
    #(9 5 0 4)
    ))

; miller knot
(defparameter *6_2* 
  #(#(0 8 1 7)
    #(2 9 3 10)
    #(4 2 5 1)
    #(6 0 7 11)
    #(8 3 9 4)
    #(10 6 11 5)))

(defparameter *12n_20*
  #(#(0 4 1 3) #(2 7 3 8) #(4 10 5 9) #(13 6 14 7) #(8 1 9 2) #(17 11 18 10)
  #(5 12 6 13) #(21 14 22 15) #(19 17 20 16) #(11 19 12 18) #(23 20 0 21)
  #(15 22 16 23)))
 

(defparameter *basic*
  #(
    #(3 2 1 0)
    #(0 1 2 3)) )


(defun test () 
  (assert (= (max-label *6_2*)
             11))

  (assert (equalp (crossing-signs *6_2*)
                  #(1 -1 1 1 -1 1)
                  ))

  (assert (equalp #(#(0 0 1 1))
                  (from-braid '(1))))
  (assert (equalp #(#(1 0 0 1))
                  (from-braid '(-1))))
 
  (assert (equalp #(#(0 0 4 1) #(4 1 2 2))
                  (from-braid '(1 2))) )

  (assert (series-equalp (jones-series *3_1*)
                       (to-unnormalized
                         (make-series
                           :order 1
                           :coef #(1 0 1 -1)
                           )
                         )))

  (assert (series-equalp (jones-series *4_1*)
                       (to-unnormalized
                         (make-series
                           :order -2
                           :coef #(1 -1 1 -1 1)
                           )
                         )))

    (assert (series-equalp (jones-series *5_1*)
                       (to-unnormalized
                         (make-series
                           :order 2
                           :coef #(1 0 1 -1 1 -1)
                           )
                         )))


  (assert (series-equalp (jones-series *6_2*)
                       (to-unnormalized
                         (make-series
                           :order -1
                           :coef #(1 -1 2 -2 2 -2 1)
                           )
                         )))

  ; figure 8 (these break mathematica)

  (assert (series-equalp (jones-series #(  #(0 0 1 1) ))
                       (make-series
                         :order 0
                         :coef #(1)
                         )
                       ))

  (assert (series-equalp (jones-series #(  #(0 1 1 0) ))
                       (make-series
                         :order 0
                         :coef #(1)
                         )
                       ))



;(pprint (series-to-string (jones-series #(  #(0 1 1 0) ))))


    ;(assert (series-equalp (jones-series *12n_20*)
    ;                   (to-unnormalized
    ;                     (make-series
    ;                       :order -5
    ;                       :coef #(1 -3 5 -7 8 -7 7 -5 3 -1)
    ;                       )
    ;                     )))

  )

(test)

;(pprint (series-to-string (jones-series *3_1*)))


(defparameter *testing*
  (fix-labels #(#(11 2 12 3) #(22 2 23 1) #(3 10 4 11) #(4 47 5 48) #(48 5 49 6) #(23 6 24 7) #(50 8 51 7) #(8 25 9 26) #(9 47 10 46) #(37 12 38 13) #(16 28 17 27) #(14 35 15 36) #(15 44 16 45) #(13 55 14 54) #(33 18 34 19) #(42 17 43 18) #(19 30 20 31) #(20 39 21 40) #(49 25 50 24) #(56 22 57 21) #(29 35 30 34) #(28 44 29 43) #(26 52 27 51) #(31 58 32 59) #(59 32 60 33) #(36 53 37 54) #(38 56 39 55) #(40 57 41 58) #(60 41 1 42) #(45 52 46 53)))
  )

;(pprint (series-to-string (jones-series *testing*)))



;(pprint (series-to-string (make-series
;                           :order 2
;                           :coef #(1 0 1 -1 1 -1)
;                           )
;))
