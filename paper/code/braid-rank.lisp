(defun commutable-p (braid)
  (let ((min-step 2))
    (reduce
      (lambda (a b)
        (let ((step (abs (- a b))))
          (if (and (not (= step 0)) (< step min-step ))
              (setf min-step step)))
        b)
      (sort (copy-seq braid) #'<))
    (= min-step 2)))

(defun find-conjugate (braid)
  (let* ((N (length braid))
         (best N)
         (start 0)
         (end N))
    (loop for i from 1 below (- N 1) do
          (loop for j from (+ i 1) below N do
                (when (identityp (nconc (subseq braid 0 i)
                                        (subseq braid j)))
                  (when (< (- j i) best) 
                    (setf best (- j i))
                    (setf start i)
                    (setf end j)))

                ))
    (values start end)))

(defun upper-bound-part (braid start end memo)
  (let ((part (subseq braid start end)))
    (multiple-value-bind (i j)
      (find-conjugate part)

      (cond ((identityp part) 0)
            ((commutable-p part) (group:total-generators part))
            (t
             (min 
               (if (< (- j i) (- end start))
                   (funcall memo (+ start i) (+ start j))
                   (- end start))

               (loop for k from (+ start 1) below end minimize 
                   (+ (funcall memo start k)
                      (funcall memo k end))))
             )))))

(defun rank-upper-bound (braid)
  "rank upper bound: modified version of free group rank."
  (let* ((N (length braid))
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
                      (upper-bound-part braid i (+ j 1) memo-lookup)))))

    (values (aref matrix 0 (- N 1)) matrix)))


