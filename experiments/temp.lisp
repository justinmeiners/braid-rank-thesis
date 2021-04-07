

(defun map-pair-reps (rank N fn)
  "permutation, upper bound, order of permutation group"
  (let ((pairs (perm:all-pairs N))
        (bands-test (make-array rank :initial-element nil)))

    (perm:do-counting (length pairs)
                rank
                (lambda (index)
                  (loop for i from 0 below (length index) do
                        (setf (aref bands-test i)
                              (elt pairs (aref index i))))

                  (funcall fn (perm:cycles-to-map N bands-test) bands-test)
                  ))))



(defun do-test (length strands)
  (let ((table (make-hash-table :test #'equalp))
        (total 0))

    (map-pair-reps length strands (lambda (x word)
           (push (coerce word 'list) (gethash (perm:map-to-cycles x) table nil))                         
                                    
                                    ))

           
    (maphash (lambda (k v) (format t "~a: ~a  ~%" k (length v))
               (incf total (length v))
               ) table)

    (format t "~a ~%" total)

    ()
    ))


