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
  "free-word is a sequence of integers. eg: (1 2 -1)"
  (let* ((word (coerce free-word 'vector))
         (N (length word))
         (matrix (make-array (list N N)
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

