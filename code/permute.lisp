(defpackage :permutate
    (:nicknames :perm)
    (:use :cl)
    (:export make-all-maps
             map-equiv-p
             map-identity-p
             eval-map
             map-to-cycles
             cycles-dimension
             eval-cycle
             cycles-to-map
             cycles-to-pairs
             cycles-evenp
             factorial
             n-choose-2
             nrotate-cycle
             permute-set
             arrangements
             map-pair-reps
             rank-cycles
             all-pairs
             do-counting
            ))

(in-package :permutate)

(declaim (inline factorial n-choose-2 eval-map build-cycle-helper to-pairs-helper))



; TODO: optimize
(defun factorial (n &key (stop 1))
  (prog ((p 1))
        loop
        (if (<= n stop)
            (return p))
        (setq p (* p n))
        (decf n)
        (go loop)))

; TODO: optimize
(defun n-choose-2 (N)
  (floor (* N (- N 1)) 2))

(defun permute-set (words)
  (if (null words)
      '(())
      (loop for x in words append
            (mapcar (lambda (tail) (cons x tail))
                    (permute-set (remove x words)))
            )))

(defun make-all-maps (n)
  (declare (type fixnum n))
    "permutation (k_1 .. k_n) maps i to k_i"
    (permute-set (loop for i from 1 to n collect i)))

(defun map-equiv-p (a b)
  (equalp a b))

(defun map-identity-p (map)
  (prog ((i 1))
        loop
        (if (not (= (car map) i))
            (return nil))

        (setf map (cdr map))
        (incf i)
        (if (not (null map))
            (go loop))
        (return t)))
 
(defun eval-map (x map)
  (elt map (- x 1)))

(defun build-cycle-helper (mark start p)
  "builds a cycle starting at start"
  (let ((cycle (list start)))
    (do ((x (eval-map start p) (eval-map x p)))
        ((= x start) (nreverse cycle))
        (push x cycle)
        (setf (elt mark (- x 1)) 1)
        )))

(defun map-to-cycles (p)
  "cycle notation from abstract algebra eg. (123) 1 -> 2, 2 -> 3, 3 -> 1"
  (let* ((N (length p))
         (mark (make-array N :initial-element 0)))
    (remove-if
      (lambda (cycle)
        (<= (length cycle ) 1))

      (loop for i from 0 below N collect
                  (if (= 1 (aref mark i))
                      '()
                      (build-cycle-helper mark (+ i 1) p)
                      )))))

(defun cycles-dimension (cycles)
  "number of things being permuted"
  (loop for cycle in cycles maximize
        (cond ((null cycle) 0)
              ((listp (cdr cycle)) (apply #'max cycle))
              (t (max (car cycle) (cdr cycle))))))


(defun eval-list (x cycle)
  (let ((i (position x cycle))
        (n (length cycle)))

    (if (null i)
        x
        (elt cycle (mod (+ 1 i) n)))) )

(defun eval-pair (x pair)
  (cond ((= (car pair) x) (cdr pair))
        ((= (cdr pair) x) (car pair))
        (t x)))

(defun eval-cycle (x reversed-cycles)
    (reduce (lambda (x cycle)
              (cond ((null cycle) x)
                    ((listp (cdr cycle))
                     (eval-list x cycle))
                    (t (eval-pair x cycle))))
            reversed-cycles
            :initial-value x))

 
(defun cycles-to-map (n cycles)
  "convert cycles back to permutations"
  (let ((reversed (reverse cycles)))
    (loop for i from 1 to n collect 
              (eval-cycle i reversed))))


(defun to-pairs-helper (cycle) 
  (let ((base (car cycle))
        (tail (cdr cycle)))
    (nreverse (loop for x in tail collect
                    (cons (min base x)
                          (max base x))))))


(defun cycles-to-pairs (cycles) 
    "every permutation can be written as transpositions"
    (apply #'append (mapcar #'to-pairs-helper cycles)))

(defun nrotate-cycle (cycle)
  (let ((N (length cycle))
        (next 0)
        (temp 0))
    (setf next (aref cycle (- N 1)))
    (dotimes (i N)
      (setf temp (aref cycle i))
      (setf (aref cycle i) next)
      (setf next temp)
     ))
  cycle)
 
(defun cycles-evenp (cycles)
  (evenp (reduce #'+ (mapcar (lambda (cycle) (- (length cycle) 1)) cycles))))

(defun all-pairs (n)
  (let ((result '()))
    (do ((i 1 (+ i 1)))
        ((>= i n) result)
        (do ((j (+ i 1) (+ j 1)))
            ((> j n) result)
            (push (cons i j) result)
            ))
    
    (assert ( = (length result) (n-choose-2 n)))
    result))

(defun do-counting (base length fn)
  (let ((digits (make-array length :initial-element 0)))
    (prog ((total 0) (carry 0))
      loop
      (funcall fn digits)
      (incf (aref digits 0))
      (setf carry 0)
      (dotimes (i length)
        (setf total (+ carry  (aref digits i)))
        (setf (aref digits i) (mod total base))
        (setf carry (floor total base)))

      (if (every #'zerop digits)
          nil
          (go loop)) 
      )))


(defmacro invert-f (f)
  `(lambda (&rest args) (not (apply ,f args))))

(defun permute-rank (p R N)
  "permutation, upper bound, order of permutation group"
  (let ((pairs (cons '() (all-pairs N)))
        (bands-test (make-array N :initial-element nil)))
    (do-counting (length pairs)
                (- R 1)
                (lambda (index)
                  (let ((new-R (count-if (invert-f #'zerop) index)))
                    (if (< new-R R)
                        (progn 
                          (dotimes (i (length index))
                            (setf (aref bands-test i)
                                  (elt pairs (aref index i))))

                          (let ((bands-permute (cycles-to-map N bands-test)))
                            (if (equalp p bands-permute)
                                (progn
                                  (pprint bands-test)
                                  (pprint bands-permute)
                                  (setf R new-R)
                                  )))))))) R))

(defun map-pair-reps (map rank N)
  "permutation, upper bound, order of permutation group"
  (let ((pairs (all-pairs N))
        (bands-test (make-array rank :initial-element nil))
        (result nil))

    (do-counting (length pairs)
                rank
                (lambda (index)
                  (loop for i from 0 below (length index) do
                        (setf (aref bands-test i)
                              (elt pairs (aref index i))))

                  (let ((other-map (cycles-to-map N bands-test)))
                    (when (map-equiv-p map other-map)
                        (push (coerce bands-test 'list) result)
                        ))))
    result))



;(pprint (permute-rank (cycles-to-permute 7 (braid-to-pairs '(3 5 -2 -1 -2 -5 2 1 2 -3 -3 -1 -1 2 1 1 2 -1 3 2 -3 2 1 3 3 3 -4 -3 -5 -3 4 -6 -4))) 
;                       6 7))

;(pprint (permute-rank (cycles-to-permute 4 (braid-to-pairs '(-2 -1 -3 1 2 1 2 -1)))
;                      4 4))




; https://en.wikiversity.org/wiki/Binomial_coefficients
(defun arrangements (n k)
  (if (= n 0)
      (list nil)
       (nconc
         (when (< k n)
           (mapcar (lambda (tail)
                     (cons 0 tail))
                   (arrangements (- n 1) k)))
         (when (> k 0)
           (mapcar (lambda (tail)
                     (cons 1 tail))
                   (arrangements (- n 1) (- k 1))
                   )))))


(defun rank-cycles (cycles)
  (loop for cycle in cycles sum
        (- (length cycle) 1))) 


(defun test () 
  (let* ((N 4)
         (permutations (make-all-maps N))
         (cycles (mapcar #'map-to-cycles permutations))
         (pairs (mapcar #'cycles-to-pairs cycles)))


    (assert (equalp (factorial N) (length permutations)))

    (assert (equalp (mapcar 
                      (lambda (x) (cycles-to-map N x))  
                      cycles) permutations ))


    (assert (equalp (mapcar 
          (lambda (x) (cycles-to-map N x))  
          pairs) permutations))
    
    ))

(test)



