(defpackage :rank
    (:use :cl)
    (:export
        find-band-presentation
        band-pair
        band-sign
        upper-bound
        upper-bound-generate
        lower-bound
        lower-bound-from-genus
        positive-band-count))

(in-package :rank)

; RANK

; suppose we write b = axa^-1
; where x,a, are words and length(a) is minimized.


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
  (let* ((n (length braid))
         (best n)
         (start 0)
         (end n))
    (loop for i from 0 below n do
          (loop for j from i to n do
                (when (and (< (- j i) best)
                       (braid:identityp (nconc (subseq braid 0 i)
                                        (subseq braid j))))
                    (setf best (- j i))
                    (setf start i)
                    (setf end j))
                ))
    (values start end)))

(defun upper-bound-part (braid start end memo)
  (let ((part (subseq braid start end))
        (N (- end start)))

    (multiple-value-bind (i j)
      (find-conjugate part)
      (min 
        (if (commutable-p part) (group:total-generators part) N)
        (if (< (- j i) N)
            (funcall memo (+ start i) (+ start j))
            N)
        (loop for k from (+ start 1) below end minimize 
              (+ (funcall memo start k)
                 (funcall memo k end)))
        ))))

(defun upper-bound (braid)
  "rank upper bound: modified version of free group rank."
  (let* ((N (length braid))
         (matrix (make-array (list N N)
                             :element-type 'fixnum
                             :initial-element 0))
         (memo-lookup (lambda (start end)
                        (if (= start end) 
                            0
                            (aref matrix start (- end 1)))))) 

    (loop for i from 0 below N do
          (setf (aref matrix i i) 1))

    (loop for length from 2 to N do
          (do ((i 0 (+ i 1)))
              ((> (+ i length) N) nil)

              (let ((j (+ i (- length 1))))
                (setf (aref matrix i j)
                      (upper-bound-part braid i (+ j 1) memo-lookup)))))

    (values (aref matrix 0 (- N 1)) matrix)))

(defun upper-bound-part-2 (braid start end memo)
  (let* ((part (subseq braid start end))
         (N (- end start))
         (best part)
         (bestScore N))

    (when (commutable-p part)
      (setf best (list 'R part))
      (setf bestScore (group:total-generators part)))

    (multiple-value-bind (i j)
        (find-conjugate part)
      (when (< (- j i) N)
        (let ((temp (funcall memo (+ start i) (+ start j))))
          (when (< (car temp) bestScore)
            (setf best (list 'C  (subseq braid start (+ start i)) (cdr temp)))
            (setf bestScore (car temp))
            ))))

      (loop for k from (+ start 1) below end do
            (let ((left (funcall memo start k))
                  (right (funcall memo k end)))

              (when (< (+ (car left) (car right)) bestScore)
                (setf best (list 'S (cdr left) (cdr right)))
                (setf bestScore (+ (car left) (car right)))
                )
              ))
    (cons bestScore best)))

(defun upper-bound-generate (braid)
  "rank upper bound: modified version of free group rank."


  (let* ((N (length braid))
         (matrix (make-array (list N N)
                             :initial-element (cons 0 '())))
         (memo-lookup (lambda (start end)
                        (if (= start end) 
                            (cons 0 nil)
                            (aref matrix start (- end 1)))))) 

    (loop for i from 0 below N do
          (setf (aref matrix i i) (cons 1 (elt braid i))))

    (loop for length from 2 to N do
          (do ((i 0 (+ i 1)))
              ((> (+ i length) N) nil)

              (let ((j (+ i (- length 1))))
                (setf (aref matrix i j)
                      (upper-bound-part-2 braid i (+ j 1) memo-lookup)))))

    (values (aref matrix 0 (- N 1)) matrix)))



(defun lower-bound (braid)
  (let ((sum (braid:algebraic-sum braid))
        (perm (perm:rank-cycles (braid:to-cycles braid))))

    (values (max (abs sum) (abs perm)) sum perm)))


(defun lower-bound-from-genus (genus strands)
  "must be a knot, and not a link"
  (+ (- (* 2 genus) 1) strands))

(defun test-rank ()
  (assert (<= (upper-bound '(2 1 2 -1 -2  3 2 -3)) 2))
  (assert (= (upper-bound '(3 2 1 2 -1 -2 -3)) 1))
  (assert (= (upper-bound '(4 6 -2 6 -4 6 2)) 3))
  (assert (= (upper-bound '(1 2 1 -2 -1 -2)) 0))
  )

(test-rank)

(defun positive-band-count (braid rank)
    "number of positive bands in a decomposition"
    (floor (+ rank (braid:algebraic-sum braid)) 2)) 


(defun band-sign (band)
  (if (= (length band) 1)
      (signum (elt band 0))
      (signum (elt band (floor (length band) 2)))))
 
(defun band-pair (band)
  (let ((list (car (braid:to-cycles band))))
    (cons (* (band-sign band) (first list)) (second list))))


(defun do-counting (counts fn)
 (let ((index-vec (make-array (length counts) :initial-element 0
                                              :element-type 'fixnum)))
  (labels ((helper (index)
            (if (>= index (length counts))
                (funcall fn index-vec)
            (loop for i from 0 below (aref counts index) do
             (setf (aref index-vec index) i)
             (helper (+ index 1))))))
   (helper 0))
  index-vec))


 (defun try-forms (braid template band-table log)
  (let* ((buckets (map 'vector (lambda (x) (gethash x band-table)) template))
         (counts (map 'vector #'length buckets))
        (result nil))

   (when log
        (format log "template: ~a has: ~a ~%" template (reduce #'* counts)))

   (do-counting counts
    (lambda (index)
     (let ((presentation (loop for j from 0 below (length index) collect
                          (aref (aref buckets j) (aref index j))) ))
      (when (braid:equivp braid (apply #'append presentation))
       (push presentation result)
      ))))
    result))

(defun words (W N)
  (if (= w 0)
      '(nil)
      (let ((smaller (words (- W 1) N) ))
        (loop for i from (- N) to N append
              (mapcar (lambda (tail)
                        (cons i tail)
                        ) smaller)
              ))))


(defun positive-bands (W N)
  (let ((words (words W N)))
    (loop for i from 1 to N append
          (loop for x in words collect
                (concatenate 'list x (list i) (group:invert x)))
          )))

(defun permutation-templates (braid rank)
  (perm:map-pair-reps (braid:to-map braid) rank (braid:strands braid)))

(defun sum-templates (braid rank)
  (perm:arrangements rank (positive-band-count braid rank)))

(defun signed-templates (sums perms)
  (loop for s in sums nconc
        (loop for p in perms collect
              (mapcar (lambda (pair sign)
                        (cons (if (= sign 1)
                                  (car pair)
                                  (* -1 (car pair))
                                  )
                              (cdr pair))
                        )
                      p s)
               )))

(defmacro return-not-nil (result)
  (let ((v (gensym)))
    `(let ((,v ,result))
       (when (not (null ,v))
         (return ,v)
         ))))

(setf lparallel:*kernel* (lparallel:make-kernel 7))

(defun find-band-presentation (braid rank conjugate-width &key (log *standard-output*))
  (let ((bank (make-hash-table :test #'equalp))
        (to-try (mapcar #'group:free-reduce (positive-bands conjugate-width (braid:strands braid))))
        (templates (signed-templates (sum-templates braid rank)
                                     (permutation-templates braid rank))))

    (loop for word in to-try do
          (push word (gethash (band-pair word) bank))   
          (push (group:invert word) (gethash (band-pair (group:invert word)) bank)))

    (maphash (lambda (k v)
               (setf (gethash k bank)
                     (coerce (group:merge-adjacent
                       #'braid:equivp
                       (lambda (a b) a)
                       (sort v #'braid:dehn-<))
                     'vector)))
             bank)


    (when log
        (maphash (lambda (k v) (format log "~a has ~a ~%" k (length v))) bank))

    (when log
        (format log "braid: ~a writhe: ~a ~%" braid (braid:algebraic-sum braid))
        (format log "~a bands to try ~%" (length to-try))
        (format log "~a templates to try. ~%" (length templates)))


    (apply #'append (remove nil (lparallel:pmap 'list
                                 (lambda (template)
                                   (try-forms braid template bank log))
                                  templates)))))



(defun test-search ()
    (assert (= (band-sign '(-1 1 1)) 1))
    (assert (= (band-sign '(2 -1 -2)) -1))
    (assert (= (band-sign '(-3 -2 -1 2 3)) -1)))

(test-search)

