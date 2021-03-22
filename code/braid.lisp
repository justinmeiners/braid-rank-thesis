(defpackage :braid
  (:use :cl)
  (:export
    make-random
    strands
    algebraic-sum
    to-pairs
    to-cycles
    to-map
    identityp
    maybe-identity-p
    equivp
    all-equiv-p
    free-automorphism
    main-generator
    reduced-p
    dehn-<
    handle-reduce
    to-kjb-string
    to-tex-string
    ))

(in-package :braid)

(defun strands (braid)
  (+ 1 (reduce (lambda (best x) (max best (abs x))) 
               braid
               :initial-value 0 )))

(defun algebraic-sum (braid)
  "abelinization"
  (loop for x in braid sum (signum x)))

(defun generator-to-pair (gen) (cons gen (+ gen 1)))

(defun to-pairs (braid)
  "permutation pairs (just pairs, not reduced)"
  (remove-if
            (lambda (pair) (= (car pair) 0))
            (mapcar (lambda (power)

                     ; TODO, can we not do the power thing?
                      (generator-to-pair (* (car power) (mod (cdr power) 2)))) 
                    (group:to-powers braid))))

(defun to-map (braid)
  (perm:cycles-to-map (strands braid)
                        (to-pairs braid)))

(defun to-cycles (braid)
  (perm:map-to-cycles (to-map braid))) 


(defun make-random (N L r-state)
  (loop for i from 0 below L collect
        (* (if (= 1 (random 2 r-state))
               1
               -1)

               (+ 1 (random (- N 1) r-state)))))
 

; WORD PROBLEM WITH FREE GROUP
; https://chaoxuprime.com/posts/2011-06-23-word-problem-for-braid-group-using-a-representation.html


(declaim (inline sigma inv-sigma))



(declaim (optimize speed))



(defun sigma (i j)
  "Automorphism of the free group.
   i is the braid generator index.
   j is the free group generator"

  (declare (fixnum i j)) 
  (cond ((= (abs j) i)
         (list (if (> j 0)
             (+ i 1)
             (- (+ i 1)))))
        ((= (abs j) (+ i 1)) 
         (list (+ i 1)
               (if (> j 0)
                   i
                   (- i))
               (- (+ i 1))))
        (t (list j))))

(defun inv-sigma (i j)
  "inverse of free-automorphism"
  (declare (fixnum i j))

  (cond ((= (abs j) (+ i 1))
         (list (if (> j 0)
                   i
                   (- i))))
        ((= (abs j) i) 
         (list 
           (- i)
           (if (> j 0)
               (+ i 1)
               (- (+ i 1)))
           i))
        (t (list j))))


(defun free-automorphism (i j)
  (declare (fixnum i j))
  (if (>= i 0)
      (sigma i j)
      (inv-sigma (- i) j)))

(defun apply-automorphism (reverse-braid i)
  (reduce (lambda (free-word braid-elem)
            (group:free-reduce
              (loop for x in free-word nconc
                    (free-automorphism braid-elem x))))
          reverse-braid
          :initial-value (list i)
          ))


; DEHONORY HANDLE REDUCTIOIN

(defun main-generator (braid)
  (if (null braid)
      0
      (reduce (lambda (a b) 
            (if (< (abs a) (abs b))
                a
                b))
          braid)))

(defun reduced-p (braid)
  "main generator occurs only positively or negatively"
    (if (null braid)
        nil
        (let ((g (abs (main-generator braid)))
          (N 0)
          (k 0))
        (loop for x in braid do
            (when (= (abs x) g)
                  (incf N)
                  (setf k (+ k (signum x)))
             ))
        (= N (abs k)))))
 

(defun in-range (x min max)
  (and (>= x min) (<= x max)))

(defun match-handle (braid start end)
  "tries to find an end to the handle beginning with start.
   the cell returned is the end of the handle (inclusive)"
    (cond ((eq braid end) nil)
          ((= (car braid) (- (car start))) braid)
          ((in-range (abs (car braid))
                     (1- (abs (car start)))
                     (abs (car start))
                     ) nil)
          (t (match-handle (cdr braid) start end))))

(defun find-left-handle (braid)
  (prog ((handle nil)
         (end nil)
         (temp nil))
        loop
        (when (not (eq braid end))
            (setf temp (match-handle (cdr braid)  braid end))
            (when temp
                (setf handle braid)
                (setf end (cdr temp)))
            (setf braid (cdr braid))
            (go loop))
        (return (values handle end))
        ))

(defun handle-homomorphism (sgn j i) 
    (assert (>= j 0))
    (cond ((= j (abs i)) nil)
          ((= (+ j 1) (abs i)) (list (* (- sgn) (+ j 1))
                                     (* (signum i) j)
                                     (* sgn (+ j 1))
                                     ))
          (t (list i))))

(defun upto (list cons)
  (let ((result nil))
    (loop for cell on list do
          (if (eq cell cons)
              (return nil)
              (push  (car cell) result))
          )
    (nreverse result)))

(defun remove-handle (handle)
  (let ((j (abs (car handle)))
        (sgn (signum (car handle))))
    (loop for x in (cdr handle) nconc
          (handle-homomorphism sgn j x))))

(defun handle-reduce (braid)
  (multiple-value-bind (start end) (find-left-handle braid)
    (if (null start)
        braid
        (handle-reduce (group:free-reduce
          (nconc (upto braid start)
                 (remove-handle (upto start end))
                 end)))
        )))

(defun dehn-< (a b)
  (< 0 (signum (main-generator (handle-reduce (append (group:invert a) b))))))

(defun maybe-identity-p (braid)
  "obstructions to being identity"
  (and (= (algebraic-sum braid) 0)
       (not (reduced-p braid))))

(defun identityp (braid &key (early-checks t))
  "word problem for groups"
  (when (or (not early-checks) (maybe-identity-p braid))
    (let ((N (strands braid)) 
          (reversed (reverse braid)))
      (not (loop for i from 1 to N do
                 (if (not (equalp (list i) (apply-automorphism reversed i)))
                     (return t)))))))

(defun equivp (a b)
  "word problem convenience, with optimizations/preparations"
  (identityp (delete 0 (append (group:invert b) a))) )

(defun all-equiv-p (braid)
  (if (or (null braid) (null (cdr braid)))
      t
      (and (equivp (car braid) (cadr braid))
           (all-equiv-p (cdr braid))))) 



(defun test ()
  (assert (equivp '(1 2 1) '(2 1 2)))
  (assert (not (equivp '(1 2 3 4 5) '(5 4 3 2 1))))
  (assert (equivp '(1 3 1 4) '(1 1 3 4)))
  (let ((x '(-1 -3 1 2 1 3 4 -5 3 -5 -6 5 -3 5 -4 -1 -2 -3 -4 3 -1 -1 -5 1 3 1 -2 1 2)))
    (assert (equivp x
                    (handle-reduce x)
                    )))
  )

(test)

(defun to-kjb-string (braid)
  "knotjob. strands a through i. uppercase for inverses."
  (map 'string (lambda (x) 
                 (code-char
                   (+ (- (abs x) 1)
                      (if (>= x 0)
                          (char-code #\a)
                          (char-code #\A))))) braid))

(defun to-tex-string (braid)
  "inverses are bars"
  (with-output-to-string (s)
                      (dolist (item (mapcar (lambda (x) (if (>= x 0)
                                                             (write-to-string x)
                                                             (format nil "\\inv{~a}" (- x))
                                                             )) braid))

                        (format s "~a" item)
                        )))

