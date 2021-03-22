; POLYNOMIALS
; these allow negative exponents, so technically
; they are formal Laurent Series

; ORDER: exponent of smallest non-zero term
; DEGREE: exponent of largest non-zero term
; https://math.stackexchange.com/questions/231357/the-degree-of-a-seriesnomial-which-also-has-negative-exponents

(defpackage :laurent-series
    (:use :cl)
    (:nicknames :ls)
    (:export series
             make-series
             make-series-simple
             make-series-identity
             series-degree
             series-condense
             series-get
             series-set
             series+
             series-
             series*
             series-expt
             series-substitute-var
             series-to-string
             series-equalp
           ))

(in-package :ls)


(defstruct series
    (order 0 :type fixnum)
    (coef (make-array 1
            :initial-element 0
            :element-type 'number)
        :type (simple-array number)))

(defun make-series-simple (degree order)
    (make-series
        :coef (make-array (+ 1 (- degree order))
                :initial-element 0
                :element-type 'number)
        :order order))

(defun make-series-identity ()
    (make-series :order 0 :coef #(1)))

(defun series-degree (p)
  (+ (- (length (series-coef p)) 1)
     (series-order p) ))

(defun series-condense (p)
    (let ((left (position-if-not #'zerop (series-coef p)))
           (right (position-if-not #'zerop (series-coef p) :from-end t ))
           )
        (make-series
            :order (+ (series-order p) left)
            :coef (subseq (series-coef p)
                          left
                          (+ right 1)))))

(defun series-get (p d)
  (let ((index (- d (series-order p))))

    (if (or (< index 0)
            (>= index (length (series-coef p))))
      0
      (aref (series-coef p) index))))


(defun series-set (p d x)
  (let ((index (- d (series-order p))))

    (if (or (< index 0)
            (>= index (length (series-coef p))))
      (error "series set out of bounds")
      (setf (aref (series-coef p) index) x))))



(defun series-binary-op (x y op)
  (let* ((d  (max (series-degree x)
                  (series-degree y)))
         (o (min (series-order x)
                 (series-order y)))
         (z (make-series-simple d o)))

    (loop for i from o to d do
          (series-set z i
                (funcall op
                   (series-get x i)
                   (series-get y i))))
    z))

(defun series+ (x y)
    (series-binary-op x y #'+))

(defun series- (x y)
    (series-binary-op x y #'-))

(defun series* (x y)
  (let* ((d  (+ (series-degree x)
                (series-degree y)))
         (o (+ (series-order x)
               (series-order y)))
         (z (make-series-simple d o)))

    (loop for i from (series-order x) to (series-degree x) do
          (loop for j from (series-order y) to (series-degree y) do
                (series-set z (+ i j)
                          (+ (series-get z (+ i j))
                             (* (series-get x i)
                                (series-get y j))))))
    z))

(defun fast-exp (r x n op)
  (prog ()
        loop
        (if (= n 0)
            (return r)) 

        (if (oddp n)
            (setf r (funcall op r x))
            )

        (setf x (funcall op x x))
        (setf n (floor n 2))
        (go loop)
        ))

(defun series-expt (p N)
    (if (= N 0)
      (make-series-identity) 
      (fast-exp p p (- N 1) #'series*)))


(defun series-substitute-var (p coef power)
  "substitue variable: P(x) -> P(a y^(n))"
  (let ((out (make-series-simple
               (* (series-degree p) power) 
               (* (series-order p) power)  
               )))

    (loop for i from (series-order p) to (series-degree p) do
          (if (not (zerop  (series-get p i)))
              (series-set out (* i power)
                        (* (expt coef i) (series-get p i))
                        )))
    out))

; TODO: derivative and integral

(defun series-write (p stream &key (var #\x))
  (loop for i from (series-order p) to (series-degree p) do
        (let ((a (series-get p i)))
            (cond ((zerop a) nil)
                  ((= i 0) (format stream " + (~a)" a))
                  ((= a 1) (format stream " + ~a^(~a)"  var i))
                  ((= a -1) (format stream " + -~a^(~a)"  var i))
                  (t (format stream " + ~a~a^(~a)" a var i))))))
        
(defun series-to-string (p &key (var #\x))
    (let ((s (make-string-output-stream)))
        (series-write p s :var var)
        (get-output-stream-string s)))

(defun series-equalp (x y)
    (prog ((low (min (series-order x)
                (series-order y)))
           (high (max (series-degree x)
                   (series-degree y)
                   )))

          loop
          (if (not (= (series-get x low) (series-get y low)))
              (return nil)
              )

          (incf low)

          (if (<= low high)
              (go loop))
          
          (return t)))


; TODO: eval

(defun test ()
    (assert (equalp
                (series-coef (series-condense (make-series :order 1 :coef #(0 1 1 0))))
                #(1 1)))

    (assert (equalp
                (series-coef
                    (series+ (make-series :order 1 :coef #(1 1 1))
                              (make-series :order 0 :coef #(1 2 1 1))))
                #(1 3 2 2)))

    (assert (equalp
                (series-coef
                    (series* (make-series :order -1 :coef #(1 0))
                              (make-series :order 1 :coef #(0 1))))
                #(0 1 0)))

    (assert (equalp
                (series-coef
                    (series-expt (make-series :order 0 :coef #(1 1))
                               3
                              ))
                #(1 3 3 1)))


    (assert (equalp
                (series-coef
                    (series-expt (make-series :order 0 :coef #(0 1))
                               6
                              ))
                #(0 0 0 0 0 0 1))))




(test)



