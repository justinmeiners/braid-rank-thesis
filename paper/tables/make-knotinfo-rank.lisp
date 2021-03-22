#!/usr/bin/sbcl --script

(load "~/.sbclrc")
(asdf:load-system "braid-algebra")
(load "data/knotinfo.lisp")


(defparameter *quasipositive*  (remove-if-not (lambda (entry)
                                                (= (abs (braid:algebraic-sum (second entry)))
                                                   (rank:upper-bound (second entry)))
                                                ) *knotinfo*))

(defparameter *specials*
  '("8_20" "8_21"
    "9_45" "10_126"
    "10_127" "10_131"
    "10_133" "10_140"
    "10_143" "10_145"
    "10_148" "10_155"
    "10_157" "10_159"
    "10_165" "12n_113"
    "12n_114" "12n_190"
    "12n_233" "12n_234"
    "12n_344" "12n_345"
    "12n_466" "12n_467"
    "12n_570" "12n_604"
    "12n_666" "12n_674"
    "12n_683" "12n_684"
    "12n_707" "12n_708"
    "12n_721" "12n_722"
    "12n_747" "12n_748"
    "12n_767" "12n_820"
    "12n_822" "12n_829"
    "12n_831" "12n_882"
    "12n_887"
    ))

(defparameter *output-path* (first (uiop:command-line-arguments)))

(with-open-file (stream *output-path* :direction :output :if-exists :supersede)
  (loop for entry in *knotinfo* do
        (let* ((rank (rank:upper-bound (second entry)))
               (strands (braid:strands (second entry)))
               (sum (braid:algebraic-sum (second entry)))
               (genus-predicted (rank:lower-bound-from-genus (fifth entry) strands))
               (attributes nil)
               )

          (when (= rank genus-predicted)
              (push 'R attributes))

          (when (= rank sum)
            (push 'P attributes))

          (when (= rank (- sum))
            (push 'N attributes))

          (when (not (null attributes))

              (when (find (third entry) *specials* :test #'string=)
                (push '* attributes))

            (format stream "~a & $~a$ & ~a & ~a & ~a & ~a & ~a & ~{~a~^ ~} \\\\~%"
                    (substitute #\space #\_ (third entry)) 
                    (braid:to-tex-string (second entry))
                    strands
                    (fifth entry)
                    genus-predicted
                    sum
                    rank
                    (reverse attributes)
                    )))))

(format t "~a/~a ~%" (length *quasipositive*)
                    (length (remove-if-not (lambda (entry) (fourth entry)) *knotinfo*)))


