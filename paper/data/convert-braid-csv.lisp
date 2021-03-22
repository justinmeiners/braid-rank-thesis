#!/usr/bin/sbcl --script

(load "~/.sbclrc")
(asdf:load-system "cl-csv")

(defun split-delimiterp (c) (or (char= c #\Space) (char= c #\:) (char= c #\;)))

(defun split-string (string &key (delimiterp #'split-delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
        :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
        :while end))



(defun parse-row (row)
  (let ((name (nth 0 row))
            (braid (mapcar (lambda (x) (parse-integer x :junk-allowed t))
                           (split-string (subseq (nth 1 row) 1 (- (length (nth 1 row)) 1)))))
            (genus (or (parse-integer (nth 2 row) :junk-allowed t) 0))
            (quasi-positive (if (string= (nth 3 row) "Y") t nil)))

    (if (position nil braid)
        nil
        (list nil 
              braid
              name
              quasi-positive
              genus
              ))))

(defparameter *input-path* (pathname (first (uiop:command-line-arguments))))
(defparameter *output-path* (pathname (second (uiop:command-line-arguments))))

(defparameter *result* (remove nil (cl-csv:read-csv *input-path*
                                                    :map-fn #'parse-row)))

(with-open-file (stream *output-path* :direction :output :if-exists :supersede)
  (pprint (list
            'defparameter 
            (intern (string-upcase (concatenate 'string "*" (pathname-name *input-path*) "*")))

            (list 'quote *result*))
          stream))

