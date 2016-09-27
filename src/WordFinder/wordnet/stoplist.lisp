(in-package :wordfinder)

(defun slurp-file (name)
  "Read the entire contents of the named file into a string and return it."
  (with-open-file (f name :direction :input)
    (let* ((len (file-length f))
           (str (make-string len)) ; FIXME bytes vs chars?
	   (len2 (read-sequence str f)))
      (unless (= len len2)
        (error "Slurped length ~s not equal to expected length ~s~%" len2 len))
      str)))

(defun split-stoplist-string (str)
  "Turn a string of one sense key per line, with comments and extra whitespace,
   into a list of trimmed, downcased single sense key strings."
  (remove ""
	  (mapcar (lambda (line)
		    (string-downcase (string-trim " "
			(let ((semicolon-pos (position #\; line)))
			  (if semicolon-pos
			    (subseq line 0 semicolon-pos)
			    line)))))
		  (util:split-string str :on '(#\Newline)))
	  :test #'string=))

(defvar *stoplist*
  (nset-difference
    (split-stoplist-string
      (slurp-file #!TRIPS"src;WordFinder;wordnet;stoplist.txt"))
    (split-stoplist-string
      (slurp-file #!TRIPS"src;WordFinder;wordnet;golist.txt"))
    )
  "List of WordNet sense keys for senses we should avoid using."
  )

(defvar *stophash*
  (loop with h = (make-hash-table :test #'equal)
	for s in *stoplist*
	do (setf (gethash s h) t)
	finally (return h)))

(defun stoplist-p (sk)
  "Is the sense key string sk in the stoplist?"
  (gethash
      ;; add :: suffix if necessary
      (case (count #\: sk)
        (2 (concatenate 'string sk "::"))
	(4 sk)
	(otherwise (error "Bogus sense key: ~a" sk))
	)
      *stophash*))

