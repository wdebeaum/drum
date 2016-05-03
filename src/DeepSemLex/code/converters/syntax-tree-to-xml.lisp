(in-package :dsl)
;; see also src/WebParser/tree-to-xml.lisp, though the input trees there are slightly different

(defun syntax-tree-to-xml-stream (st s)
  (destructuring-bind (syn-cat features &rest children) st
    (let* ((tagname (symbol-name syn-cat))
	   (apos-pos (position #\^ tagname))
	   (wrap-p (> (length children) (if features 0 1))))
      (when apos-pos
	(setf tagname (concatenate 'string
	    (subseq tagname 0 apos-pos)
	    (if (= 0 apos-pos) "" "-")
	    "apostrophe"
	    (if (= (- (length tagname) 1) apos-pos) "" "-")
	    (subseq tagname (1+ apos-pos))
	    )))
      (format s "<~a>" tagname)
      (indented
	(when features
	  (when wrap-p (format s "~&~vt" *indent*))
	  (format s "<syn-feats>~(~{<feat name=\"~s\">~s</feat>~}~)</syn-feats>" (apply #'append features)))
	(dolist (child children)
	  (when wrap-p (format s "~&~vt" *indent*))
	  (syntax-tree-to-xml-stream child s))
	)
      (when wrap-p (format s "~&~vt" *indent*))
      (format s "</~a>" tagname)
      )))

