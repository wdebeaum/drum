(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
                       :name "trips")))
(load #!TRIPS"src;DeepSemLex;code;converters;dsl-to-xml")

(in-package :dsl)

(defun cl-user::run (output-dir)
  (require-resource-version :ont)
  (require-resource-version :ont-t)
  (require-resource-version :ont-w)
  (let ((*package* (find-package :dsl))
	(ont-pkg (find-package :ont)))
    (maphash
      (lambda (concept-name concept)
        (let ((processing-stage "(huh) "))
	  (handler-case
	    (when (and (eq ont-pkg (symbol-package concept-name)) ;; ONT::*
		       ;; was actually defined in TRIPS
		       (provenance concept)
		       (eq 'TRIPS (name (car (provenance concept)))))
	      (with-open-file
		(
		  xml
		  (format nil "~a/ONT::~a.xml" output-dir
			  (string-downcase (symbol-name concept-name)))
		  :direction :output
		  :if-exists :supersede
		)
		(format xml "<?xml version=\"1.0\"?>~&<?xml-stylesheet type=\"text/xsl\" href=\"ont-type.xsl\"?>~&<dsl>")
		;; set and write the provenance so we don't keep repeating it
		(setf processing-stage "provenance of ")
		(let ((*current-provenance* (car (provenance concept))))
		  (dsl-to-xml-stream (listify *current-provenance*) xml)
		  ;; write the main concept,
		  (setf processing-stage "")
		  (dsl-to-xml-stream (listify concept) xml)
		  ;; ... its ancestors,
		  (setf processing-stage "ancestors of ")
		  (dolist (anc (eval-path-expression
				   '(+ >inherit) (list concept)))
		    (setf processing-stage
			(if (and (typep anc 'concept)
				 (not (anonymous-concept-p anc)))
			  (format nil "ancestor named ~s, of " (name anc))
			  "ancestor of "))
		    (dsl-to-xml-stream (listify anc) xml))
		  ;; ... and its children (including senses)
		  (setf processing-stage "children of ")
		  (dolist (child (eval-path-expression '<inherit (list concept)))
		    ;; TODO include non-parametric template definitions?
		    (setf processing-stage
			  (cond
			    ((and (typep child 'sense)
			          (slot-boundp child 'morph)
			          (maps (morph child)))
			      (format nil "sense of ~s, under "
				  (listify (base-word (morph child)))))
			    ((anonymous-concept-p child)
			      (format nil "anonymous child ~s of "
				  (type-of child)))
			    (t
			      (format nil "child named ~s, of " (name child)))
			    ))
		    (dsl-to-xml-stream (listify child) xml))
		  (format xml "~&</dsl>~%")
		  )
		))
	      (error (e)
		(format *error-output* "Error while processing ~aconcept named ~s:~%~a~%" processing-stage concept-name e))
	      )))
      (concepts *db*)
      )
    ;; TODO W::*.xml
    )
  )
