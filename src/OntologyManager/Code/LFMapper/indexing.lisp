;; Nate Chambers
;;
;; LISP functions for indexing and retrieving transform information
;;


(defun get-lf-typetransforms ()
  "@desc Returns all the LF types used in the transform rules"
  (let ((lfs (mapcan #'get-lf-types-in-rule (append (car *lftransforms*) (second *lftransforms*)))))
    ;; remove duplicates
    (remove-duplicates lfs :test #'eq)))

(defun get-lf-types-in-rule (rule)
  "@param rule An lftransform structure"
  (cons (typetransform-lftype (lftransform-typetransform rule))
	(mapcan #'get-lf-types-in-arg
		(lftransform-argtransforms rule))))

(defun get-lf-types-in-arg (arg)
  "@param arg An argtransform structure"
  (mapcan #'(lambda (x) (if (and (listp x) (> (length x) 2))
			    (list (extract-lftype x))))
	  (append (argtransform-content arg)
		  (argtransform-context arg))))


(defun get-kr-typetransforms ()
  "@returns A list of KR types that the transforms create"
  (let (all kr)
    ;; scale all the transforms
    (dolist (transform (append (car *lftransforms*) (second *lftransforms*)))
      (setq kr (typetransform-kr (lftransform-typetransform transform)))
      (if (and kr (not (is-transform-var kr)))
	  (setq all (cons kr all))))

    ;; append all the atom-mappings
    (remove-duplicates
     (append all (mapcan #'(lambda (x) (if (not (stringp (second x))) (list (second x))))
			 *lf-atom-map*)))))