;; Handles feature type definitions file
;; each feature type has a permissible definition along with allowable features
;; and their defaults


(in-package "ONTOLOGYMANAGER")
;; Takes an (unparsed) feature type description
;; and adds it to the table
(defun add-feature-type (type lfontology &key features defaults)
  "Adds the type description to the table; Parses feature defaults and makes them into feature list structure"
  ;; Starting point for recursion
  (handler-case (add-feature-type-entry type features defaults lfontology)
    (invalid-feature-type-entry (err) 
      (lfontology-warn "invalid feature type specification for ~S:~%  ~A" type err)
      nil)
    (invalid-sem-spec (err) 
      (lfontology-warn "invalid SEM specification for ~S:~%  ~A" type err)
      nil)))


;; helpr function that does all the work for add-feature-type
(defun add-feature-type-entry (name features defaults lfontology)
  (when (null name)
    (error (make-system-condition 'invalid-feature-type-entry
			     :format-control "Invalid description ~S: feature type name expected" 
			     :format-arguments (list name))))
  (when (notevery (lambda (x) (member (car x) features)) defaults)
    (error (make-system-condition 'invalid-feature-type-entry
			     :format-control "Invalid description ~S: defaults contains features ~S not declared in features " 
			     :format-arguments (list name
						     (remove-if (lambda (x) (member (car x) features)) defaults)
						     ))
	   ))
  ;; check feature names and values for typos  
  (checksemfeatures defaults lfontology)    
  (add-feature-type-description
   (make-feature-type-description :name name
				  :features features
				  :defaults (make-feature-list :type name :features defaults))
   (ling-ontology-feature-type-table lfontology)
   )
  (ling-ontology-feature-type-table lfontology)
  )



;; takes a feature type description and adds it to the table 
(defun add-feature-type-description (ft table)
  (sethash (feature-type-description-name ft) ft table)  )

;; Takes a typed feature list and completes it according to the defaults in the table
(defun complete-features-with-defaults (tfl table)
  (let* ((fl (or tfl (make-untyped-sem tfl)))
	 (type (feature-list-type fl))
	 )
    (cond
     ((null type) fl)
     ((is-variable-name type) fl)
     ((listp type) fl)
     ((null (gethash type table))
      (error (make-system-condition 'invalid-sem-spec
			       :format-control "Invalid semantic specification ~S: unknown feature type name ~S"
			       :format-arguments (list fl type))))
     (t ;; we know the type
      (merge-feature-list-with-defaults fl (feature-type-description-defaults (gethash type table)))
      ))
    ))
  


;; Takes a typed feature list and completes it according to the defaults in the table
(defun complete-features-with-rootvalues (tfl table)
  (let* ((fl (or tfl (make-untyped-sem tfl)))
	 (type (feature-list-type fl))
	 (typefeat (and type (get-features-for-type type table)))
	 )
    (cond
     ((null type) fl)
     ((null typefeat) ;; could not find features - error
      (error (make-system-condition 'invalid-sem-spec
			       :format-control "Invalid semantic specification ~S: unknown feature type name ~S"
			       :format-arguments (list fl type))))
     (t ;; now go for the values
      (merge-feature-list-with-defaults
       fl
       (make-feature-list :type type
			  :features (mapcar (lambda (f) (list f (feature-root-name f)))
					    typefeat))
       ))
     )
    ))

;; givena type, get all associated feature names
(defun get-features-for-type (tp table)
  (cond
   ((is-variable-name tp)
    (remove-duplicates (mmapcan (lambda (x) (get-features-for-type x table)) (get-all-keys table)))
    )
   ((symbolp tp)
    (and (gethash tp table)
	 (feature-type-description-features (gethash tp table)))
    )
   ((listp tp)
    (if (eql (length tp) 2) ;; just a type name with no restr
	(remove-duplicates (mmapcan (lambda (x) (get-features-for-type x table)) (get-all-keys table)))
      (remove-duplicates (mmapcan (lambda (x) (get-features-for-type x table)) (cddr tp)))
      )
    )
   (t nil)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for hierarchical matching with subtype, needed for James sem unification
;;

(defun make-feature-type-table-trees (table)
  (let ((types (get-all-keys table)))
    (mapcar #'list types)
    ))
  
