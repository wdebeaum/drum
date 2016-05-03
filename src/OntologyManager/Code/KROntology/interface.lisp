(in-package "ONTOLOGYMANAGER")


(defun new-kr-ontology ()
  (make-kr-ontology
   :class-table (make-hash-table)
   :predicate-table (make-hash-table)
   :operator-table (make-hash-table)
   :map-table nil
   )	
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for modules using both LF and KR ontologies
;;


(defun initialize-lf-kr-interface (lfontology krontology krfeature)
  (declare (ignore lfontology))
  (setf (kr-ontology-map-table krontology) (make-hash-table))
  (setf (kr-ontology-slot-table krontology) (make-hash-table))
  (setf (kr-ontology-krfeature krontology) krfeature)
  (kr-ontology-map-table krontology)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Accessor functions currently used by other modules
;;

(defun add-kr-type (class krontology &key (isa nil) (slots nil) (lfontology nil) )
  (add-class class (kr-ontology-class-table krontology) :isa isa :slots slots)
  ;; if we have both LF and KR working, we need to add this feature to known features
  (when lfontology
    (when (kr-ontology-krfeature krontology)
      (add-value-to-feature (kr-ontology-krfeature krontology) class isa (ling-ontology-feature-table lfontology))
      )
    )
  )
    
(defun get-sem-for-class-type (class type lfontology krontology)
  (let* ((typesem (lf-description-sem (get-lf-definition type lfontology)))
	 (classsem (infer-implied-features (make-untyped-sem (list (list (kr-ontology-krfeature krontology) class))) 
					   lfontology))
	 )
    (merge-typed-feature-lists classsem typesem lfontology :two-way t)
    ))


(defun get-type-for-class (class lfontology krontology &optional sc)
  (let* ((trans (get-transform-for-class class lfontology krontology sc)))
    (cond
     ((null trans)
      (krontology-warn "Get-type-for-class: No known LF mappings for class ~S or its parents" class)
      (ling-ontology-default-lf-root lfontology)
      )
     (t
      (lf-kr-transform-lf (gethash trans (kr-ontology-map-table krontology)))
      ))
    ))

(defun get-transform-and-type-for-class (class lfontology krontology &optional sc)
  "Returns a transform that would be used to transform the class into LF"
  (let* ((classdescr (get-kr-type class krontology))	 
	 (cmaps (and classdescr (basic-class-description-lf-transforms classdescr)))
	 (start-class (and sc class))
	 (maps (remove-if-not (lambda (x) (null (get-lf-form
						 (lf-kr-transform-lf (gethash x (kr-ontology-map-table krontology)))
						 ':*)
						))
			      cmaps))
	 )
    (cond     
     ((null classdescr)
      (krontology-warn "unknown class ~S" class)
      )
     (maps ;; found some mappings
      (when (> (length maps) 1)      
	(krontology-warn "Get-type-for-class: Too many known LF mappings for class ~S (taken from the definition of ~S). The possible mappings are : ~S. I am using ~S in the lexicon" 
			 start-class class maps (first maps)))
      (let* ((transform (first maps))
	     (lf (lf-kr-transform-lf (gethash transform (kr-ontology-map-table krontology))))
	     )
	(list transform lf)
	))
     ;; NO MAPPINGS FOUND AFTER THIS POINT
     ((class-description-isa classdescr)
      ;; have a parent - search for parent mappings
      (get-transform-and-type-for-class (class-description-isa classdescr) lfontology krontology start-class)
      )
     (t ;; maps are null and no parent
      nil))
    ))


(defun get-kr-type (type krontology)
  (when type (gethash type (kr-ontology-class-table krontology))))


(defun get-kr-info (symbol kr-ontology)
  "finds out whatever information is defined for the symbol in the kr ontology"
  (or (gethash symbol (kr-ontology-class-table kr-ontology))
      (gethash symbol (kr-ontology-map-table kr-ontology))
      (gethash symbol (kr-ontology-predicate-table kr-ontology))))
      
               

(defun find-coerce-operators (krtype krontology)
  (let ((result nil))
    (maphash (lambda (key val)
	       ;;(print val)
	       (when (and (operator-description-coerce val) 
			  (compatible-values krtype (first (operator-description-arguments val))
					     :test (lambda (x y) (subclass x y krontology))))
		 (push key result)
		 ))
	     (kr-ontology-operator-table krontology)
	     )
    result
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for subtype matching
;;

(defun add-kr-ontology-to-hierarchy (krontology hierarchy)
  "Takes a given KROntology and adds it to the provided hierarchy"
  ;;Add the info from LF hierarchy
  (add-hierarchy (make-class-table-tree (kr-ontology-class-table krontology) nil) hierarchy)
  (add-hierarchy (make-class-table-tree (kr-ontology-predicate-table krontology) nil) hierarchy)
  (add-hierarchy (make-class-table-tree (kr-ontology-operator-table krontology) nil) hierarchy)
  (setf (kr-ontology-subtype-hierarchy krontology) hierarchy)
  )



