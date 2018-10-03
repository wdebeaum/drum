;; Functions and variables set for loading etc
;; which can be called by different modules 
(in-package "ONTOLOGYMANAGER")

(defvar *trap-bad-hash-entries* nil)

(defun new-lf-ontology (&key (default-lf-root nil))
  (make-ling-ontology
   :lf-table (make-hash-table)
   :default-lf-root default-lf-root
   :feature-table (make-hash-table)
   :feature-type-table (make-hash-table)
   )
  )

(defun add-linguistic-type (type lfontology &key (parent nil) (semantics nil) (arguments nil) (coercions nil) (wordnet-sense-keys nil) (comment nil))
  (add-lf type lfontology :parent parent
	  :sem semantics :arguments arguments
	  :coercions coercions :wordnet-sense-keys wordnet-sense-keys :comment comment
	  )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Accessor functions currently used by other modules
;;

(defun get-lf-definition (lf lfontology)
  "A smart function that will strip lexical form before retrieving the lf definition from the ontology"
  (and lf (gethash (strip-lf-form lf ':*) (ling-ontology-lf-table lfontology)))
  )

(defun get-lf-arguments (lf lfontology)
  "return the argument list structure, which contains role names and their semantic restrictions, from the lf description, if it exists"
  (let ((lf-def (get-lf-definition lf lfontology)) res)
    (when lf-def (setq res (lf-description-arguments lf-def)))
    res)
  )

(defun get-lf-argument-role (lf-argument)
  (if (sem-argument-p lf-argument)
      (sem-argument-role lf-argument))
  )
(defun get-lf-argument-restriction (lf-argument)
  (if (sem-argument-p lf-argument)
      (sem-argument-restriction lf-argument))
  )

(defun get-lf-argument-sem-features (lf-restriction lfontology)
  (if (feature-list-p lf-restriction)
      (list (feature-list-type lf-restriction) (feature-list-features (get-all-sem-features lf-restriction lfontology))))
  )

(defun get-lf-roles-and-restrictions (lf)
  "return a list of the role names and their semantic restrictions for a given lf type"
  (let ((lf-argument-list (get-lf-arguments lf *lf-ontology*))
	(res nil))
    (dolist (lf-argument lf-argument-list)
      (let ((lf-role (get-lf-argument-role lf-argument))
	    (lf-role-sem-features (get-lf-argument-sem-features (get-lf-argument-restriction lf-argument) *lf-ontology*)))
	(push (list lf-role lf-role-sem-features) res)))
    res)
  )


;;(defun get-linguistic-type (type lfontology)
;;  (when type (gethash type (ling-ontology-lf-table lfontology))))

(defun get-linguistic-type-sem (type lfontology)
  (let ((tdescr (get-lf-definition type lfontology)))
    (and tdescr (lf-description-sem tdescr))
    ))

(defun find-lf-coercion-operators (lf lfontology)
  (let ((def (get-lf-definition lf lfontology)))
    (and def (lf-description-coercions def))))

(defun find-best-lfs-for-sem (sem lfontology)
  "Given a sem, find the most general LF type or a set of LF's that match the SEM"
  ;; we deal with one special case when the F::TYPE feature is specified
  (let ((explicit-type (if (feature-list-p sem) (assoc 'F::TYPE (feature-list-features sem)))))
    (if explicit-type
	(list (cadr explicit-type))
	;; otherwise we add a F::TYPE based on the SEM type
	(let ((newsem (make-feature-list :type (feature-list-type sem)
					 :features (cons (list 'F::TYPE (classify-LF-type (feature-list-type sem)))
							 (feature-list-features sem)))))
	  (mmapcan #'(lambda (x) (find-best-lf-recursive x newsem lfontology))
		   (ling-ontology-lf-table-roots lfontology))
	  )))
  )

(defun classify-LF-type (semtype)
  (case semtype
    (F::PHYS-OBJ 'ONT::PHYS-OBJECT)
    (F::ABSTR-OBJ 'ont::ABSTRACT-OBJECT)
    (F::TIME 'ont::TIME-OBJECT)
    (F::SITUATION 'ont::situation-root)
    (otherwise 'ont::referential-sem)))

(defun find-best-lf-recursive (lfname sem lfontology)
  "if the LFNAME fully satisfies the SEM then return it, if it is consistent with the SEM
     then check its children, and return them if found (otherise return this one), otherwise if it is inconsistent with the SEM, then fail"
  (let* ((lfdef (get-lf-definition lfname lfontology))
	 (lfsem (lf-description-sem lfdef))
	 )
    (when lfsem
      (if (satisfies-sem lfsem sem :typeh (ling-ontology-subtype-hierarchy lfontology))
	  ;; so the LF fully satisfies the SEM
	  (list lfname)
	  (if (consistent-pattern-sem lfsem sem :typeh (ling-ontology-subtype-hierarchy lfontology))
	      ;;  LF sem was consistent, so check children
	      (let ((chres (remove-if #'null
				      (mapcar #'(lambda (x) (find-best-lf-recursive x sem lfontology))
					      (remove-if #'listp (lf-description-children lfdef))))))
		(if chres
		    ;; otherwise return the children
		    (reduce #'append chres))
		)
	      )
	  )))
  )

(defun satisfies-sem (lf-sem pattern-sem &key (typeh nil))
  "succeeds only if lf-sem satisfies the pattern-sem"
  (cond 
    ((null pattern-sem) t)
    ((null lf-sem) nil)
    (t 
     ;; we check that types are compatible and then that feature lists are comp.
     (when (satisfies-types (feature-list-type lf-sem) (feature-list-type pattern-sem) :typeh typeh)
       (compatible-pattern-feature-lists (feature-list-features pattern-sem)
					(feature-list-features lf-sem)
					:value-test (lambda (x y) (compatible-symbol-values x y :typeh typeh))
					)
      ))
    ))

(defun consistent-pattern-sem (result pattern &key (typeh nil))
  "two sems are consistent as long as no feature in the pattern is inconsistent"
  (cond 
    ((null pattern) t)
    ((null result) nil)
    (t 
     ;; we check that types are compatible and then that feature lists are comp.
     (when (merge-types (feature-list-type pattern) (feature-list-type result) :typeh typeh)
       (compatible-with-pattern (feature-list-features pattern)
				(feature-list-features result) typeh
			;;	:value-test (lambda (x y) (consistent-symbol-values x y :typeh typeh))
				)
      ))
   ))

(defun compatible-with-pattern (patfeats lffeats typeh)
  (every #'(lambda (x) (consistent-feat-values x lffeats typeh))
	 patfeats))

(defun consistent-feat-values (pat lffeats typeh)
  (let ((patfeat (car pat)))
    (or (not (assoc patfeat lffeats))
	(consistent-symbol-values (cadr pat) (cadr (assoc patfeat lffeats)) :typeh typeh))
    ))
  
(defun consistent-symbol-values (v1 v2 &key (typeh nil))
  "Calls subtype if typeh is defined and equality otherwise, returns the most specific compatible value"
  ;;(trace subtype-in compatible-symbol-values)
  (cond    
    ((eql v1 v2) v1)
    ((null v2) v1)
    ((null v1) v2)
    ((subtype-in v1 v2 typeh) v2)
    ((subtype-in v2 v1 typeh) v1)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; support for wordnet-sense-key mapping 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ont-type-wordnet-sense-keys (ont-type)
  "return the ontology type wordnet sense keys from the lf description, if it exists"
  (let ((lf-def (get-lf-definition ont-type *lf-ontology*)) res)
    (when lf-def (setq res (lf-description-wordnet-sense-keys lf-def)))
    res)
  )

(defun add-ont-type-wordnet-sense-keys (ont-type sense-keys)
  (when (and ont-type sense-keys)
    (let* ((this-def (get-lf-definition ont-type *lf-ontology*))
	   (wordnet-sense-keys (ont-type-wordnet-sense-keys ont-type)))
      (when this-def
	(cond (wordnet-sense-keys
	       (dolist (key sense-keys)
		 (if (not (find key wordnet-sense-keys :test #'equal))
		     (setq wordnet-sense-keys (append (list key) wordnet-sense-keys)))))
	      (t (setq wordnet-sense-keys sense-keys))
	      )
	(setf (lf-description-wordnet-sense-keys this-def) wordnet-sense-keys)
      ))))

(defun show-ont-type-wordnet-sense-keys ()
  "print a list of all ont types with corresponding wordnet sense keys"
  ;; force single line for synset lists
  (let ((*print-right-margin* nil)
	(*print-pretty* nil))
    (remove-if #'null (mapcar #'(lambda (ont-type)
				  (when (ont-type-wordnet-sense-keys ont-type)
				    (format t "~A ~S~%" ont-type (ont-type-wordnet-sense-keys ont-type))))
				  (get-all-ont-types)))
    ))


(defun find-general-lf-for-sem (sem lfontology)
  "Given a sem, find most general lf"
  (mmapcan #'(lambda (x) (find-general-lf-recursive x sem lfontology)) (ling-ontology-lf-table-roots lfontology))
  )

(defun find-general-lf-recursive (lfname sem lfontology)
  "Given a lf name, if it is consistent with the sem, return the general lf still consistent with the sem, or NIL otherwise"
  (let* ((lfdef (get-lf-definition lfname lfontology))
	 (lfsem (lf-description-sem lfdef))
	 (tmpres (and lfsem (compatible-pattern-sem lfsem sem :typeh (ling-ontology-subtype-hierarchy lfontology)) (list lfname))))
    (when (or tmpres (null lfsem))
      (let ((chres (mapcar #'(lambda (x) (find-general-lf-recursive x sem lfontology))
			   (remove-if #'listp (lf-description-children lfdef)))))
	(cond
	 ((null lfsem) (reduce #'append chres))
	 ((or (null chres) (member nil chres))
	  tmpres)
	 (t (reduce #'append chres))
	 )
	))
    ))

	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for subtype matching
;;

(defun add-ling-ontology-to-hierarchy (lfontology hierarchy)
  "Takes a given LFOntology and adds it to the provided hierarchy"
  ;;Add the info from LF hierarchy
  (add-hierarchy (make-lf-table-tree (ling-ontology-lf-table lfontology) nil) hierarchy
		 )
  ;; Add the feature types
  (mapcar (lambda (x) (add-hierarchy x hierarchy))
	  (make-feature-type-table-trees (ling-ontology-feature-type-table lfontology))	  
	  )
  (mapcar (lambda (x) (add-hierarchy x hierarchy))
	  (make-feat-hierarchy-list (ling-ontology-feature-table lfontology))
	  )
  (setf (ling-ontology-subtype-hierarchy lfontology) hierarchy)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for feature declarations
;;

 (defun get-sorted-sem-features (lfontology) 
  (let* ((type-table (ling-ontology-feature-type-table lfontology))
	 (types (get-all-keys type-table)) ;; feature types
	 (tfeat (mapcar (lambda (type) 
			  (cons type (feature-type-description-features (gethash type type-table))))
			types))
	 (checked-features nil)
	 (common-features nil)
	 (individ-feats nil)
	 )
    ;; now go over and find common features
    (dolist (typelist tfeat)
      (let ((type (first typelist))
	    (feat (rest typelist)))
	(dolist (complist tfeat)
	  (when (not (eql (first complist) type))
	    (setq common-features (union common-features (intersection complist feat)))
	    (setq feat (set-difference feat common-features))
	    
	    ))
	(push (cons type feat) checked-features)
	(setq individ-feats (append individ-feats feat))
	))
    
    `(,types
      ,(append common-features individ-feats)  ;; we make all feats "common" so they get a unique array position to help handle restrictions that cross multiple SEM types
      ;;,checked-features)
      ())
    ))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pretty printing 
;;


(defun format-trace-sem (sem)
  (if (feature-list-p sem)
      (format t "(~S ~S)" (feature-list-type sem) (feature-list-features sem))
    (format t "~S" sem)))



