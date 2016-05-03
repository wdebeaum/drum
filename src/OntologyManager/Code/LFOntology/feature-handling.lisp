(in-package "ONTOLOGYMANAGER")   
;;;;
;;;; feature-handling.lisp
;;;;

;;;; OK, this is another of these "compatibility files". 
;;;; The function has side effects. Ask George for better solution?

;;; No IN-PACKAGE form since this code has to be loaded into different
;;; packages depending on the setting of *PACKAGE* when it is loaded.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that process define-feature declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; A nicer interface with keyword arguments and error handling
(defun add-feature (name table &key (values nil) (nameonly nil))
  "Takes a feature name and unparsed values description, and adds it to the table."
  (handler-case
      (add-feature-entry name values nameonly table)  
    (invalid-feat-spec (err) 
      (lfontology-warn "invalid feature specification for ~S:~%  ~A" name err)
      nil)
    (invalid-sem-spec (err) 
      (lfontology-warn "invalid SEM specification for ~S:~%  ~A" name err)
      nil))
  )

(defun add-feature-entry (name values nameonly table)
  " takes a feature name and a tree of values and adds a feature description to the table"
  (when (and nameonly ;; ignore the other argument
	     values)
    (lfontology-warn "values specified for name-only feature ~S" name)
    (setq values nil)
    )
  (let ( (rootvalue
	  (make-featval-description :feature-name name :value (feature-root-name name) :parent nil :children nil))
	)
    (add-feature-description 
     (make-feature-description
      :name name
      :root-value (featval-description-value rootvalue)
      :values (add-featval-tree-list name values (featval-description-value rootvalue) (list (cons (featval-description-value rootvalue) rootvalue)))
      :external nameonly)
     table)
    ))

(defun add-feature-description (featdescr table)
  " adds a feature description to the table, doing other processing as required "
  (sethash
   (feature-description-name featdescr)
   featdescr
   table)
  )

(defun add-featval-tree-list (name values parent knownvalues)
  "Parses a tree of values for a feature, converting them into feature-value structure list. Uses knownvalues to verify that values were not defined twice"
  (let ((nvalues knownvalues))
    (dolist (value values)
      (setf nvalues (add-featvals-from-tree name value parent nvalues))
      )
    nvalues))


(defun add-featvals-from-tree (name tree parent knownvalues)
  "Given a feature value tree, converts it in the list of feature structures. Uses knownvalues to verify that values were not defined twice"
  (if tree
      (let ((nvalues (add-feature-value-to-list name knownvalues (first tree) parent))
	    )
	(add-featval-tree-list name (rest tree) (first tree) nvalues)
	)
    knownvalues))

(defun add-feature-value-to-list (name valdescrs val parent)
  ;; Value itself should not yet be defined
  (when (assoc val valdescrs)
    (error (make-system-condition 'invalid-feat-spec
			     :format-control "Incorrect feature value addition: value ~S defined twice"
			     :format-arguments (list val))))
  (let ((vdescr (make-featval-description 
		 :feature-name name :value val :parent nil :children nil))
	(pdescr (cdr (assoc parent valdescrs)))
	)
    ;; Parent should be defined
    (when parent
      (when (null pdescr)
	(error (make-system-condition 'invalid-feat-spec
				 :format-control "Incorrect feature value addition: unknown parent ~S for value ~S"
				 :format-arguments (list parent val))))
      (push vdescr (featval-description-children pdescr))
      (setf (featval-description-parent vdescr) pdescr)
      )
    (cons (cons val vdescr) valdescrs)
    )
  )

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that process define-feature-value declarations
;;

(defun add-feature-arguments (featname featval arguments table)
  (let ((featdescr (gethash featname table))
	(valdef nil) ;;(parent nil)
	)
    (when (null featdescr)
      (error (make-system-condition 'invalid-feat-spec
			       :format-control "Incorrect feature arguments specification: unknown feature name ~S"
			       :format-arguments (list featname))))
    (setf valdef (cdr (assoc featval (feature-description-values featdescr))))
					(cond
     ;; if we do not have a definition and the feature was not declared external, that's an error
     ((null valdef) 
      (error (make-system-condition 'invalid-feat-spec
			       :format-control "Incorrect feature arguments specification: unknown feature value ~S for feature ~S"
			       :format-arguments (list featval featname)))
      )
     (t ;; there is a definition already - just merge in the restriction
      ;;(setf parent (featval-description-parent valdef))      
      (setf (featval-description-arguments valdef)
	(merge-sem-arguments (parse-sem-arguments arguments)
			     (featval-description-arguments valdef))
	)
      (mapcar (lambda (x) (update-feature-value x)) (featval-description-children valdef))
      )
     )
    ))


(defun add-feature-value-definition-entry (featname value table arguments implied)
  "Given a feature name and value, with unparsed arguments and implied, adds the definition (arguments and implied) to the entry"
  (let ((featdescr (gethash featname table))
	(implsem (make-typed-sem implied))
	(semargs (parse-sem-arguments arguments))
	)
    (when (null featdescr)
      (error (make-system-condition 'invalid-feat-spec
			       :format-control "Incorrect feature value description addition: unknown feature name ~S"
			       :format-arguments (list featname)))
      )
    (add-feature-value-description featdescr value semargs implsem)
    ))

(defun add-feature-value-description (featdescr value args impl)
  "given a feature description, adds arguments and implied to the value. The value itself should be previously declared"
  (let ((valdescr (cdr (assoc value (feature-description-values featdescr))))
	(parent nil) (arguments args) (implied impl)
	)
    (when (null valdescr)
      ;; The value should be declared
      (error (make-system-condition 'invalid-feat-spec
			       :format-control "Incorrect feature value addition: unknown feature value ~S for feature ~S"
			       :format-arguments (list value (feature-description-name featdescr))))
      )
    (setf parent (featval-description-parent valdescr))
    (when parent
      ;; do the merging
      (setf arguments (merge-sem-arguments arguments (featval-description-arguments parent)))
      (setf implied (merge-typed-feature-lists implied (featval-description-implied parent) nil))
      )
    
    (when (featval-description-arguments valdescr) 
      (lfontology-warn "Replacing current arguments ~S for feature value ~S ~S with ~S" 
		     (featval-description-arguments valdescr)  (feature-description-name featdescr) value arguments))
    (setf (featval-description-arguments valdescr) arguments)
    (when (featval-description-implied valdescr) 
      (lfontology-warn "Replacing current implied sem ~S for feature value ~S ~S with ~S" 
		     (featval-description-implied valdescr)  (feature-description-name featdescr) value implied))
    (setf (featval-description-implied valdescr) implied)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers to parse arguments declarations
(defun merge-sem-arguments (args inherited)
  (let ((result args))
    (dolist (element inherited)
	    ;; this argument is not implemented
	    (when (not (get-role-argument (sem-argument-role element) result))
	      (push element result))
	    )
    result))
	    

(defun parse-sem-arguments (args)
  "Takes an unparsed list of sem arguments and converts them to semargument structures"
  (mapcar #'parse-sem-argument args)
  )

(defun parse-sem-argument (arg)
  "Parses a semantic argument and makes it into a sem-argument structure"
  (let ((role (first arg)) (restr (second arg)) (implements (third arg)))
    (cond
     ((null role)
      (error (make-system-condition 'invalid-sem-entry
			     :format-control "Incorrest argument specification in ~S: role name expected" 
			     :format-arguments (list arg))))
     (t 
      (make-sem-argument :role role :restriction (make-typed-sem restr) 
			 :optionality :OPTIONAL :implements (OR implements role))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions to handle making trees in compile-hierarchy format from a feature table
;;


(defun make-featval-hierarchy-tree (featval)  
  "Takes a tree of featval desctiptions and builds up a tree for hierarchy.
Excludes -, + features."
  (cond
   ((null featval) nil)
   (t
    (let ((value (featval-description-value featval)))
      (cond
       ((equal  (symbol-name value) "-")
	nil)
       ((equal  (symbol-name value) "+")
	nil)
       (t (cons
	   value
	   (remove-if #'null
		      (mapcar (lambda (x) (make-featval-hierarchy-tree x)) (featval-description-children featval))))
	  ))
      )
    ))
  )

;; Takes a feature table and makes an "old-style" ontology list out of it
(defun make-feat-hierarchy-list (table)
  (let ((result nil))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push (make-feat-subtree val) result))
	     table)
    (remove-if #'null result)
  ))

;; takes a feature-description structure and makes it into a subtree
(defun make-feat-subtree (feat)
  (AND (not (feature-description-external feat))
       (make-featval-hierarchy-tree ;; makes the subtree rooted in the known root name
	(cdr (assoc (feature-description-root-value feat)
		    (feature-description-values feat))))
       ))



(defun sub-semvalue (feat sub super lfontology)
  "True if v1 is a subtype of v2 as feature values in the lf ontology"
  (cond
   ((eql sub super) sub)
   (t (when lfontology
	(cond
	 ((ling-ontology-subtype-hierarchy lfontology)
	  (subtype-in sub super (ling-ontology-subtype-hierarchy lfontology)))
	 (t (descendant-featval feat sub super (ling-ontology-feature-table lfontology)))))
   )))

(defun descendant-featval (feat sub super table)
  "True if sub and super are both possible values of feat and sub is a subtype of super"
  (if (eql sub super)
      sub
    (let* ((fdescr (gethash feat table))
	   (subdescr (cdr (assoc sub (feature-description-values fdescr))))
	   (superdescr (cdr (assoc super (feature-description-values fdescr)))))
      ;;(print fdescr)
      ;;    (format t "Sub: ~S Super: ~S ~% Subdescr: ~S~% Superdescr: ~S~%" sub super subdescr superdescr)
      (cond     
       ((or (null subdescr) (null superdescr)) nil)
       ((null (featval-description-parent subdescr)) nil)
       ((eql (featval-description-value (featval-description-parent subdescr)) super)
	sub)
       (t (descendant-featval feat (featval-description-value (featval-description-parent subdescr)) super table))
       ))
  ))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Functions to support consistent types facility for converting to type restrictions
;;

;; takes a type and a feature list
;; adds in the type to all featvals consistent with the list
;; NOTE: the feature list should contain ALL features required for the type
;; including the inherited features. The function assumes that the type will]be compatible
;; with any value of any feature not on the list
(defun add-declaring-type (type features lfontology)
  (let ((full-features (find-complement-features features lfontology)))
    (mapc (lambda (x) (add-declaring-type-to-feature type x lfontology))
	  (feature-list-features full-features))
    ))


;; a helper function for determining features consistent with a given
;; list takes a feature list and augments it with the list of features
;; in the table not mentioned in the list the list takes the form
;; (feature F_feature) and then can be matched
(defun find-complement-features (features lfontology)
  (let ((inffeatures (and features (infer-implied-features features lfontology)))
	)
    (complete-features-with-rootvalues inffeatures (ling-ontology-feature-type-table lfontology))
    ))
  

;; a helper function. Takes a (feature value) pair ad adds the type as
;; consistent to the featval and all its descendants
(defun add-declaring-type-to-feature (type pair lfontology)
  (cond
   ((listp (second pair))
    (mapc (lambda (x) (add-declaring-type-to-feature 
		       type (list (first pair) x) lfontology))
	  (cddr (second pair))
	  ))
   (t
    (let ((featval (get-featval-descr pair lfontology)) 
	  )
      (when featval
	(pushnew type (featval-description-declaring-types featval))
	)
      ))
   ))

(defun add-consistent-subtypes-to-feature-table (lfontology)
  (maphash (lambda (key val)
	     (declare (ignore key))
	     (mapc (lambda (x)
		     (add-consistent-subtypes-to-featval (cdr x) lfontology)
		     )
		   (feature-description-values val)
		   ))
	   (ling-ontology-feature-table lfontology)
	   ))

(defun add-consistent-subtypes-to-featval (featval lfontology)
  (add-consistent-subtypes-to-featval-parent
   (featval-description-declaring-types featval) featval lfontology)
  ;; Now, add the consistent types to the feature children
  (mapc (lambda (x)
	  (add-consistent-subtypes-to-featval-children 
	   (featval-description-consistent-supertypes featval) x lfontology)
	  )
	(featval-description-children featval)
	)
  )
  

;; Given a list of types, determines which are consistent with the featval
;; and adds them to its list of consistent subtypes to it and its parents
(defun add-consistent-subtypes-to-featval-parent (types featval lfontology)
  (let ((ctypes (featval-description-consistent-supertypes featval))
	(fparent (featval-description-parent featval))
	)
  (dolist (type types)
    (when (and (not (member type ctypes))
	       (is-consistent-subtype type featval lfontology)
	       )
      (push type (featval-description-consistent-supertypes featval))
      ))
  ;; now add the same types to parent, if they are too consistent
  (when fparent
    (add-consistent-subtypes-to-featval-parent types fparent lfontology))
  ))

;; Given a list of types, determines which are consistent with the featval
;; and adds them to its list of consistent subtypes, as well as to its children
(defun add-consistent-subtypes-to-featval-children (types featval lfontology)
  (let ((ctypes (featval-description-consistent-supertypes featval))
	)
  (dolist (type types)
    (when (and (not (member type ctypes))
	       (is-consistent-subtype type featval lfontology)
	       )
      (push type (featval-description-consistent-supertypes featval))
      ))
  ;; Now, add the consistent types to the feature children
  (mapc (lambda (x)
	  (add-consistent-subtypes-to-featval-children 
	   (featval-description-consistent-supertypes featval) x lfontology)
	  )
	(featval-description-children featval)
	)
  ))

;; Determines if a type is consistent with a featval
;; (i.e. declare it, its parent or its children and have all childrent consistent with it)
(defun is-consistent-subtype (type featval lfontology)
  (let* ((lftable (ling-ontology-lf-table lfontology))
	 (lfdescr (gethash type lftable))
	 (lfchildren (lf-description-children lfdescr))
	 (lfsem (lf-description-sem lfdescr))
	 (lfval (and lfsem (assoc (featval-description-feature-name featval)
				  (feature-list-features lfsem))))
	 )
    (cond
     ((null lfval) nil)
     ((eql (second lfval) (featval-description-value featval))
      t)
     ;; otherwise
     ((or (is-feature-parent (get-featval-descr lfval lfontology) featval)
	  (is-feature-parent featval (get-featval-descr lfval lfontology)))
      (or (null lfchildren)
	  (every (lambda (x) (is-consistent-subtype x featval lfontology))
		 lfchildren))
      )
     (t nil)
     )
    ))

;; determines if a feature described by pdescr is the parent of vdescr
(defun is-feature-parent (pdescr vdescr)
  (let ((parent (featval-description-parent vdescr)))
  (cond
   ((null parent) nil)
   ((eql (featval-description-value pdescr) (featval-description-value parent))
    t)
   (t (is-feature-parent pdescr parent))
   )
  ))

;; Given a feature, find all types that can have it (i.e. all which are consistent with it or its children)
(defun find-consistent-subtypes-for-feature (pair lfontology )
  (let (
	 (fval (get-featval-descr pair lfontology))
	)
    (when fval      
      (simplify-type-list
       (append (featval-description-declaring-types fval) 
	       (mmapcan (lambda (val) (find-consistent-subtypes-for-feature 
				       (list (first pair) (featval-description-value val)) lfontology))
			(featval-description-children fval))
	       )
       nil
       :typeh (ling-ontology-subtype-hierarchy lfontology)
       )      
      )
    ))


;; Given a feature, find all  types that had it or its parents declared
(defun find-consistent-supertypes-for-feature (pair lfontology)
  (let (
	 (fval (get-featval-descr pair lfontology)))
    (when fval      
      (simplify-type-list
       (append (featval-description-declaring-types fval) 
	       (and (featval-description-parent fval)
		    (find-consistent-supertypes-for-feature 
		     (list (first pair) (featval-description-value (featval-description-parent fval)))
		     lfontology))
	       )       
       nil
       )      
      )
    ))

(defun find-consistent-types-for-sem (sem lfontology)
  (let* ((fsem (complete-features-with-rootvalues sem (ling-ontology-feature-type-table lfontology)))
	 ;;	 (res (list 'root)) ;; // the most basic ca
	 (res nil)
	 (tmpres nil)
	 (types nil)	
	 )
    (dolist (feat (feature-list-features fsem))
      (setq types (append (find-consistent-subtypes-for-feature feat lfontology) (find-consistent-supertypes-for-feature feat lfontology)))
      (setq tmpres res) (setq res nil)
      ;;      (format t "~S ~% ~S ~% ~S ~% # ~%" feat types tmpres)
      (dolist (type types)
	(when (member type tmpres :test #'compatible-symbol-values)
	  (pushnew type res)))
      (dolist (type tmpres)
	(when (member type types :test #'compatible-symbol-values)
	  (pushnew type res)))
      )
    (simplify-type-list res nil :typeh (ling-ontology-subtype-hierarchy lfontology))
    ))

(defun find-lftypes-for-sem (sem lfontology)
  (let* ((fsem (complete-features-with-rootvalues sem (ling-ontology-feature-type-table lfontology))) ;; complete feature list
	 (tmpres nil)
	 (types nil)
	 (common-types nil)
	 )
    ;; initialize types to all possible types for these features
    (dolist (feat (feature-list-features fsem))
      (setq types (append (find-consistent-subtypes-for-feature feat lfontology) (find-consistent-supertypes-for-feature feat lfontology)))
      )
    (setq tmpres types)
    (dolist (feat (feature-list-features fsem))
      (setq types (find-consistent-subtypes-for-feature feat lfontology))
      (setq common-types (remove-duplicates (intersection types tmpres)))      
      (setq tmpres common-types)
      )
    common-types))

(defun simplify-type-list (lst reslst &key (typeh nil))
  "Simplifies the type list by removing the types subsumed by more general types on the rest of the list"
  (cond 
   ((null lst)
    reslst)
   ((member (car lst) (rest lst) :test (lambda (x y) (compatible-symbol-values x y :typeh typeh)))
    (simplify-type-list (rest lst) reslst))
   ((member (car lst) reslst :test (lambda (x y) (compatible-symbol-values x y :typeh typeh)))
    (simplify-type-list (rest lst) reslst))      
   (t ;; no supertypes for the type on rest of the list - move it to the results list
    (simplify-type-list (rest lst) (cons (car lst) reslst))
    )
  ))
  
;; Destructively modifies feattable by simplifying all type lists
(defun simplify-consistent-types (feattable &key (typeh nil))
  (maphash (lambda (key val)
	     (declare (ignore key))
	     (when val
	       (mapcar (lambda (x) (simplify-consistent-types-for-featval (cdr x) :typeh typeh)) 
		       (feature-description-values val))
	       )
	     )
	   feattable)
  )

(defun simplify-consistent-types-for-featval (fval &key (typeh nil) )
  "Replaces the consistent-types field for featval with a simplified description list"
  (setf (featval-description-declaring-types fval)
    (simplify-type-list (featval-description-declaring-types fval) nil :typeh typeh)
    ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that support dynamically adding feature values from KR ontology
;;

(defun add-value-to-feature (featname featval parent table)
  "Dynamically Adds the value featval to feature featname with a given parent"
  (let ((featdescr (gethash featname table))
	)
    (when (null featdescr)
      (error (make-system-condition 'invalid-feat-spec
			       :format-control "Incorrect dynamic feature value addition: unknown feature name ~S"
			       :format-arguments (list featname)))
      )
    (setf (feature-description-values featdescr) (add-feature-value-to-list featname (feature-description-values featdescr) featval parent))
    ))			   

(defun add-implied-rule (featname featval implied table)
  (let ((featdescr (gethash featname table))
	(valdef nil)
	)
    (when (null featdescr)
      (lfontology-warn "Incorrect feature rule specification: unknown feature name ~S"
			       featname))
    (setf valdef (cdr (assoc featval (feature-description-values featdescr))))
    (cond
     ;; if we do not have a definition and the feature was not declared external, that's an error
     ((null valdef) 
      (lfontology-warn "Incorrect feature rule specification: unknown feature value ~S for feature ~S"
			        featval featname))
      
     (t ;; there is a definition already - just merge in the restriction
      ;;(trace merge-typed-feature-lists)
      (setf (featval-description-myimplied valdef)
	(merge-typed-feature-lists implied (featval-description-myimplied valdef) nil :feattable table))
      (setf (featval-description-implied valdef)
	(merge-typed-feature-lists (featval-description-myimplied valdef) (featval-description-implied valdef) nil :feattable table))
      (mapcar (lambda (x) (update-feature-value x)) (featval-description-children valdef))
      ;;      (untrace merge-typed-feature-lists)
      )
     )
    ))

;; updates the value because the parent changed restrictions
;; so we set ours with preference to what parent has just set
(defun update-feature-value (valdef)
  (let* ((parentdef (featval-description-parent valdef))
	 )
    (when parentdef
      (setf (featval-description-implied valdef)
	(merge-typed-feature-lists (featval-description-myimplied valdef) (featval-description-implied parentdef) nil))
      (setf (featval-description-arguments valdef)
	(merge-sem-arguments (featval-description-arguments parentdef) (featval-description-arguments valdef)))
      )
    (mapcar #'update-feature-value (featval-description-children valdef))
    ))


    
    
