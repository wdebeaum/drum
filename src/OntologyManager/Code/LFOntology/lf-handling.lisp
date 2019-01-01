
;;;;
;;;; lf-handling.lisp
;;;;
;;;; Handles LF file
;;;;

(in-package "ONTOLOGYMANAGER")

;;(defun make-lfs (list feattable &key (default-root nil))
;;  "Returns the hash table with templates."
;;  (let ((table (make-hash-table)))
;;    (parse-and-add-lfs list table feattable :default-root default-root)
;;    table
;;    ))

;; parses the list of lfs and adds them to the table
;;(defun parse-and-add-lfs (list lfontology  &key (default-root nil))
;;  (mapc (lambda (x) (parse-and-add-lf x lfontology default-root)) list)
;;  )





;; Takes an LF name with unparsed sem arguments and children values
;; and adds it to the lftable as a child of the specified parent
;; a wrapper with nicer key interface and error control
(defun add-lf (name lfontology &key (sem nil) (arguments nil) (parent nil) (coercions nil) (wordnet-sense-keys nil) (comment nil))
  "Takes lf description and parses it.
Returns the list of lf-description structures or NIL in case of problems."
  
  ;; Starting point for recursion
  (handler-case (remove-if #'null 
			   (add-lf-entries name sem arguments parent coercions wordnet-sense-keys lfontology comment))
    (invalid-lf-entry (err) 
      (lfontology-warn "invalid lf specification for ~S:~%  ~A" name err)
      nil)
    (invalid-sem-spec (err) 
      (lfontology-warn "invalid SEM specification for ~S:~%  ~A" name err)
      nil)))

(defun add-ftype-to-sem-features (sem type)
  (if (member (caar sem) '(:default :required))
      (insert-ftype-in-required sem type)
      (append sem (list (list 'f::type type)))))

(defun insert-ftype-in-required (sem type)
  (when sem
    (if (eq (caar sem) :required)
	(cons (append (car sem)  (list (list 'f::type type)))
	      (cdr sem))
	(cons (car sem)
	      (insert-ftype-in-required (cdr sem) type))))
  )

;; The main processing function
;; Takes a name and unparsed SEM, ARGUMENTS  and parses and adds it to the ontology
(defun add-lf-entries (name semf args pname coercions wordnet-sense-keys lfontology comment)
  (let* ((sem nil)
	 (arguments (mapcar (lambda (x) (make-lf-argument x lfontology)) 
			   args))
	;;  Note we add the LFType as a feature automatically here
	 (semfeats (make-typed-sem (cons (car semf)
					(add-ftype-to-sem-features (cdr semf) name))))
	 (parentname nil) (parentargs nil) (parentsem nil)
	 (semargs nil) ;; argument restrictions coming from sem
	 (parentstruct (get-lf-parentstruct name pname (ling-ontology-default-lf-root lfontology) (ling-ontology-lf-table lfontology)))
	 (parentcoercions nil)
	 )
    ;; first we get the parent structure and both features and arguments from     
    (when parentstruct
      ;; we change the parentname to whatever was specified in lf description
      (setf parentname (lf-description-name parentstruct))
      (setf parentargs (copy-list (lf-description-arguments parentstruct)))
      (setf parentsem  (cp-feature-list (lf-description-sem parentstruct)))
      (setf parentcoercions (copy-list (lf-description-coercions parentstruct)))
     )
    
    ;;(format t "Before everything ~S~%" parentstruct)
    
    ;; now we check the features for correctness against feattable
    (when semfeats 
      (checksemfeatures (feature-list-features semfeats) lfontology)
      ;; we may end up doing inference, but not right away
      )  
    ;; once this is done, we need to merge in parent features first,
    ;; and then get the corresponding argument list
    (setf sem (merge-typed-feature-lists semfeats parentsem lfontology)) 
    ;;(format t "~%Afeter merge-typed-featre: sem is  ~S" sem)

    ;; got the features, get the corresponding arguments
    (setf semargs (get-featurelist-arguments sem lfontology))

    ;;(format t "semargs ~S~%" semargs)    
    ;; we get the arguments defined in the form and merge them with
    ;; the default, which itself is merged parent arguments and
    ;; arguments coming from semfeats list
    ;; mapcar here grabs the defined
    ;; arguments in text form and makes them into sem-argument structure list
    (setf arguments 
      (merge-args arguments (merge-args parentargs semargs lfontology) lfontology)		      
      )
    
    ;; Check that all coercions only involve operators that exist in the LF, and print warnings otherwise
    ;; For the correct coercions, add sem information for future use
    (setq coercions (remove nil
			    (mapcar (lambda (c) (parse-coercion c name lfontology)) coercions)))
    
    ;; this will make LF descriptions for all names specified and add then to the table
    (add-lf-description
     (make-lf-description :name name 
			  :sem sem
			  :arguments arguments
			  :parent parentname
			  :children nil
			  :wordnet-sense-keys wordnet-sense-keys
			  :coercions (merge-in-defaults coercions parentcoercions :key #'lf-coercion-operator-result :test (lambda (x y) (sublf x y lfontology)))
			  :comment comment)
     lfontology)
    ))


(defun parse-coercion (c lfname lfontology)
  "Given a coercion, parses it and returns a lf-coercion-operator structure, or nil if there's an error"
  (let* ((op (second (assoc :operator c)))
	 (reslf (second (assoc :result c)))
	 (opdef (get-lf-info op lfontology))
	 (reslfdef (get-lf-info reslf lfontology))
	 )
    (cond    
     ((not opdef)
      (ontology-warn "Unknown LF operator ~S in coercions for LF ~S - the coercion will be ignored" op lfname)
      nil)
     ((not reslfdef)
      (ontology-warn "Unknown result LF ~S in coercions for LF ~S - the coercion will be ignored" reslf lfname)
      nil)
     (t 
      (make-lf-coercion-operator
       :lfname lfname
       :operator op
       :result reslf
       :resultsem (lf-description-sem reslfdef))
      ))
    ))


(defun get-lf-parentstruct (name pname default-root lftable)   
  "Finds the description of parent for the LF in names given the specified parent name and a default root"
  ;; we take parentname to be pname unless it is null in which case we
  ;; will take default-root
  (let ((parentname (OR pname default-root)))
    ;; now determine the result - copy of the parent structure if it exists
    (cond
     ((null parentname) nil)
     ;; To avoid loops, we check so that we don't return default root as parent of itself
     ((AND (eql name parentname)
	   (eql name default-root))
      nil)
     ((eql name parentname)
      ;; Nothing can be a parent of itself
      (error (make-system-condition 'invalid-lf-entry
			       :format-control "Invalid description ~S specifies itself as a parent"
			       :format-arguments (list name)))
      )
     ((null (gethash parentname lftable))
      (error (make-system-condition 'invalid-lf-entry
			       :format-control "Invalid description ~S: Unknown parent name ~S " 
			       :format-arguments (list name parentname))))     
     (t
      (copy-lf-description (gethash parentname lftable)))
     )
    ))


;; takes a LF description and adds it to the table 
(defun add-lf-description (lf lfontology) 
  ;;(format t "~% Add-LF_description: ~S" lf)
  (let* ((parentname (lf-description-parent lf))
	(lfname (lf-description-name lf))
	(lftable (ling-ontology-lf-table lfontology))
	(args (lf-description-arguments lf))
	(impl (mapcar (lambda (x) (list (sem-argument-role x))) args))
	)
    (when parentname
      (let ((parentstruct (gethash parentname lftable)))
	(when (not parentstruct)
	  (error (make-system-condition 'invalid-lf-entry
				   :format-control "Unknown parent name ~S for LF ~S" 
				   :format-arguments (list parentname lfname))))
	;; here we are all ready to add the child to parent's list
	(pushnew lfname (lf-description-children parentstruct) :test #'equal)	      
	))
    ;; get the implementation structures, and adjust the implementor's sem features accordingly
    (dolist (arg args)
      (let* ((role (sem-argument-role arg)) 
	     (implementor (sem-argument-implements arg))
	     (idef (get-role-argument implementor args)) 
	     (ientry (assoc implementor impl))
	     )
	(when (and idef (not (eql (sem-argument-role idef) role)))
	  ;; the argument is implemented differently
	  (setf (sem-argument-restriction arg)
	    (merge-typed-feature-lists (sem-argument-restriction arg) (sem-argument-restriction idef) lfontology))
	  ;; add the info to the implementors list
	  ;; careful!!! - destructive operation
	  (setf (cdr ientry) (cons role (cdr ientry)))
	  )))
    (setf (lf-description-role-implementations lf) impl)
    ;;(format t "~%Pushing ~S defined as ~S onto LF ontology" lfname lf)
    (when (null parentname) 
      (pushnew lfname (ling-ontology-lf-table-roots lfontology)))
    (sethash lfname lf lftable)  
    ;; add type consistency information if the feature table is given
    (add-declaring-type (lf-description-name lf) 
			 (lf-description-sem lf)
			 lfontology)
    ))

(defun make-lf-argument (arg lfontology)
  "Takes a triple optionality-role-restriction and makes a sem-argument
structure out of it."
  (let* ((ess (first arg)) (role (second arg)) 
	 (restr (third arg)) (params (cdddr arg))
	 (implements (second (assoc :implements params)))	 
	 (sem (or (make-typed-sem restr t) (make-untyped-sem nil)))
	 )        
    (cond
     ((not (member ess '(:REQUIRED :ESSENTIAL :OPTIONAL)))
      (error (make-system-condition 'invalid-lf-entry
			     :format-control "Incorrect optionality specification in ~S" 
			     :format-arguments (list arg))))
     ((null role)
      (error (make-system-condition 'invalid-lf-entry
			     :format-control "Incorrest argument specification in ~S: role name expected" 
			     :format-arguments (list arg))))
     (t 
      (checksemfeatures (feature-list-features sem) lfontology)
      (make-sem-argument :role role 
			 ;; if the restriction is empty, we will force it into ?type with empty features
			 ;; by calling make-untyped-sem
			 :restriction sem
			 :optionality ess :implements (OR implements role))))
    ))


     
(defun merge-args (childlist parentlist lfontology)
  "Takes 2 lists of semarg structures and merges them in the following way:
If 2 of the same elements are defined and opitonality conflicts, the arglist
wins.
For every 2 specified elements, we merge in the semantic restrictions.
Both lists are copied to make totally new restrictions."
  (let ((result (mapcar #'copy-sem-argument childlist)) 
	(charg nil))

    (dolist (parg parentlist)
      (setf charg (get-role-argument (sem-argument-role  parg) result ))
      ;;(print def)
      (if charg ;; we found an already defined argument for the default restriction
	  (setf (sem-argument-restriction charg) 
	    (merge-typed-feature-lists (sem-argument-restriction charg) 
				 (sem-argument-restriction parg)
				 lfontology))
	;; otherwise we found nothing and simply push the default onto the list
	(push (copy-sem-argument parg) result)
	)
      )
    result
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that support converting the lf tables into trees
;; Needed for hierarchical support through subtype in unification
;;
;;
(defun make-lf-subtree (lf table allow-non-root)
  "Makes a subtree for a root entry or not a non-root entries if this is
allowed."
  (when (OR allow-non-root (not (lf-description-parent lf)))
    (append
     (list (lf-description-name lf))
     (mapcar #'(lambda (x)
		 (make-lf-subtree (gethash x table) table t))
	     (lf-description-children lf)))))


(defun make-lf-table-tree (lftable root)
  "Takes a table and a root name and makes a tree out of it"
  ;; now this makes another tree of LF's and attaches it to the ontology
  (let ((tabletrees nil))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push (make-lf-subtree val lftable nil) tabletrees)
		 )
	     lftable)
    (setf tabletrees (remove-if #'null tabletrees))
    ;; now consistency checking here
    (cond
     (root ;; unique root provided by the caller
      (cons root tabletrees))
     ;; if there's no unique root, then we must habe only 1 tree in the result
     ((eql (length tabletrees) 1)
      (first tabletrees))
     (t
      (lfontology-warn "No unique root found in lf table. Taking the first tree as the type hierarchy. The forest is ~S " tabletrees)
      (first tabletrees))
     )
    ))

;; A helper to do fast hierarchical matching if available or trace the parents through the hierarchy otherwise

(defun descendant-lf (sublf superlf  table)
  "Returns sublf if sublf is a descendant of superlf or nil otherwise"
  (let ((subdescr (gethash sublf table))
	(superdescr (gethash superlf table))
	)
    (cond 
     ((null subdescr) nil)
     ((null superdescr) nil)
     ((eql (lf-description-parent subdescr) superlf)
      t)
     ((lf-description-parent subdescr)
      ;;      (descendant-lf sublf (lf-description-parent subdescr) table))
      (descendant-lf (lf-description-parent subdescr) superlf table))
     (t nil)
     )
    ))


(defun sublf (sub super lfontology)
  "Determines if sub is a subtype of super. If lfontology has a hierarchy table, performs fast hierarchical matching using subtype-in. Otherwise, walks the class tree"
  (if (ling-ontology-subtype-hierarchy lfontology)
      (subtype-in sub super (ling-ontology-subtype-hierarchy lfontology))
    (descendant-lf sub super (ling-ontology-lf-table lfontology))
    ))

(defun lex-sublf (sub super lfontology)
  "Determines if sub is a subtype of super. If lfontology has a hierarchy table, performs fast hierarchical matching using subtype-in. Otherwise, walks the class tree"
  (cond
   ((eql sub super)
    sub)
   (t
    ;; we should be conscious of lexical info here
    (let* ((sub-lf (strip-lf-form sub ':*))
	   (super-lf (strip-lf-form super ':*)))
      (cond
       ((eql super-lf super)
	;; if super does not have a lexical form, then simple subtype matching on lfs works -- but we make sure to return the real form
	(and (sublf sub-lf super-lf lfontology)
	     sub)
	)
       ((eql sub-lf sub)
	;; if super has lexical form, and sub does not, this is a definite fail
	nil)
       (t ;; both sub and super have lexical form - they can only be subtypes if they are equal
	(and (equal sub super)
	     sub)
	))
      ))
   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Awarenes of * names
;;

;;;(defun get-lf-parent-and-form (lf)
;;;  (let ((spos (position #\* (symbol-name lf) :test #'eql)))
;;;    (if spos
;;;	(values
;;;	 (read-from-string (subseq (symbol-name lf) 0 spos))
;;;	 (read-from-string (subseq (symbol-name lf) (+ spos 1)))
;;;	 )
;;;      (values lf nil)
;;;      )
;;;    ))


(defun convert-star-to-operator-form (lf form-operator)
  "Given a a*b name, converts it to the operator form"
  (let* ((namestr (format nil "~S" lf))
	 (spos (position #\* namestr :test #'eql)))
    (if spos
	(list form-operator
	 (read-from-string (subseq namestr 0 spos))
	 (read-from-string (subseq namestr (+ spos 1)))
	 )
      lf
      )
    ))
  

(defun get-lf-parent-and-form (lf form-operator)
  "Given a lf possibly using a given form operator, split it into the parent and form"
  (if (and (listp lf) (eql (first lf) form-operator))
      (values (second lf) (third lf))
    (values lf nil)))


(defun get-lf-form (lf form-operator &key package)
  (multiple-value-bind
      (parent form)
      (get-lf-parent-and-form lf form-operator)
      (declare (ignore parent))
    ;;      (or form lf)      
    (if package
	(intern (symbol-name form) package)
      form
      )
    )
  )

(defun strip-lf-form (lf form-operator)
  (multiple-value-bind
      (parent form)
      (get-lf-parent-and-form lf form-operator)
      (declare (ignore form))
    parent
    )
  )

(defun get-lf-info (symbol lf-ontology)
  "finds out whatever information is defined for the symbol in the lf ontology"
  (gethash symbol (ling-ontology-lf-table lf-ontology))
  )


(defun find-parent-if (pred lf lftable)
  "Returns the lf or the first of its ancestors that satisfies a given condition. Null if no parents satisfy the condition. Pred takes lf-description structure as an agrument"
  (cond 
   ((null lf) nil)
   ((null (gethash lf lftable)) nil)
   ((funcall pred (gethash lf lftable)) lf)   
   (t (find-parent-if pred (lf-description-parent (gethash lf lftable)) lftable))
   ))
