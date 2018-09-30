;; Nate Chambers
;;
;; 1/12/04 Consolidated all the external functions to one interface file.
;;

(in-package "ONTOLOGYMANAGER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LFMapper interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform (lf &key krtype all)
  "@param lf A list of valid TRIPS LF terms.
   @param krtype The name of the KR rules to use.  Right now, there is no
   @param all Set to true if you want all possibilities returned
   distinction of rules to load.  All rules are global in *lftransforms*."
  (declare (ignore krtype)) ;; for now
  (lf-to-kr lf *lftransforms* :all all))

(defun kr-atom-transform (kr)
  "@param kr A single kr atom
   @returns A single LF type or an LF triple (:* LF WORD)"
  (kr-atom-to-lf kr (append (car *lftransforms*) (second *lftransforms*))))

(defun get-lf-atoms-for-krtype (krtype)
  "return list of all words mapped to krtype"
 (let ((res nil))
   (dolist (atom-map *lf-atom-map*)
     (when (find krtype atom-map)
       (pushnew (first atom-map) res)
     ))
   res)
 )

(defun kr-role-transform (role value type)
  "@param role A single kr role
   @param value The kr value of the role
   @param type The kr type that contains this role
   @returns A pair:  1. A list of LF terms and/or features for the role
                     2. The LF type or the LF triple (:* LF WORD)"
  (kr-role-to-lf role value type (append (car *lftransforms*) (second *lftransforms*))))

(defun kr-role-value-transform (role value)
  "@param role A single kr role
   @param value The kr value of the role
   @returns List of LF types corresponding to the reified role.  Note that this
            is specific to the AKRL ROLE-VALUE construction and not a general
            role transform."
  (kr-role-value-to-lf role value (append (car *lftransforms*) (second *lftransforms*))))

(defun kr-term-transform (term)
  "@param role A kr term with role keywords: (ont::term v1 ont::type :role ont::val)
   @returns A list of LF terms that represent the given kr"
  (kr-term-to-lf term (append (car *lftransforms*) (second *lftransforms*) (third *lftransforms*))))

(defun kr-terms-transform (terms root)
  "@param role A list of kr terms: ((ont::term v1 ont::type :role ont::val) ...)
   @param root The variable of the root term
   @returns A list of LF terms that represent the given kr terms"
  (kr-terms-to-lf terms root (append (car *lftransforms*) (second *lftransforms*) (third *lftransforms*))))

(defun lf-atom-transform (lf)
  "@param lf A single lf atom or star triple (:* LF W)
   @returns A single KR type"
  (transform-lftype lf *lftransforms*))

(defun get-transform-types ()
  "@returns A list of LF types, the types that are in the transform rules."
  (get-lf-typetransforms))

(defun get-kr-types ()
  "@returns A list of LF types, the types that are in the transform rules."
  (get-kr-typetransforms))

(defun km-to-triples (km &optional (frame-id nil))
  "@returns A list of triples, still using the KM types."
  (km-frame-to-triples km frame-id))

(defun triples-to-km (triples &optional (head-id nil))
  "@returns A KM frame equivalent to the triples, rooted at the head-id triple."
  (km-triples-to-frame triples head-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; **LF functions** available to interact with the Ontology Manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lf-sem (x &key (no-defaults nil))
  "@param x lf type  (i.e. lf_move)
  @param no-defaults set to true if you don't want the default features also
  @return list form of the feature-list structure, with all defaults
            filled in from the feature-type declarations"
  (let* ((table (ling-ontology-lf-table *lf-ontology*))
	 (x-desc (gethash x table))
	 (fl (if (not (null x-desc)) (lf-description-sem x-desc)))
	 )
    
    (if (null fl) nil
      ;; fill in default features that are not explicitly coded in
      (listify_feature-list
       (if no-defaults fl
	 (complete-features-with-defaults
	  fl (ling-ontology-feature-type-table *lf-ontology*)))))
    ))


(defun lf-arguments (x)
  "@param x lf type  (i.e. lf_move)
   @return  list of x's arguments in their original Data list format"
  (let* ((table (ling-ontology-lf-table *lf-ontology*))
	 (x-desc (gethash x table))
	 (args (if x-desc (lf-description-arguments x-desc)))
	 (retval nil)
	 )

    (cond
     ((null args) nil)
     (t
      ;; create the list, listify each argument one by one
      (dolist (arg args)
	(setf retval (cons (listify_sem-argument arg) retval)))
      retval))
    ))

(defun best-lfs-from-sem (semexpr)
  "Top level function to find compatible LFs for a SEM feature list"
  (let ((sem (cond ((consp semexpr) (make-typed-sem semexpr))
		   ((feature-list-p semexpr) semexpr)
		   (t (format t "Error: argument must be a feature list")
		      nil))))
    (when sem
      (find-best-lfs-for-sem sem *lf-ontology*))))

(defun lf-GCB (fs1 fs2)
  "@param  fs1 list form of a feature set
           ( i.e. (ft_Situation (intentional -))
   @param  fs2 list form of a feature set
   @return greatest (most-specific) common bound of the two feature sets
           nil, if the lists contain different or default features
           if there is no bound for two feature values, nil is inserted
           as the value.  ((origin nil) (object f_word))"

  (let* ((fl1 (make-typed-sem fs1))
	 (fl2 (make-typed-sem fs2))
	 (type (merge-types (feature-list-type fl1) (feature-list-type fl2) :typeh nil))
	 (list1 nil) (list2 nil)
	 (gcb nil) (result nil))
    (if (or (null fl1) (null fl2)) (return-from lf-GCB nil))

    (setf list1 (feature-list-features fl1))
    (setf list2 (feature-list-features fl2))

    ;; check list consistency
    (when (or (not (= (length list1) (length list2)))
	    (feature-list-defaults fl1)
	    (feature-list-defaults fl2))
      (lfontology-warn "GCB: feature lists must contain the same features")
      (return-from lf-GCB nil))

    ;; find greatest common bound
    (dolist (ft list1)
      (setf gcb (GCB-value (second ft)
			   (second (assoc (first ft) list2))))
      (setf result (cons (list (first ft) gcb) 
			 result))
      )
    
    (when type
      (cons type result))))


(defun lf-feature-pattern-match (fs1 fs2)
  "@param  fs1 feature set list...the pattern  (ft_situation (ft val) (ft val))
   @param  fs2 feature set list...set to match to the pattern
   @return t if every feature value in fs1 is a supertype of the
           corresponding feature value in fs2
           fs2 can contain more feature/value pairs than fs1"
  (let ((fl1 (make-typed-sem fs1))
	(fl2 (make-typed-sem fs2)))
    (if (or (null fl1) (null fl2)) nil
      (compatible-pattern-sem fl1 fl2 :typeh
			      (ling-ontology-subtype-hierarchy *lf-ontology*))))
  )


(defun lf-feature-set-merge (fs1 fs2)
  "@param  fs1 feature set list  (ft_situation (ft val) (ft val) ...)
   @param  fs2 feature set list  (ft_situation (ft val) (ft val) ...)
   @return feature set list of merge, inherited and any feature inference"
  (let ((fl1 (make-typed-sem fs1))
	(fl2 (make-typed-sem fs2)))

    (if (or (null fl1) (null fl2)) nil
      (listify_feature-list
       (get-all-sem-features 
	(merge-typed-feature-lists fl1 fl2 *lf-ontology*)
	*lf-ontology*)))
   )
  )

(defun lf-coercion-operators (lfname)
  "
  @desc Given a class, return a list of quintuples that define applicable coercions for this kr class
 
  @param  lfname  lf type 
  @return list of applicable coercions (opdef, lfname, newlf, newsem)
  where
  opdef   = lf-coercion-operator structure (these are stored directly with the lf)
  lfname  = lf type to which the coercion applies
  newlf   = lf type that results from the coercion
  newsem  = semantic features for newlf"
  
    (let ((coercions (find-lf-coercion-operators lfname *lf-ontology*))
	)
      (mapcar (lambda (opdef)
		(let* (
		       (newlf (lf-coercion-operator-result opdef))
		       (newsem (get-all-sem-features (lf-coercion-operator-resultsem opdef) *lf-ontology*))		       
		       )
		  (list (lf-coercion-operator-operator opdef) lfname newlf (listify_feature-list newsem))
		  ))
	      coercions)
      ))

(defun get-defined-sem-features ()
  (get-sorted-sem-features *lf-ontology*)
  )

(defun convert-sem-to-lf-type (sem)
  "Given a sem structure, return a general and consistent lf type
  -- in practice this function returns ont::referential-sem in most cases"
  (let ((ctype (or 
		(;;find-most-specific-lf-for-sem
		 find-general-lf-for-sem
		 ;; third -> cddr changed to fit the new output form JFA 1/04
	         ;; (make-typed-sem (cons (second sem) (replace-or-with-var (third sem))))
		 (make-typed-sem (cons (second sem) (replace-or-with-var (cddr sem))))
		 *lf-ontology*
		 )
		'ont::any-sem
		)))
    (if (listp ctype)
	;; there's read-from-string here to guarantee that <OR> will be in the package of the caller
	;; and not in the OM package
	(cons ;;(read-from-string "<OR>") ctype)
	 'ont::<or> ctype)
      ctype)
    ))

(defun get-specific-lfs-for-sem (sem)
  "Given a sem structure, find out the most specific LF type(s) consistent with it and return it"
  (let ((ctype (or 
		(find-most-specific-lfs-for-sem
		 ;; third -> cddr changed to fit the new output form JFA 1/04
	         ;; (make-typed-sem (cons (second sem) (replace-or-with-var (third sem))))
		 (make-typed-sem (cons (second sem) (replace-or-with-var (cddr sem))))
		 *lf-ontology*
		 )
		'ont::any-sem
		)))
    (if (listp ctype)
	;; there's read-from-string here to guarantee that <OR> will be in the package of the caller
	;; and not in the OM package
	(cons (read-from-string "<OR>") ctype)
      ctype)
    ))

(defun replace-or-with-var (sem)
  "Given a sem in 'print' format, replace <OR> with ? var"
  (mapcar #'(lambda (x)
	      (if (and (listp x) (listp (second x)) 
		       (symbolp (first (second x))) (eql (symbol-name (first (second x))) '<OR>))
		  `(,(first x) (? ,(gentemp) ,@(cdr (second x))))
		x))
	  sem))

;; swift -- 04/20/2006 added this accessor function for the lxm to retrieve words from an lf *and* all its sublfs 
(defun is-sublf (sub super)
  "return child if it is a subtype (child) of the supertype (parent) -- also if it's the same lf; otherwise nil"
  (sublf sub super *lf-ontology*))

(defun ont-size ()
	(hash-table-count (ling-ontology-lf-table *lf-ontology*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; **KR functions** available to interact with the Ontology Manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kr-add-to-lf (lf-name sem 
		     &key (present-sem-args nil)
			  (no-kr-preference 1)
			  (kr-preference 1)
			  (allow-coercions t)
			  (from-predicate nil)
			  (to-predicate t)
			  )
  "@param lf-name lf-type or star list - e.g. lf_move or (:* lf_move go)
   @param sem     feature list
   @return   list of 4-grams: (transform-name 
                               preference-weight
                               sem-with-kr-type
                               list-of-semantic-arguments)"
  (declare (ignore no-kr-preference)) ;; NC - doesn't seem to be used
  ;; We need to fix package issues here. If lf name is a symbol, we assume that it is a lf type, and don't convert it
  ;; if lf name is a list (:* ont::move go) we need to convert "go" into the OM package or it won't match
  (if (and (listp lf-name) (eql (first lf-name) :*))
      (setq lf-name (list (first lf-name) (second lf-name) (util::convert-to-package (third lf-name) :ontologymanager)))
    )
  (let ((res (add-kr-to-lf lf-name (make-typed-sem sem)
			   *lf-ontology* *kr-ontology*
			   :present-sem-args present-sem-args
			   :kr-preference kr-preference
			   :from-predicate from-predicate
			   :to-predicate to-predicate
			   :allow-coercions allow-coercions)))
    (mapcar (lambda (quint) 
	      (list
	       (first quint)
	       (second quint)
	       (third quint)
	       (listify_feature-list (fourth quint))
	       (mapcar #'listify_sem-argument (fifth quint))
	       ))
	    res)))

(defun kr-slots (x)
  "@param x kr class  (i.e. Cargo)
   @return the slots of x in list form"
  (let* ((table (kr-ontology-class-table *kr-ontology*))
	 (desc  (gethash x table)))
    (if desc (class-description-slots desc) nil)))

(defun lf-info-for-class (class)
"
@desc Given a class, return a triple: (transform lf sem) corresponding to the retrieved LF information for that class

  @param class kr-class (i.e. kr-type)
  @return list (transform lf sem)
  where
  transform = the 'best' lf-kr-transform for this class
  lf        = lf specified in the transform
  sem       = domain specific sem -- i.e. sem feature vector consistent with this lf, plus kr-type for this class"

  (let ((tmp (get-transform-and-type-for-class class *lf-ontology* *kr-ontology*)))
    (when tmp
      (list (first tmp) (second tmp)
	    (get-sem-for-class-type class (second tmp) *lf-ontology* *kr-ontology*)))    
    ))



(defun kr-coercion-operators (class)
  "
  @desc Given a class, return a list of quintuples that define applicable coercions for this kr class
 
  @param class kr-type 
  @return list of applicable coercions (opname, class, newtype, newlf, newsem)
  where
  opname  = name of coercion operation (e.g. medication-from-prescription)
  class   = kr class to which the coercion applies (e.g prescription)
  newtype = kr class that results from the coercion (e.g. medication)
  newlf   = lf type associated with newtype (if any)
  newsem  = semantic features for newlf with kr-type added"
  
    (let ((coercions (find-coerce-operators class *kr-ontology*))
	)
      (mapcar (lambda (opname)
		(let* ((opdef (gethash opname (kr-ontology-operator-table *kr-ontology*)))
		       (newtype (operator-description-result opdef))
		       ;; Myrosia 08/19/03 
		       ;; If the LF is not specified
		       ;; by the operator, find the best possible
		       ;; result lf based on the KR coercion type and
		       ;; return it
		       ;; (newlf (operator-description-lf opdef))
		       (newlf (or (operator-description-lf opdef)
				  (second (get-transform-and-type-for-class newtype *lf-ontology* *kr-ontology*))))
		       (newsem (get-all-sem-features (add-kr-type-to-sem newtype (get-linguistic-type-sem newlf *lf-ontology*) *lf-ontology*) *lf-ontology*))
		       )
		  (list opname class newtype newlf (listify_feature-list newsem))
		  ))
	      coercions)
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; **LF and KR Functions** available to external packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; does not utilize the ontology parameter
(defun subtype (x y &key (ontology nil))
  "@param x  one of: lf-type/lf-feature-val/kr-class/kr-predicate/kr-operator
   @param y  one of: lf-type/lf-feature-val/kr-class/kr-predicate/kr-operator
   @param ontology   lf/kr, whichever sub/super is a member of
   @return returns t if sub is a subclass of super"
  (declare (ignore ontology)) ;; maybe needed in the future
  (subtype-in x y (ling-ontology-subtype-hierarchy *lf-ontology*))
  )

;; does not utilize the ontology parameter
(defun more-specific (x y &key (ontology nil))
  "@param x  one of: lf-type/lf-feature-val/kr-class/kr-predicate/kr-operator
   @param y  one of: lf-type/lf-feature-val/kr-class/kr-predicate/kr-operator
   @param ontology lf/kr, whichever sub/super is a member of
   @return the more specific of x and y, i.e. the subtype"
  (declare (ignore ontology)) ;; maybe needed in the future
  (if (subtype x y) x
    (if (subtype y x) y nil)))

(defun get-children (lf)
  "@param x	lf-type
   @return list of children of x or nil if none"
  (let ((lf-desc (gethash lf (ling-ontology-lf-table *lf-ontology*))))
    (if lf-desc
      (lf-description-children lf-desc)
      nil
      )
    )
  )

(defun get-parent (x &key (ontology 'lf))
  "@param x    one of the following: lf-type/kr-class/kr-predicate/kr-operator
   @param ontology  lf/kr, whichever x is a member of
   @return the parent of x, nil if it does not exist"
  (let ((ontology (util::convert-to-package ontology 'ONTOLOGYMANAGER)))
       (cond 
	;; if an LF query
	((eq ontology 'lf)
	 (let ((lf-desc (gethash x (ling-ontology-lf-table *lf-ontology*))))
	   
	   (if lf-desc (lf-description-parent lf-desc) nil)
	   ))
	;; if a KR query
	((eq ontology 'kr)
	 ;; look for x in each of the 3 KR tables
	 (let ((desc (or (gethash x (kr-ontology-class-table *kr-ontology*))
			 (gethash x (kr-ontology-predicate-table *kr-ontology*))
			 (gethash x (kr-ontology-operator-table *kr-ontology*)))))
	   
	   (if desc (basic-class-description-isa desc) nil)
	   ))
	;; bad ontology parameter
	(t 'BAD-PARAMETER))
       )
  )

(defun get-parents (x &key (ontology 'lf))
  "@param x  one of the following: lf-type/kr-class/kr-predicate/kr-operator
   @param ontology  lf/kr, whichever x is a member of
   @return  list of the path from x to the top of the ontology, nil if none"
  
  (let ((parent (get-parent x :ontology ontology)))
    (if parent (cons parent (get-parents parent :ontology ontology))
      nil))
  )

(defun common-ancestor (x y &key (ontology 'lf))
  "@param x        one of: lf-type/kr-class
   @param y        one of: lf-type/kr-class
   @param ontology lf/kr, whichever x is a member of
   @return         returns the common ancestor of x and y, nil if there is none"
  (let* ((ontology (util::convert-to-package ontology 'ONTOLOGYMANAGER))
	 (table (if (eq ontology 'lf) (ling-ontology-lf-table *lf-ontology*)
		  (kr-ontology-class-table *kr-ontology*)))
	 (x-desc (gethash x table))
	 (y-desc (gethash y table)))

    (cond 
     ((or (null x-desc) (null y-desc)) nil)

     (t (if (subtype x y :ontology ontology) y
	  (common-ancestor x (if (eq ontology 'lf)
				 (lf-description-parent y-desc)
			       (predicate-description-isa y-desc))
			   :ontology ontology))
	))))



(defun defined-p (x &key (ontology nil) (cat nil))
  "@param x         an atom to check for in the ontologies
   @param ontology  lf/kr - only search in the specified ontology
   @param cat       type/feature/feature-type/class/predicate/operator
                    - only search in the specific table given
@return : if :cat is specified, returns t if x is found in the specific
             table, otherwise nil.
           : if :ontology is specified, search the tables only in that
             ontology and return the table name that it is found in.
           : if :ontology/:cat are nil, search all tables, return where
             it was found.
   ex. (defined-p 'ont::move :ontology lf) -> TYPE
   ex. (defined-p 'manner :ontology kr) -> PREDICATE"
  (let ((ontology (util::convert-to-package ontology 'ONTOLOGYMANAGER))
	(cat (util::convert-to-package cat 'ONTOLOGYMANAGER))
	)
    (cond 
     ;; the :cat key is specified, only search the specific table
     (cat
      (case cat
	(type (if (gethash x (ling-ontology-lf-table *lf-ontology*)) 
		  t nil))
	(feature (if (gethash x (ling-ontology-feature-table *lf-ontology*)) 
		     t nil))
	(feature-type (if (gethash x (ling-ontology-feature-type-table *lf-ontology*))
			  t nil))
	(class (if (gethash x (kr-ontology-class-table *kr-ontology*)) 
		   t nil))
	(predicate (if (gethash x (kr-ontology-predicate-table *kr-ontology*))
		       t nil))
	(operator (if (gethash x (kr-ontology-operator-table *kr-ontology*))
		      t nil))
	(t 'BAD-PARAMETER)))
     ;; the KR ontology, search class/pred/op tables
     ((eq ontology 'kr)
      (if (gethash x (kr-ontology-class-table *kr-ontology*))
	  (return-from defined-p 'class))
      (if (gethash x (kr-ontology-predicate-table *kr-ontology*))
	  (return-from defined-p 'predicate))
      (if (gethash x (kr-ontology-operator-table *kr-ontology*))
	  (return-from defined-p 'operator))
      )
     ;; the LF ontology, search type/feature/feature-type tables
     ((eq ontology 'lf)
      (if (gethash x (ling-ontology-lf-table *lf-ontology*))
	  (return-from defined-p 'type))
      (if (gethash x (ling-ontology-feature-table *lf-ontology*))
	  (return-from defined-p 'feature))
      (if (gethash x (ling-ontology-feature-type-table *lf-ontology*))
	  (return-from defined-p 'feature-type))
      )
     ;; nothing specified, search all tables for x
     ((null ontology)
      (or (defined-p x :ontology 'lf)
	  (defined-p x :ontology 'kr)))
     ;; bad :ontology specification
     (t 'BAD-PARAMETER))
    )
    )

(defun who-am-i ()
  "*me* must be defined in the LFTransforms atom-map for each domain"
  (get-lf-atom-map '*me*))
