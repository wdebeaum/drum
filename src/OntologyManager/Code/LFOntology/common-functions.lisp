
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions used by multiple modules dealing with LF ontology and features
;;
;;
(in-package "ONTOLOGYMANAGER")

(defun equal-feature-list (l1 l2)
  (let ((t1 (feature-list-type l1))
	(t2 (feature-list-type l2)))
    (cond 
     ((null l1) (null l2))
     ((null l2) nil) ; (null l1)
     ((equal-values t1 t2)
      (and (equal-feature-value-list (feature-list-features l1) (feature-list-features l2))
	   (equal-feature-value-list (feature-list-defaults l1) (feature-list-defaults l2)))
      ))
    ))

(defun equal-feature-value-list (l1 l2)
  (null (set-exclusive-or l1 l2  :test #'equal-feature))
  )

(defun equal-feature (f1 f2)
  (and (eql (car f1) (car f2))
       (equal-values f1 f2)))

;; given a feature-value pair produces the featval-description structure associated
;; with a feature value
;; raises invalid-sem-spec if feature name is not defined
;; if we're offered "or" of several feature values,
;; return the common parent
;; !!!! not fully implemented here. Common parent taken to be the root value
(defun get-featval-descr (featdescr lfontology &key (stype nil))
  "Get a featval-description structure associated with the value. If value is consistent with different feature type lists, get only those consistent with the specified type if it is non-nil"
  (let* ((feattable (ling-ontology-feature-table lfontology))
	(featname (first featdescr)) 
	(featval (second featdescr))
	(featdescr (gethash featname feattable))
	 )
    ;;    (print "here")
    (cond
     ((null featdescr)
      (Error 
       (make-system-condition 'invalid-sem-spec
	 :format-control "Invalid Feature name  ~S" 
	 :format-arguments (list featname))))
     ((atom featval)
      (if (is-variable-name featval)
	  ;; unconstrained var. Retrieve the global root value
	  (cdr (assoc (feature-description-root-value featdescr)
		      (feature-description-values featdescr)))
	;; usual case - retrieve constraints for feature
	(cdr (assoc featval 
		    (feature-description-values featdescr))))
      )
     ((is-variable-symbol (car featval))
      (if (is-positive-variable-name (second featval))
	  (create-common-featval-descr featdescr (cddr featval) stype lfontology)
	;;	  ;; a variable with a single value, and the restriction is positive, i.e. not "?!var" so regular retrieve
	;; (cdr (assoc (third featval) (feature-description-values featdescr)))
	;; otherwise multiple values, retrieve the most global descr for root value	
	(cdr (assoc (feature-description-root-value featdescr)
		    (feature-description-values featdescr)))
	))
     (T ;; some mistake here
      nil))
    ))

(defun create-common-featval-descr (featdescr vallist stype lfontology)
  "Given a list of possible values, creates a common description that can be used by the caller to infer implied features etc. If stype is non-nil, filters the description to only let in values consistent with stype"
  (let* ((alldescrs (mapcar (lambda (val)	
			      (cdr (assoc val (feature-description-values featdescr))))
			    vallist))
	 (result (copy-featval-description (cdr (assoc (feature-description-root-value featdescr)
						       (feature-description-values featdescr)))))	 
	 ;; We are trying to get what features would be implied in this common featval description
	 ;; To do that, we fist get rid of all feature values that are inconsistent with the given stype
	 ;; and then keep implied values from the rest to merge later
	 (allimplied (mapcar #'featval-description-implied 
			     (remove-if-not #'(lambda (x) (valdescr-consistent-with-p x stype)) 
					    (remove-if #'null alldescrs))))
	 (commonimplied (cond
			 ((null allimplied) nil)
			 ((eql (length allimplied) 1) (featval-description-implied (first alldescrs)))
			 (t (reduce (lambda (x y) (merge-common-features x y)) allimplied))
			 ))
	 )
    ;;    (format t "allimplied: ~S~% commonimplied: ~S~%" allimplied commonimplied)
    (cond
     ((member-if #'null alldescrs) ;; an error occured - there was a feature with no definition
      nil)
     (t
      (when commonimplied (setf (featval-description-implied result) (merge-typed-feature-lists commonimplied (featval-description-implied result) lfontology)))
      result))
    ))

(defun valdescr-consistent-with-p (valdescr stype)
  "True if valdescr is consistent with a given type, that is, if it does not imply some other type"
  (cond
   ((null stype) t)
   ((null (featval-description-implied valdescr)) nil)
   (t (merge-types (feature-list-type (featval-description-implied valdescr)) stype))
   ))

(defun merge-common-features (flist1 flist2)
  "Attempts to merge common features in the types, creating disjunctions instead, and throwing out features that don't appear in both. Returns nil if types don't unify"
  (cond
   ((or (null flist1) (null flist2)) nil)
   ((not (eql (feature-list-type flist1) (feature-list-type flist2)))
    nil)
   (t ;; have equal types, attempt to merge features
    (let ((result (make-feature-list :type (feature-list-type flist1))))
      ;;(print "equal types")
      (dolist (feat1 (feature-list-features flist1))
	      ;;(print feat1)
	(let ((feat2 (find (car feat1) (feature-list-features flist2) :key #'car)))
	  ;;  (print feat2)
	  (when feat2
	    (push (list (first feat1) (merge-feature-values (second feat1) (second feat2))) (feature-list-features result))
	    )
	  ))
      result
      ))
   ))
    
   
(defun merge-feature-values (val1 val2)
  "Merges values. If they are equal, returns val1 or val2, otherwise makes a variable"
  (cond
   ((equal val1 val2) val1)
   (t
    (let ((vals1 (if (listp val1) (cddr val1) (list val1)))
	  (vals2 (if (listp val2) (cddr val2) (list val2)))			  
	  )
      (append (list (read-from-string "?") (gentemp))
	      vals1
	      vals2)
      ))
   ))

(defun feature-root-name (feat)
  " Gives the name of the root tree of a feature"
  ;;(right-var feat 'F_Any- nil)
  (read-from-string (format nil "F::Any-~A" (symbol-name feat)))
  )

;; given a feature list and a feature table, get the list of all the
;; arguments associated with the features in the table
(defun get-featurelist-arguments (featlist lfontology)
  ;; for every feature in the featlist, we get the associated semarguments
  ;; we then concatenate the results
  (when featlist
    (mmapcan (lambda (x)
	       (when (get-featval-descr x lfontology)
		 (featval-description-arguments (get-featval-descr x lfontology))))
	     (feature-list-features featlist)
	     ))
  )

;; Tries to merge t1 and t2 and returns null if they are incompatible
;; returns the name if both types are identical names, or one is a
;; name and the other is the variable
;; returns a variable if both are variables
;; returns nil if they have non-identical names
(defun merge-types (t1 t2 &key (typeh nil) (lfontology nil) (two-way nil))  
  (cond
   ((null t1) t2)
   ((null t2) t1)
   (t
    (merge-values t1 t2 :two-way two-way :new-vars t
		  :test #'(lambda (x y) 
			    (cond
			     ((eql x y) x)
			     (typeh
			      (or (subtype-in x y typeh) (and two-way (subtype-in y x typeh)))
			      )
			     (lfontology
			      ;;(or (sub-semvalue x y lfontology) (and two-way (sub-semvalue y x lfontology))))
			      (and (eql x y) x))
			     (t nil)
			     ))
		  ))
   ))


;; T1 must be identical or a subtype of PAT
(defun satisfies-types (t1 pat &key (typeh nil) (lfontology nil) (two-way nil))  
  (let ((t1 (if (consp t1)
		(if (is-variable-name (car t1))
		    (cddr t1)
		    t1)
		(list t1)))
	(pat (if (consp pat)
		 (if (is-variable-name (car PAT))
		     (cddr PAT)
		     pat)
		 (list pat))))
    
    (if (null pat) t1
	(every #'(lambda (x) (sat-types x pat typeh))
	       t1
	       ))))

(defun sat-types (type pat typeh)
  (some #'(lambda (y)
		   (cond
		     ((eql type y) type)
		     (typeh
		      (subtype-in type y typeh)
		      )
		     (lfontology
		      ;;(or (sub-semvalue x y lfontology) (and two-way (sub-semvalue y x lfontology))))
		      (and (eql type y) type))
		     (t nil)
		     ))
	 Pat))

;; Takes 2 feature list structures and attempts to merge them together
;; both for types and names
(defun merge-feature-list-with-defaults (features defaults &key (typeh nil))
  (cond
   ((null features) defaults)
   ((null defaults) features)
   ((equal-feature-list features defaults) (copy-feature-list features))
   (t
    (let ((type (merge-types (feature-list-type features) (feature-list-type defaults) :typeh typeh)))
      (when (null type)
	;;	(break)
	(Error 
	 (make-system-condition 'invalid-sem-spec
			   :format-control "Incompatible sem types in ~S and ~S" 
			   :format-arguments (list features defaults)))
	)
      (make-feature-list
       :type type
       :features (merge-in-defaults (feature-list-features features) (feature-list-features defaults))
       :defaults (merge-in-defaults (feature-list-defaults features) (feature-list-defaults defaults))       
       ))
    )))

;; Takes 2 feature list structures and attempts to merge them together
;; both for types and names
(defun merge-typed-feature-lists (child parent lfontology &key two-way feattable)
  (cond
   ((null child) parent)
   ((null parent) child)
   ((equal-feature-list child parent) (copy-feature-list child))
   (t
    (let ((type (merge-types (feature-list-type child) (feature-list-type parent) :two-way two-way :lfontology lfontology))
	  )
      (when (null type)
	;;	(break)
	(Error 
	 (make-system-condition 'inconsistent-feat-spec
			   :format-control "Incompatible sem types in ~S and ~S" 
			   :format-arguments (list child parent)))
	)
      (make-feature-list
       :type type
       :features (add-sem-feature-lists (feature-list-features child) (feature-list-features parent) 
					lfontology :feattable feattable :two-way two-way)
       :defaults (merge-in-defaults (feature-list-defaults child) (feature-list-defaults parent))       
       ))
    )))

(defun add-sem-feature-lists (childlist parentlist lfontology &key (feattable nil) (two-way nil))
  (let ((result parentlist))
    (dolist (fval childlist)
      (setq result (add-sem-feature fval result lfontology childlist parentlist :feattable feattable :two-way two-way)))
    result))
  
(defun add-sem-feature (fval list lfontology childlist parentlist &key (feattable nil) (two-way nil))
  (let* ((name (first fval))
	 (value (second fval))
	 (olddef (assoc name list))
	 )
    (if olddef 
	(let ((nval (compatible-values value (second olddef)
				       :test #'(lambda (x y) (cond
			      
							      (lfontology (or (sub-semvalue name x y lfontology)
									      (and two-way (sub-semvalue name y x lfontology))))
							      (feattable (or (descendant-featval name x y feattable)
									     (and two-way (descendant-featval name y x feattable))))
							      (t (eql x y)))
						       )))
	      )
	    (when (and (null nval) (not (member name '(F::TYPE F::SCALE))))  ;; f-type does not need checking as its automatically generated while the ontology is being built
	      (lfontology-warn " Incompatible feature values ~S and ~S in for child ~S of parent ~S~%" fval olddef childlist parentlist) ;;(break)
	      )
	    (if (member name '(F::TYPE F::SCALE)) (setq nval value))
	    (cons (list name nval)
		  (remove name list :key #'first))
	  )
      (cons fval list))
    ))

;; takes a list of sem features and checks that they are all defined
;; raises invalid-sem-spec, or returns the list
;; features are either declared in F package, or are an element in the ontology
(defun checksemfeatures (semlist lfontology)
  (dolist (element semlist)
    (when (and (null (get-featval-descr element lfontology)) ;; couldn't find feature
	       (if (symbolp (second element))
		   (not (eq (symbol-package (second element)) *ont-package*))
		   (not (eq (car (second element)) '?))))
      (Error 
       (make-system-condition 'invalid-sem-spec
	 :format-control "Invalid Feature value in ~S" 
	 :format-arguments (list element))))    
    )
  semlist)


(defun get-role-argument (name arglist)
  (find name arglist :key #'sem-argument-role))

;; takes a typed feature list in the format type <feature list>
;; and makes it into a structure
;; Note there are two input forms: one is (<type> (<feat> <val>)*)
;;  and the other is (<type> :required (<feat> <val>)* :optional (<feat> <val>)*)
;;p  if RENAME-VARS is non null, we generte a new var name to make sure its unique
(defun make-typed-sem (tsem &optional rename-vars)
  (when tsem
    (let* ((type (make-var-unique (car tsem)))
	   (flist (cdr tsem))
	   (required (cdr (assoc :required flist)))
	   (default (cdr (assoc :default flist)))
	   )
      ;; check if there are required and default features there
    (when (and (null required) (null default))       
      ;; we assume that if no either required or default features were
      ;; specified, we treat the whole list as requireds
      (setq required flist))
    (make-feature-list
     :type type
     :features (if rename-vars (mapcar #'(lambda (x)
					   (list (car x) (make-var-unique (cadr x))))
				       required)
		   required)
     :defaults (if rename-vars
		   (mapcar #'(lambda (x)
			       (list (car x) (make-var-unique (cadr x))))
			   default)
		   default)
     )
    )))

(defun make-var-unique (class)
  (cond ((and (consp class)
	      (eq (car class) '?))
	 (list* '? (make-name-unique (cadr class)) (cddr class)))
	((is-variable-name class)
	 (make-name-unique class))
	(t
	 class)))

(defun make-name-unique (id)
  (intern (symbol-name (gensym (symbol-name id))) *ont-package*))

(defun strip-first-char (id)
  (intern (string-trim '(#\?) (symbol-name id))))

;; takes an untyped feature list in the format type <feature list>
;; and makes it into a structure with variable type
(defun make-untyped-sem (features)
  (make-feature-list
   :type '?!type
   :features features
   )
  )

(defun get-all-sem-features (sem lfontology) 
  "Given a sem, does feature inference, merges the defaults into the main list, and returns the result"
  (let* (
	 (type (feature-list-type sem))	 
	 (impsem (infer-implied-features sem lfontology))
	 (defaults  (complete-features-with-defaults 
		     (infer-implied-features (make-feature-list :type type :features (feature-list-defaults sem)) lfontology)
		     (ling-ontology-feature-type-table lfontology)))
	 )
    (merge-feature-list-with-defaults impsem defaults)
    ))

;; takes a typed feature list and infers all the implied features
;; returns the updated list
(defun infer-implied-features (featlist lfontology)
  (when featlist    
    (let ( (feats (feature-list-features featlist))
	  (tmplist (copy-feature-list featlist))
	  )
      ;; we get all the implied values for every feature
      ;; append them to the original feature list
      ;; and merge them in one by one
      (reduce 
       (lambda (x y) (merge-typed-feature-lists x y lfontology :two-way t))
       (cons tmplist
	     (mapcar (lambda (x) (and (get-featval-descr x lfontology) (featval-description-implied (get-featval-descr x lfontology))))
		     feats)
	     )	   
       )
      ))
  )

;; takes a typed feature list and infers all the implied features
;; returns the updated list
(defun infer-implied-features-list (feats lfontology)
  (when feats
      ;; we get all the implied values for every feature
      ;; append them to the original feature list
      ;; and merge them in one by one
      (reduce 
       #'merge-in-defaults
       (cons feats
	   (mapcar (lambda (x) (feature-list-features (featval-description-implied (get-featval-descr x lfontology))))
		   feats))
	   ))
       )


;; This is a function that will copy a feature list only if it is non-nil
(defun cp-feature-list (list)
  (when (not (null list))
    (copy-feature-list list))
  )

;; automatically generate a LF name default based on word
;;(defun default-lf-name (word)
;;  (right-var word 'LF_ nil))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Used by several modules for merging and unifying feature lists
;;

;; checks if 2 sems are compatible 
(defun compatible-sem (pattern result &key (typeh nil))
  (cond 
   ((null pattern) t)
   ((null result) nil)
   (t 
    ;; we check that types are compatible and then that feature lists are comp.
    (when (merge-types (feature-list-type pattern) (feature-list-type result) :typeh typeh)
      (compatible-feature-lists (feature-list-features pattern)
				(feature-list-features result)
				:value-test (lambda (x y) (compatible-symbol-values x y :typeh typeh))
				)
      ))
   ))

;; every featrue in the pattern should be compatible with the result
(defun compatible-feature-lists (pattern result &key (typeh nil))
  (every (lambda (x) 
	   (OR (not (member x result :test (lambda (x y) (eql (car x) (car y)))))
	       (member-feature x result :value-test (lambda (x y) (compatible-symbol-values x y :typeh typeh)))
	       ))
	 pattern))



(defun member-feature (f flist &key (value-test #'compatible-symbol-values))
  (member f flist :test (lambda (x y) (compatible-feature x y :value-test value-test)))
  )

(defun compatible-feature (f1 f2 &key (value-test #'compatible-symbol-values))
  (and (equal (first f1) (first f2))
       (compatible-values (second f1) (second f2) :test value-test)
       ))

;; 2 sems are compatible if every feature of the pattern is defined in result
;; with the same or more general value
(defun compatible-pattern-sem (pattern result &key (typeh nil))
  (cond 
   ((null pattern) t)
   ((null result) nil)
   (t 
    ;; we check that types are compatible and then that feature lists are comp.
    (when (merge-types (feature-list-type pattern) (feature-list-type result) :typeh typeh)
      (compatible-pattern-feature-lists (feature-list-features pattern)
					(feature-list-features result)
					:value-test (lambda (x y) (compatible-symbol-values x y :typeh typeh))
					)
      ))
   ))
  

;; every featrue in the pattern should be compatible with the result
(defun compatible-pattern-feature-lists (pattern result &key (value-test #'compatible-symbol-values))
  (every (lambda (x) 
	   (member-pattern-feature x result :value-test value-test)
	   )
	 pattern)
  )


(defun member-pattern-feature (f flist &key (value-test #'compatible-symbol-values))
  (member f flist :test (lambda (x y) (compatible-pattern-feature x y :value-test value-test)))
  )
  

(defun compatible-pattern-feature (f1 f2 &key (value-test #'compatible-symbol-values))
  (and (equal (first f1) (first f2))
       (compatible-values (second f2) (second f1) :test value-test)
       ))



(defun compatible-types (x y &key (typeh nil))
  (if (null typeh)
      (and (eql x y) x)
    (OR (subtype-in x y typeh) (subtype-in y x typeh)))
  )


;; Values are compatible if v1 is the same or subtype of v2
(defun compatible-values (v1 v2 &key (test #'compatible-symbol-values))
    (cond 
     ;; variables are always compatible
     ((is-variable-name v1) v2)
     ((is-variable-name v2) v1)
     ((and (symbolp v1) (symbolp v2))
      (funcall test v1 v2))
     ((symbolp v1)
      (if (member v1 (cddr v2) :test test)
	  v1 nil))
     ((symbolp v2)
      (if (some (lambda (x) (funcall test x v2)) (cddr v1))
	  v2 nil)
      )
     (t ;; both are lists
      (let ((cval (remove-if-not #'(lambda (x) (compatible-values x v2 :test test)) (cddr v1))))
	;; when cval is non-nil, return the first variable restricted to only values in both lists
	(and cval
	     (cons (first v1) 
		   (cons (gentemp (symbol-name (second v1))) cval)))))
     ))


(defun compatible-symbol-values (v1 v2 &key (typeh nil))
  "Calls subtype if typeh is defined and equality otherwise, returns the most specific compatible value"
  ;;(trace subtype-in compatible-symbol-values)
  (cond    
    ((eql v1 v2) v1)
    ((null v2) v1)
    ((null v1) v2)
   (t (subtype-in v1 v2 typeh))
   ))


(defun get-all-ont-types ()
  "return a list of all lf-types defined to use as an index in analyzing the lf ontology"
    ;; filter out "types" not in the lf package -- for example, SA_XX (speech acts); +; -
    (remove-if-not 
     (lambda (lf)
       (equal "ONT" (package-name (symbol-package lf)))
       )
     (get-all-keys (ling-ontology-lf-table *lf-ontology*))
     )
  )
