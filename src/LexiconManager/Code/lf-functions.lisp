;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; **potential share?**
;; copied from Ontology/Code/ for reprogramming lexicon integration with ontology

(in-package "LEXICONMANAGER")


;; from common-functions.lisp


;; takes a typed feature list in the format type <feature list>
;; and makes it into a structure
(defun make-typed-sem (tsem)
  (when tsem
    (let* ((type (car tsem))
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
     :features required
     :defaults default)
    )))
  

;; takes an untyped feature list in the format type <feature list>
;; and makes it into a structure with variable type
(defun make-untyped-sem (features)
  (make-feature-list
   :type '?!type
   :features features
   )
  )

(defun get-lf-sem (lf &key (no-defaults t))
  "
  @param   lf : lf type e.g. lf::vehicle
  @return     : feature-list structure for the lf

  lf-sem returns a list, but in the future it will be a message 
  make-typed-sem converts from a list to the feature-list structure we use
"
  (make-typed-sem (ontologymanager::lf-sem lf :no-defaults no-defaults))
  )

(defun get-lf-arguments (lf)
  (mapcar #'parse-lf-argument	  
	  (ontologymanager::lf-arguments lf))
  )

;; adapted from lf-handling (make-lf-argument)
(defun parse-lf-argument (arg)
  "Takes a triple optionality-role-restriction and makes a sem-argument
structure out of it."
  (let* ((ess (first arg)) (role (second arg)) 
	 (restr (third arg)) (params (cdddr arg))
	 (implements (second (assoc :implements params)))	 
	 (sem (or (make-typed-sem restr) (make-untyped-sem nil)))
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
      (make-sem-argument :role role 
			 ;; if the restriction is empty, we will force it into ?type with empty features
			 ;; by calling make-untyped-sem
			 :restriction sem
			 :optionality ess :implements (OR implements role))))
    ))


(defun get-lf-description (lf)
  (make-lf-description :name lf :sem (get-lf-sem lf) :arguments (get-lf-arguments lf))
  )

(defun get-domain-senses ()
  "@return: list of lf types used in the transform rules for the current domain"
  (om::get-transform-types)
  )

(defun in-domain (lf)
  " @param lf : lf type, e.g. lf::vehicle
    @return   : t if lf is found in the table of domain-specific senses, otherwise nil
 "
  (let ((domain-senses (get-domain-senses)))
    (find lf domain-senses)
    )
  )

(defun boost (pref)
  "@param pref : defined preference (hand-set in lexicon data files, or default to 1)
   @return     : preference multiplied by the boost factor

  *kr-sense-boost* is defined in lxm::defsys.lisp
"
   (* .98 (* pref *kr-sense-boost*)) ;; 15/2/08 get preferences < 1 to improve best-first outcome
  )

(defun boost-domain-senses (wdef)
  " @param wdef : lex entry structure
    @return     : the lex entry structure with any domain-specific senses boosted
                  (lex-entry-pref multiplied by the boost factor -- default (unboosted) is 1) 
  "
  (dolist (def wdef)
    (let ((cat (lex-entry-cat def))
	  (feats (lex-entry-feats def))
	  (pref (lex-entry-pref def))
	  )
      (when (not (eq cat 'w::word))
	(if (or (lex-entry-boost-word def)
		(in-domain (strip-out-lf (get-feature-values feats 'w::lf))))
	    (setf (lex-entry-pref def) (boost pref))
	    ))))
  wdef)

(defun is-compared (alf blf compared-pairs)
  (or (intersection (list (list alf blf)) compared-pairs :test #'equal)
      (intersection (list (list blf alf)) compared-pairs :test #'equal))
  )

(defun prefer-more-specific-senses (wdef)
  (let (adjusted-def checked)
    ;; set lexical preference according to sense specificity
    (dolist (adef wdef)
      (let* ((afeats (lex-entry-feats adef))
	     (apref (lex-entry-pref adef))
	     (alf (strip-out-lf (get-feature-values afeats 'w::lf)))
	     (aid (lex-entry-id adef))
	     )
	(dolist (bdef wdef)
	   (let* ((bfeats (lex-entry-feats bdef))
		  (bpref (lex-entry-pref bdef))
		  (blf (strip-out-lf (get-feature-values bfeats 'w::lf)))
		  (bid (lex-entry-id bdef))
		  )
	     (when (not (is-compared alf blf checked))
	       (cond ((equal alf blf) ;; equal -- do nothing
		     )
		   ((om::is-sublf alf blf) ;; a is a child of b
		    (setf (lex-entry-pref bdef) (- (lex-entry-pref bdef) .01))
		    )
		   ((om::is-sublf blf alf) ;; b is a child of a
		    (setf (lex-entry-pref adef) (- (lex-entry-pref adef) .01))
		    )
		   ;; orthogonal -- do nothing
		   (t  nil))
	     (pushnew (list aid bid) checked :test #'equal)
	     (pushnew adef adjusted-def)
	     (pushnew bdef adjusted-def)
	     )
	    ))
	))
    adjusted-def)
  )


(defun prefer-senses-in-entry (wdef preferred-sense-list)
  (dolist (def wdef)
    (let* ((cat (lex-entry-cat def))
	  (feats (lex-entry-feats def))
	  (pref (lex-entry-pref def))
	  (lf (strip-out-lf (get-feature-values feats 'w::lf)))
	  )
      (when (not (eq cat 'w::word))
	(if (remove-if #'null (mapcar (lambda (preferred-lf)
					(om::is-sublf lf preferred-lf))
				      preferred-sense-list))
	    (setf (lex-entry-pref def) .99)
	  (setf (lex-entry-pref def) (- pref .01))
	  ))))
  wdef)

(defun is-subtype-of (sense-list sense)
  (remove-if #'null (mapcar (lambda (this-sense)
			      (om::is-sublf sense this-sense))
			    sense-list))
  )

(defun is-parent-of (sense sense-list)
  (remove-if #'null (mapcar (lambda (this-sense)
			      (om::is-sublf this-sense sense))
			    sense-list))
  )

;; copied from OM/Code/lf-public.lisp
;;

(defun listify-feature-list (fl)
  "fl      : feature-list structure
returns : list form of the structure, (ft_situation (container -) ( ) ...)"
  (if (null fl)
      nil
    (let ((result (list (feature-list-type fl))))
      (when (feature-list-features fl)
	(setq result (append result 
			     (list (cons :required (feature-list-features fl))))))    
      (when (feature-list-defaults fl)
	(setq result (append result (list (cons :default (feature-list-defaults fl))))))
      result)))

;; new version for use with OM
(defun merge-sem-with-lf (lf semspec)
  (make-typed-sem (om::lf-feature-set-merge 
		   (listify-feature-list semspec)
		   (listify-feature-list (lf-description-sem lf))))
  )

 (defun add-kr-to-lf (lf sem kr-pref from-pred to-pred)
  (mapcar #'(lambda (quint)
	      ;; convert into structures
	      (list (first quint) (second quint) (third quint) (make-typed-sem (fourth quint))
		    (mapcar #'parse-lf-argument (fifth quint))))
	  ;; get the list from OM
	  (ontologymanager::kr-add-to-lf lf (listify-feature-list sem) :kr-preference kr-pref			
			:from-predicate from-pred			
			:to-predicate to-pred)	 
			     ))

;; new functions for use with defining objects in the scenarion
(defun get-lf-info-for-class (class)
  (om::lf-info-for-class class)
  )

(defun find-kr-coercion-operators (class)
  (mapcar #'(lambda (quint)
	      (make-coercion-operator-description :name (first quint)
						  :argument (second quint)
						  :result (third quint)
						  :lf (fourth quint)
						  :sem (make-typed-sem (fifth quint))
						  ))
	  (ontologymanager::kr-coercion-operators class)
	  ))
    
(defun find-lf-coercion-operators (lfname)
     (mapcar #'(lambda (quad)
		 (make-coercion-operator-description :result (second quad)
						     :name (first quad)
						     :lf (third quad)
						     :sem (make-typed-sem (fourth quad))
						     ))
	  (ontologymanager::lf-coercion-operators lfname)
	  ))

