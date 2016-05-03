(in-package "ONTOLOGYMANAGER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for transform application
;;

(defun get-transform-definition (name krontology)
  "Returns the definition of a transform from KR ontology"
  (gethash name (kr-ontology-map-table krontology))
  )

(defun apply-frame-to-predicate-transform (lfname mapdef rolelist krontology)
  
  "Applies a frame-to-predicate transform under the following assumptions:
    lfname is the lf from the frame
    rolelist is the list of role substitutions in the form rolename value
    returns a list expression (predicate arg1 arg2 ...) where arguments are either 
    constants from the transform or were substituted from the role list"
  (evaluate-pattern-expression (lf-kr-transform-pattern mapdef) krontology
			       :lf lfname :rolelist rolelist)
  )


(defun apply-predicate-to-predicate-transform (lfname mapdef argval subcatval krontology)
  (evaluate-pattern-expression (lf-kr-transform-pattern mapdef) krontology
			       :lf lfname :argval argval :subcatval subcatval
			       )
  )
			  
(defun apply-slot-pattern (lfname pattern argval subcatval classname krontology)
  (let* ((expr (transform-pattern-pattern pattern))
	 (classdef (gethash classname (kr-ontology-class-table krontology)))
	 (slotname (expression-class lfname expr))
	 (slotdef (assoc slotname (class-description-slots classdef)))
	 )
    (cond
     ((null slotdef) ;; haven't found a slot - try to treat it as a predicate
      (evaluate-pattern-expression pattern krontology
				   :lf lfname :argval argval :subcatval subcatval
				   ))
     ;; have a slot definition -- return the default form supplied for transforms into slots
     (t (evaluate-pattern-element expr :lf lfname :argval argval :subcatval subcatval))
     )))
     
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support functions

(defun find-argument-transform-in-map (role mapdef ontology)
  "Given a mapping definition, finds an argument transform in it"
  (find-lf-argument role (lf-kr-transform-arguments mapdef) ontology :skey #'transform-pattern-lf)
  )


(defun find-lf-argument (arg arglist ontology &key skey)
  (find arg arglist :key skey :test (lambda (x y) (lex-sublf x y ontology)))
  )

(defun get-transform-pattern-argument-restriction (role lf transform krontology)
  (get-pattern-argument-restriction role (transform-pattern-pattern transform) lf (transform-pattern-lf-form-default transform) krontology t))
      
(defun get-pattern-slot-restriction (role pattern cdef krontology) 
  "Given a pattern, assume it applies to a slot and attempt to find and return a restriction"
  (let ((pat (transform-pattern-pattern pattern)))
    (cond 
     ((member :subcat-var (rest pat))
      (nth (position :subcat-var (rest pat)) (get-pattern-argument-types role pat cdef krontology))
      )
     (t ;; otherwise we need to dig down in the operators in the pattern -- we don't really care if the top-level predicate exists, this has hopefully been checked before
      (let ((res (remove-if #'null
			    (mapcar (lambda (arg)
				      (get-pattern-argument-restriction :subcat-var arg role 
									(transform-pattern-lf-form-default pattern)
									krontology nil))
				    (cdr pat))))
	    )
	(when (> (length res) 1)
	  (ontology-warn "Problem with a slot mapping ~S: multiple use of :arg-var. Taking the first restriction" pattern))
	(first res)
	))
     )))


(defun get-pattern-slot-coercions (role pattern cdef krontology) 
  "Given a pattern, assume it applies to a slot and attempt to find and return a restriction"
  (declare (ignore role cdef)) ; gf: See comment below
  (let ((pat (transform-pattern-pattern pattern)))
    ;; gf: 31 Oct 2009: Deleted ancient dead code that used variable PAT,
    ;; but can't know whether call to TRANSFORM-PATTERN-PATTERN is still
    ;; needed for its side-effects or not. Hence the strange IGNORE below.
    (declare (ignore pat))
    (mapcar (lambda (descr) (first (operator-description-arguments descr)))
	    (mapcar (lambda (x) (gethash x (kr-ontology-operator-table krontology))) (transform-pattern-coerce pattern))
	    )
    ))

;;;;;;;;;; A helper to get the argument restriction from a complex expression
;;; Searches for the element given, determines what encompasses it, and returns a corresponding operator or predicate restriction
(defun get-pattern-argument-restriction (arg pat lf lfdefault krontology predicate) 
  (cond
   ((null pat) nil)
   ((symbolp pat) nil)
   (t (let ((argpos (position arg (rest pat) :test #'equal)))
	(if argpos ;; argument found
	    (let* ((opname (expression-class lf pat :lfform lfdefault))	   
		   (deftable (if predicate 
				 (kr-ontology-predicate-table krontology) 
			       (kr-ontology-operator-table krontology)))
		   (opdef (or (gethash opname deftable) (gethash lfdefault deftable)))
		   )
	      (and opdef (nth argpos (predicate-description-arguments opdef)))
	      )
	  ;; no argument - recurse trying to find a non-null value
	  (let ((res (remove-if #'null
				(mapcar (lambda (patarg)
					  (get-pattern-argument-restriction arg patarg lf lfdefault krontology nil))
					(cdr pat)))
		     ))
	    (when (> (length res) 1)
	      (ontology-warn "Problem with a slot mapping: multiple use of argument ~S. Taking the first restriction" arg))
	    ;; take the first of res as a result
	    (first res)
	    ))
	))      
   ))


(defun find-obligatory-lf-arguments (mapdef)
  "Given a transform definition, find the names of lf arguments that must be defined"
  (let* ((arguments (lf-kr-transform-arguments mapdef))
	)
    (cond
     ((not (lf-kr-transform-from-frame mapdef))
      nil)
     ((lf-kr-transform-to-frame mapdef)
      (mapcar #'transform-pattern-lf 
	      (remove-if-not #'transform-pattern-oblig arguments)))
     (t ;; this is a frame-to-predicate transform. We have to go into the tree structure
      ;; and find all role names there
      (mapcar #'second
	      (tree-find-if (lambda (x) (and (listp x) (eql (car x) :role))) arguments)
	      )
      ))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for finding relevant transforms 
;;

(defun find-all-transforms (lf sem lfontology krontology 
			    &key (found-pref nil) 
				 (from-frame t) (from-predicate nil) 
				 (to-frame t) (to-predicate nil)				 
				 )
  (find-all-transforms-helper lf lf sem lfontology krontology
			      :found-pref found-pref
			      :from-frame from-frame
			      :from-predicate from-predicate
			      :to-frame to-frame
			      :to-predicate to-predicate))

(defun find-all-transforms-helper (truelf currentlf sem lfontology krontology 
				   &key (found-pref nil) 
					(from-frame t) (from-predicate nil) 
					(to-frame t) (to-predicate nil)				 
					)
  "Find all mappings compatible with a given truelf and sem among the transforms for currentlf.  
   IF found-pref is set, return them with preference found-pref, or 1 if there are none and transforms are taken from the parent"
  
  (let* ((result nil)
	 (cmaps nil) 
	 (lfdef (get-lf-definition currentlf lfontology))
	 (abstract-only nil)
	 )
    
    (when *trace-transform-process*
      (format t "~%Finding possible maps for form ~S and parent ~S." truelf currentlf)
      (if (lf-description-kr-transforms lfdef)
	  (format t " Candidates are ~S" (lf-description-kr-transforms lfdef))
        (format t "~%        ----> No candidates found")))
  
    (when lfdef
	  (setf cmaps (remove-if-not 
		       (lambda (x) (and
				    (is-compatible-transform (first x) (second x) truelf sem lfontology krontology) 
				    (correct-representation-transform x from-frame to-frame from-predicate to-predicate krontology)))
		       (lf-description-kr-transforms lfdef))) 
	  ;; now see if there are only abstract applicable transforms left
	  (setq abstract-only (and cmaps 
				   (every (lambda (x) 
					    (lf-kr-transform-abstract 
					     (get-transform-definition (second x) krontology))) 
					  cmaps)))
	  (when (or (null cmaps) abstract-only)
	    ;; No form applicable transforms - and all the remaining are abstract -- get the defaults
	    (setf cmaps (remove-if-not 
			 (lambda (x) 
			   (and
			    (is-compatible-transform (first x) (second x) truelf sem lfontology krontology)
			    (correct-representation-transform x from-frame to-frame from-predicate to-predicate krontology)))
			 
			 (lf-description-default-kr-transforms lfdef)))
	    )
    
	  (cond
	   ((and (null cmaps) (lf-description-parent lfdef)) ;; found nothing useful on this level
	    ;; if there were abstract transforms here, higher up levels should not get boosted, otherwise they can
	    (setq result (find-all-transforms-helper truelf (lf-description-parent lfdef) sem lfontology krontology 
						     :found-pref (and found-pref (if abstract-only 1.0 found-pref))
						     :from-frame from-frame :from-predicate from-predicate
						     :to-predicate to-predicate :to-frame to-frame))
	    )
	   (t ;; good transforms found
	    (setq result (sort-transforms-by-specificity 
			  (remove-if (lambda (x) (lf-kr-transform-abstract 
						  (get-transform-definition (second x) krontology)))
				     cmaps)
			  found-pref lfontology krontology))
	    ))
	  result
	  ))
  )

(defun correct-representation-transform (tspec from-frame to-frame from-predicate to-predicate krontology)
  "A helper instead of lambda function. Gets the transform as listed on the lf (consists of applicable lf and transform def) and makes sure that it satisfies the from and to frame conditions"
  (let ((mapdef (get-transform-definition (second tspec) krontology)))
    (and (or (and to-predicate
		   (not (lf-kr-transform-to-frame mapdef)))
	     (and to-frame
		  (lf-kr-transform-to-frame mapdef))
	      )
	  (or (and from-predicate
		   (not (lf-kr-transform-from-frame mapdef)))
	      (and from-frame
		   (lf-kr-transform-from-frame mapdef))
	      ))
    ))
    
(defun find-all-slot-transforms (truelf sem cdef lfontology krontology)
  (find-all-slot-transforms-helper truelf truelf sem cdef lfontology krontology))
  
(defun find-all-slot-transforms-helper (truelf currentlf sem cdef lfontology krontology)
  "Given a lf and sem, find all role transforms that could be applied to this role. Sort in order from more to less specific." 
  (let ((result nil)
	(cmaps nil) 
	(lfdef (get-lf-definition currentlf lfontology))
	)
    
    (setf cmaps (remove-if-not 
		 (lambda (x) (is-compatible-slot-transform (first x) (second x) truelf sem cdef lfontology krontology)) 
		 (lf-description-kr-transforms lfdef)))
    
    (setf result (sort-transforms-by-specificity cmaps nil lfontology krontology))
    ;;(setf result (mapcar #'second cmaps))
    ;; if there are no compatible mappings on LF, search in parent
    (when (and (null result)
	       (lf-description-parent lfdef))
      ;; if we had to search the parent, we lose the preference for being defined
      (setf result (find-all-slot-transforms-helper truelf (lf-description-parent lfdef) sem cdef lfontology krontology))
      ) 
    result
    ))

(defun is-compatible-slot-transform (maplf mapname truelf sem cdef lfontology krontology)
  "Determine if the slot transform is compatible with specified class definition, truelf and SEM values, or a corresponding predicate"
  (declare (ignore cdef))
  (let* ((mapdef (get-transform-definition mapname krontology))
	 (pat (lf-kr-transform-pattern mapdef))
	 )
    ;; first verify compatibility for maplf and lf-parent
    (and
     (not (lf-kr-transform-to-frame mapdef))
     (lex-sublf truelf maplf lfontology)
     (is-form-compatible-pattern (get-lf-form truelf ':* :package :ont) pat krontology :to-frame nil )
     (compatible-pattern-sem (lf-kr-transform-sem mapdef) sem :typeh (ling-ontology-subtype-hierarchy lfontology))
     ))
  )
	      
(defun is-compatible-transform (maplf mapname truelf sem lfontology krontology)
  "Determine if the transform is compatible with specified truelf and SEM values, and maps to an existing class"
  (let* ((mapdef (get-transform-definition mapname krontology))
	 (pat (lf-kr-transform-pattern mapdef))
	 (answer   (and  
		    ;;(not (lf-kr-transform-abstract mapdef))
		    (lex-sublf truelf maplf lfontology)
		    (compatible-pattern-sem (lf-kr-transform-sem mapdef) sem :typeh (ling-ontology-subtype-hierarchy lfontology))
		    ))		   
	 )        
    
    (when *trace-transform-process*
      (if (null answer) 
	  (progn
	    (Format t "~%   Map ~S is not compatible with LF ~S and SEM ~%       " mapname truelf)
	    (format-trace-sem sem)
	    (cond 
	     ((not (compatible-pattern-sem (lf-kr-transform-sem mapdef) sem :typeh (ling-ontology-subtype-hierarchy lfontology)))
	      (format t "~%    Because ~%     The Sem is not compatible with map SEM ")
	      (format-trace-sem (lf-kr-transform-sem mapdef)))
	     ))	
	(format t "~%  Map ~S is compatible with LF ~S, will check for class correctness ~%" mapname truelf)))    
    (and answer
	 (is-form-compatible-pattern (get-lf-form truelf ':* :package :ont) pat  krontology :to-frame (lf-kr-transform-to-frame mapdef))
	 )
    ))

(defun is-form-compatible-pattern (lf-form pat krontology &key (to-frame t))
  "Determines if the transform pattern is compatible with a given lf-form. Basic check only: if the pattern specifies a class name, verifies that the name is in fact in the class table. If the pattern specifies a lf-form, verifies that the form exists in the ctable and is a subtype of the pform in the pattern"
  (let* ((ctable (if to-frame (kr-ontology-class-table krontology) (kr-ontology-predicate-table krontology))) 
	 (cname (transform-pattern-valid-class lf-form pat ctable :lfform lf-form))	 
	 (cdef (gethash cname ctable))
	 (pform (transform-pattern-lf-form-default pat))
	 (answer nil)
	 )
    (setq answer (and cdef 
		      (or (null pform) (subclass cname pform krontology))))
    (when *trace-transform-process*
      (cond
       ((not (gethash cname ctable))
	(format t "Class name ~S is not defined in class table~%" cname))
       ((and pform (not (subclass cname pform krontology)))
	(format t "Class ~S is not subtype of parent type ~S~%" cname pform))
       ))
    answer    
    ))


(defun sort-transforms-by-specificity (transflist foundpref lfontology krontology &optional sorted-so-far)
  "Sort the transforms in order of applications. Most specific first, then less specific, with preference adjusted accordingly. Remove transforms if they have the same LF but different forms"
  (let (
	(form-transforms nil)
	(no-form-transforms nil))
    (dolist (tr transflist)
      (if (get-lf-form (first tr) ':*)
	  (push tr form-transforms)
	(push tr no-form-transforms))
      )
    (sort-transforms-by-specificity-helper
     (append form-transforms (remove-if (lambda (x) (member (strip-lf-form (first x)  ':*)
							    form-transforms
							    :key (lambda (y) (strip-lf-form (first y) ':*))
							    ))
					no-form-transforms)
	     )
     foundpref lfontology krontology sorted-so-far)
    
  ))
  
  
(defun sort-transforms-by-specificity-helper (transflist foundpref lfontology krontology &optional sorted-so-far)
  "Sort the transforms in order of applications. Most specific first, then less specific, with preference adjusted accordingly"
  ;; for starters, we find cfmaps with the most specific lf forms - they should be applied first
  (let* ((maptable (kr-ontology-map-table krontology))
	 (most-specific-lfs nil)
	 (most-specific-classes nil)
	 (elform nil) (elclass nil) 
	 (remainder nil) 
	 (no-resort nil)
	 (classlist (mapcar (lambda (x) (get-pattern-class-name 
					 (lf-kr-transform-pattern (gethash (second x) maptable))))
			    transflist))		    
	 (eclist classlist) 
	 (newsort nil)
	 )
    (dolist (el transflist)
      (setf elform (first el))
      (setf elclass (first eclist))
      ;;(format t "~% El: ~S ElClass: ~S transflist: ~S  Classlist: ~S no-resort: ~S remainder: ~S ~%" el elclass transflist classlist no-resort remainder)
      (cond
       ((not (find-if (lambda (x) (and (not (equal (first x) elform)) (lex-sublf (first x) elform lfontology))) transflist))
	;; this is the most specific lf we found
	(push el most-specific-lfs)
	)
       ((not (find-if (lambda (x) (and (not (equal x elclass)) (subclass x elclass krontology))) classlist))
	(push el most-specific-classes))
       (t (push el remainder))
       )
      ;; looping
      (setf eclist (rest eclist))
      )
    ;; now figure out what happened
    (cond
     (most-specific-lfs ;; found some very specific lfs - they go first, and we ignore what happened to classes
      (setq no-resort most-specific-lfs)
      (setq remainder (append most-specific-classes remainder))
      )
     (t
      ;; no specific lfs - look for most specific classes
      (setq no-resort most-specific-classes))
     )    
    (setf newsort (append sorted-so-far 
			  (mapcar (lambda (x) 
				    (if foundpref (list (second x) foundpref) (second x))) no-resort)
			  ))
    (cond
     ((null remainder)
      newsort)
     (t      
      (sort-transforms-by-specificity-helper remainder (and foundpref (* foundpref *more-specific-preference-adjustment*)) lfontology krontology newsort)
      ))
    ))
    
    


;;;;; FUNCTIONS FOR COERCION
(defun find-coercion-operators (class-type slot-name apparent-type krontology)
  (let* ((cdef (gethash class-type (kr-ontology-class-table krontology)))
	 (roledef (second (assoc slot-name (class-description-slots cdef))))
	 )
    (cond 
     ((subclass apparent-type (second roledef) krontology)
      nil)
     (t ;; otherwise look through all operators for possible coercion
      (let ((result nil))
	(maphash (lambda (key val)
		   (declare (ignore key))
		   (when (subclass apparent-type (first (operator-description-arguments val)) krontology)
		     (push (list (operator-description-name val) 
				 (operator-description-result val))
			   result)
		     ))
		 (kr-ontology-operator-table krontology))
	result))
     )
    ))
	
      
