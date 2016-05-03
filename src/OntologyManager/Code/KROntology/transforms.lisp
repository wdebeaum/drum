;; Nate Chambers
;;


(in-package "ONTOLOGYMANAGER")



(defun add-kr-to-lf (lf-name sem lfontology krontology &key (present-sem-args nil)
							    (kr-preference 1)
							    (from-predicate nil)
							    (to-predicate t)
							    (allow-coercions t)							    
						 )
  "given an lf-name and a sem, we return a list of four: a transform name, preference, sem, and a list of semantic arguments."

  (when *trace-transform-process*
    (format t "~%~%Building entries for lf ~S and sem ~S" lf-name sem))
  (let* (
	 ;; find all the transforms applicable to this sense
	 (transforms (find-all-transforms lf-name sem lfontology krontology 
					  :found-pref kr-preference
					  :from-frame t :to-frame t
					  :from-predicate from-predicate
					  :to-predicate to-predicate))
	 ;; try to apply these transforms to the sense
	 (res (mmapcan (lambda (x) 
			 (add-transform-to-lf lf-name sem (first x) (second x) allow-coercions lfontology krontology 
					      ;; either the caller told us what semantic arguments are present,
					      ;; or, if we were not told, we assume that every argument possible to that LF is present
					      ;; so we retrieve a list of argument definitions, and convert it into the list of role names
					      :present-sem-args (or present-sem-args
								    (mapcar #'sem-argument-role (lf-description-arguments (get-lf-definition lf-name lfontology))))
					      ))
		       transforms))
	 )
    ;; tracing
    (when (and *trace-transform-process* (null transforms))
      (format t "~%  ----> No KR Maps found for lf ~S" lf-name))
    res) 
  )


(defun add-transform-to-lf (lf-name sem transform-name prefmult allow-coercions lfontology krontology &key (present-sem-args nil))
  "Adds a sem to an lf.  Sense is in word-sense-definition structure, transform is lf-kr-transform structure, result is a list of new word-sense-definitions associated with the sense"
  (when *trace-transform-process*
    (format t "~%    Applying transform ~S" transform-name ))
  
;;;;;;;!!!!!!!!!!!!!!!!!1 Error handling
;;;; If there was exception on a level below (for example, because there was a merge error in arguments - sems conflicted and exception was thrown
;;;; We ignore that and return nil
;;;; We really should have a better checking mechanism in find-all-transforms, but that's for the future
  (handler-case 
      (let* ((transform (gethash transform-name (kr-ontology-map-table krontology)))
	     (obligs (find-obligatory-lf-arguments transform))
	     )
	;; verify that all obligatory argoments are in the sense
	(when (subsetp obligs present-sem-args)
	  (cond
	   ((and (lf-kr-transform-from-frame transform) (lf-kr-transform-to-frame transform))
	    (add-frame-to-frame-transform-to-lf lf-name sem transform prefmult allow-coercions lfontology krontology))
	   ;; transform from frame to predicate, not to frame
	   ((lf-kr-transform-from-frame transform)
	    (add-frame-to-predicate-transform-to-lf lf-name sem transform prefmult lfontology krontology))
	   ;; just predicate to predicate
	   (t
	    ; 	    (add-predicate-to-predicate-transform-to-lf lf-name sem transform prefmult))
	    nil)
	   )))
    (inconsistent-feat-spec (err) 
      (when *trace-transform-process*
	(format t "~%    Exception ~A generated during transform application - transform didn't apply " err)
	)
      nil
      )
    (invalid-argument-slot-mapping (err)
      (when *trace-transform-process*
	(format t "~%    Exception ~A generated during transform application - transform didn't apply " err)
	)
      nil
      )
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame-to-frame transforms
;;

(defun add-frame-to-frame-transform-to-lf (lf-name sem transform prefmult allow-coercions lfontology krontology)
  (declare (ignore allow-coercions))
  (let* ((kr-type (lf-kr-transform-class lf-name transform krontology))
	 (kr-typedef (gethash kr-type (kr-ontology-class-table krontology)))
	 (arguments (lf-description-arguments (get-lf-definition lf-name lfontology)))
	 ;; this is a sense with coercions	 
	 (newsem (add-kr-type-to-sem kr-type sem lfontology))
	 (newargs (add-frame-to-frame-transform-to-arguments transform kr-typedef arguments lfontology krontology))
	 )
    
    ;; double list, coercions would require multiple lists from every applicable coercion
    (list (list
	   (lf-kr-transform-name transform)
	   kr-type
	   prefmult
	   newsem
	   newargs
	   ))
    ))


;;;(defun add-lf-coercions (lfname sense lfontology krontology)
;;;  (let* ((coercions (find-lf-coercion-operators lfname lfontology))
;;;	 (feattable (ling-ontology-feature-table lfontology))
;;;	 (newsenses (mmapcan (lambda (c)
;;;			       (let ((news (copy-word-sense-definition sense))
;;;				     (krnews nil)
;;;				     )
;;;				 (setf (word-sense-definition-lf news) (list ':* (lf-coercion-operator-result c) (get-lf-form lfname ':*)))
;;;				 (setf (word-sense-definition-sem news) (lf-coercion-operator-resultsem c))
;;;				 (setf (word-sense-definition-operator news) (lf-coercion-operator-operator c))
;;;				 (add-kr-to-sense news lfontology krontology :allow-coercions nil)))
;;;			     coercions))
;;;		    
;;;	 )
;;;    (mapcar (lambda (x)
;;;	      ;; Now we re-map the new senses into the coercions feature
;;;	      ;; First we need to figure out if there is a new KR type here	      
;;;	      `((operator ,(word-sense-definition-operator x))
;;;		(lf ,(word-sense-definition-lf x))
;;;		(kr-type ,(get-arg-value 'kr-type (feature-list-features (word-sense-definition-sem x))))
;;;		(sem ,(word-sense-definition-sem x)))
;;;	      )
;;;	    newsenses)
;;;    ))
  


(defun add-kr-type-to-sem (cname sem lfontology)
  "Adds the kr type to sem and merges in all implied features"
  (let* (
	 (krsem (if sem 		      ;; if sem is defined, put in the feature list type, otherwise just (kr-type cname)
		    (make-typed-sem (list (feature-list-type sem) (list 'f::kr-type cname)))
		  (make-untyped-sem (list (list 'f::kr-type cname)))))
	 (krfeatures (infer-implied-features krsem lfontology))
	 )		     
   (infer-implied-features (merge-typed-feature-lists krfeatures sem lfontology :two-way t) lfontology)
   ))



(defun find-mapping-for-argument (argument transform)
  "Given a LF argument, find the pattern that will apply to it in the transfomr definition"
  (find (sem-argument-role argument) (lf-kr-transform-arguments transform)
	:key #'transform-pattern-lf))

(defun add-frame-to-frame-transform-to-arguments (transform class-def arguments lfontology krontology)
" arguments is a list of sem-argument structures
  transform is the lf-kr-transform
  make a new list by adding in restrictions from KR table  
  then merge in the roles declared in the transform but not in the LF
"
(let ((transformed-arguments
       ;; go over the list of arguments, find the correct mapping for each of them, and apply it
       (remove nil (mapcar #'(lambda(argument) 
			       (add-frame-to-frame-transform-to-argument argument class-def 
									 (find-mapping-for-argument argument transform)
									 lfontology krontology
									 ))
			   arguments))))
  ;; Now see if transform mentioned some specific roles not
  ;; explicitly present in the arguments passed to us. For those, we assume that the only restriction is provided by KR, 
  ;; and we add the semargument structure with this information to the list
  (dolist (pattern (lf-kr-transform-arguments transform))
    (when (not (find (transform-pattern-lf pattern) transformed-arguments :key #'sem-argument-role :test #'equal))
      ;; we have not known about this role previously, so we add the information to the result list
      (let* ((rolename (transform-pattern-lf pattern))
	     ;; the restriction from the KR
	     (restr (get-pattern-slot-restriction rolename pattern class-def krontology)))
	(when (and restr (not (eql restr -)))
	  (push
	   (make-sem-argument :role rolename :optionality :optional :implements rolename
			      ;; convert the restriction into a sem structure, doing inference as part of it
			      :restriction (add-kr-type-to-sem restr (make-untyped-sem nil) lfontology))
	   transformed-arguments))
	)))
  transformed-arguments 
  ))


(defun add-frame-to-frame-transform-to-argument (arg class-def argmap lfontology krontology) 
  "arg : type sem-argument
   class-def : type class-definition
   argmap : type transform-pattern
   Takes the arg and the argmap, determines which slot the arg goes to and retrieves the restriction from the slot and
   inserts it into the argument."
  (if (null argmap) 
      (add-slot-transform arg class-def lfontology krontology)
    (let* ((role (sem-argument-role arg))
	   (restr (get-pattern-slot-restriction role argmap class-def krontology))		     
	   )
      (cond
       ((null restr)
	;; That is a real problem -- there was a map, we looked to apply it and failed
;;	(ontology-warn "Warning: impossible to use argument mapping ~% ~S ~% from  with argument ~% ~S: ~%   no corresponding slot in class ~% ~S or predicate in the predicate table"
;;		     argmap arg class-def)
	(error (make-system-condition 'invalid-argument-slot-mapping
				      :format-control "impossible to use argument mapping ~% ~S ~% from  with argument ~% ~S: ~%   no corresponding slot in class ~% ~S or predicate in the predicate table"
				      :format-arguments (list argmap arg class-def)
				      ))
	nil
	)
       (t
	 (add-kr-restriction-to-argument arg restr lfontology)))
    )))


(defun add-slot-transform (arg class-def lfontology krontology)
  "arg : type sem-argument
   class-def: type class-definition
   Attempts to add a slot transform to an argument given that it tries to map into a class with the definition class-def"
  (let* ((role (sem-argument-role arg))
	 (res (copy-sem-argument arg))
	 (lf-def (get-lf-definition role lfontology))
	 )
    ;;    (format t "Res is ~S ~%" res)
    (when lf-def
      ;; the role is defined in the ontology - see if there's a transform here
      (let*
	  ((transforms (find-all-slot-transforms role (lf-description-sem lf-def)
						 class-def lfontology krontology))
	   (restrs (remove nil
			   (mapcar (lambda (transform) (get-slot-transform-restriction arg transform class-def lfontology krontology))
				   transforms)))
	   ) 
;;;;;; FIX ME: right now we don't support disjunction on
;;;;;; semantic feature lists. The if there are multiple restrictions,
;;;;;; we will return a list which cannot be handled yet
	;;	(format t "Restrs is ~S ~%" restrs)
	(when restrs 
	  (setf (sem-argument-restriction res) 
	    (if (eql (length restrs) 1)
		(first restrs) 
	      restrs)))
	))
    res))

(defun get-slot-transform-restriction (arg transform-name class-def lfontology krontology)
  "Given an argument and a slot transform, return the restriction that results from applying that transform"
  (let* ((transform-def (gethash transform-name (kr-ontology-map-table krontology)))	  	
	 (restr (get-pattern-slot-restriction (sem-argument-role arg) (lf-kr-transform-pattern transform-def) class-def krontology))
	 )
    (when restr ;; found a compatible restriction 
      (when *trace-transform-process*
	(format t "~%    Applying slot transform ~S" transform-name))	  
      (add-kr-type-to-sem restr (sem-argument-restriction arg) lfontology))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame to predicate transforms
;;
(defun add-frame-to-predicate-transform-to-lf (lf-name sem transform prefmult lfontology krontology)
  (when *trace-transform-process*
    (format t "~%    Applying frame-to-predicate transform ~S" transform ))
  (let* ((kr-type (lf-kr-transform-class lf-name transform krontology))
	 (kr-typedef (gethash kr-type (kr-ontology-predicate-table krontology)))
	 (arguments (lf-description-arguments (get-lf-definition lf-name lfontology)))
	 ;; this is a sense with coercions	 
	 (newsem (add-kr-type-to-sem kr-type sem lfontology))
	 (newargs (add-frame-to-predicate-transform-to-arguments transform kr-typedef lf-name arguments lfontology krontology))
	 )    
    (when *trace-transform-process*
      (format t "~%    Resulting KR type is ~S~% and resulting arguments are ~S~%" kr-type newargs ))
    ;; double list, coercions would require multiple lists from every applicable coercion
    (list (list
	   (lf-kr-transform-name transform)
	   kr-type
	   prefmult
	   newsem
	   newargs
	   ))
    ))



;; arguments is in the form of (role.sem) list
;; map is the predicate-transform
;; make a new list by adding in restrictions from KR table
(defun add-frame-to-predicate-transform-to-arguments (transform cdef lf arguments lfontology krontology)
  (mapcar (lambda (x) 
	    (add-frame-to-predicate-transform-to-argument x cdef (lf-kr-transform-pattern transform) lf lfontology krontology))
	  arguments))

(defun add-frame-to-predicate-transform-to-argument (arg cdef pat lf lfontology krontology)
  "arg : type sem-argument
   class-def : type class-definition
   argmap : type transform-pattern
   Takes the arg and the argmap, determines which slot the arg goes to and retrieves the restriction from the slot and
   inserts it into the argument."
  (declare (ignore cdef))  
  (let* ((role (sem-argument-role arg))
	 (restr (get-transform-pattern-argument-restriction (list :role role)
							    lf pat krontology ))
	 )
    (cond
     ((null restr)
      ;; well, we tried, and there's no map in this pattern for the argument, we just ignore it silently
      arg
      )
     (t
      (add-kr-restriction-to-argument arg restr lfontology)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame to predicate transforms
;;
(defun add-predicate-to-predicate-transform-to-sense (sense transform prefmult lfontology krontology)
  (let* (;;(word (word-sense-definition-word sense))
	 (arguments (word-sense-definition-roles sense))
	 (argdef (get-arg-value 'argument (word-sense-definition-mappings sense)))
	 (argrole (and argdef (word-synsem-map-slot argdef)))
	 (subcatdef (get-arg-value 'subcat (word-sense-definition-mappings sense)))
	 (subcatrole (and subcatdef (word-synsem-map-slot subcatdef)))
	 (sem (word-sense-definition-sem sense))
	 (lf (word-sense-definition-lf sense))
	 (fsense (copy-word-sense-definition sense))
	 (newsem nil) (newargs nil)
	 (ctable (kr-ontology-predicate-table krontology))
	 (cname (lf-kr-transform-class lf transform krontology))
	 (preddef (gethash cname ctable))
	 )
    
    (cond
     ((null preddef)
      (parser-warn "Warning: the transform ~%~S~% is incompatible with LF ~S.~% Either the resulting class does not exist, or is not a subtype of the lf-form-default specified" transform (word-sense-definition-lf sense))
      nil
      )
     (t
      (setf (word-sense-definition-pref fsense)
	(* prefmult (word-sense-definition-pref sense))
	)	    
      
      (setf newsem (add-kr-type-to-sem cname sem lfontology))
            
      (setf newargs (add-predicate-to-predicate-transform-to-arguments transform preddef lf arguments argrole subcatrole lfontology krontology))
      (setf newargs (merge-in-defaults newargs
				       (make-roles-from-lf-arguments 
					(get-featurelist-arguments newsem lfontology))
				       ))
      
      ;; finally, add new sem and roles to the sense
      (setf (word-sense-definition-sem fsense) newsem)
      (setf (word-sense-definition-roles fsense) newargs)
      (list fsense)
      ))
    ))

;; arguments is in the form of (role.sem) list
;; map is the predicate-transform
;; make a new list by adding in restrictions from KR table
(defun add-predicate-to-predicate-transform-to-arguments (transform cdef lf arguments argrole subcatrole lfontology krontology)
  (mapcar (lambda (x) 
	    (add-predicate-to-predicate-transform-to-argument x cdef (lf-kr-transform-pattern transform) lf argrole subcatrole lfontology krontology))
	  arguments))

(defun add-predicate-to-predicate-transform-to-argument (arg cdef pat lf argrole subcatrole lfontology krontology)
  (declare (ignore cdef))
  (let ((role (first arg))  
	(restr nil))
    (cond
     ((eql role argrole)
      (setq restr (get-transform-pattern-argument-restriction :arg-var lf pat krontology)))
     ((eql role subcatrole) 
      (setq restr (get-transform-pattern-argument-restriction :subcat-var lf pat krontology)))
     )
    ;; now that we figured out the restr value, attempt to merge it in
    (if restr
	(add-kr-restriction-to-argument arg restr lfontology)
      arg)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc common functions
;;

(defun add-kr-restriction-to-argument (arg slotrestr lfontology)
  "Adds a restriction to the argument by adding a kr-type feature with the restrictio from krrestr. 
   Arg -- type sem-argument and slotrestr is the value to be given to kr-type feature"
  ;; We pass in an argument, which is not necessarily unique - it's probably directly from the LF definition
  ;; so we don't want to destroy it. We copy it, and add the restriction to the copied version
  (let ((res (copy-sem-argument arg)))
    (setf (sem-argument-restriction res)
      (add-kr-type-to-sem slotrestr (sem-argument-restriction arg) lfontology)
      )
    res
    ))

;;;;;;;;;;;;;;;
;;
;; Support - to be revised if we change predicate definitions
;;
(defun is-predicate-sense (sense)
  (let* ((args (word-sense-definition-mappings sense))
	 (len (length args))
	 )
    (AND (assoc 'ARGUMENT args)
	 (or (eql len 1)
	     (and (eql len 2) (assoc 'subcat args))
	     ))
    ))


(defun make-type-spec (semspec &key (semvar nil) (sempref 's) (feature-list-sign '$))
  "takes a semspec structure and returns the printable form:
   $ sem semspec
   need varname for null specs "
  (let ((type nil) (feat nil)	
	)
    (cond
     ((eql semspec '-) (setf type '-) (setf feat nil))     
     ((null semspec)
      (setq type (or semvar (right-var sempref '? 'sem))))
     (t
      (setf type (feature-list-type semspec)) 
      (setf feat (merge-in-defaults (feature-list-features semspec) (feature-list-defaults semspec)))      
      (when (null type) ;; have a spec, but not the order
	(error (make-system-condition 'invalid-vocab-entry
				      :format-control "Incorrect sem specification: type missing" 
				      :format-arguments nil)))      
      ))
    ;;    ;; add restriction if there's a variable type with no features
    ;;(when (and (is-variable-name type) (null feat))
    ;;  (setq type `(? ,(strip-variable-sign type) ft_phys-obj ft_abstr-obj ft_time ft_situation)))     
    ;; return $ type features list
    (cons feature-list-sign (cons type feat))
    ))




;;;;; Given a list of lexicon entries in internal format
;;;;; Print out a list in Parser format
;;;(defun make-lexicon-entries (senselist &key (pref 1.0) )
;;;  (remove-if #'null (mapcar (lambda (x) 
;;;			      (handler-case (build-parser-lexicon-entry x pref)
;;;				(invalid-vocab-entry (err) 
;;;				  (parser-warn "Invalid vocabulary entry for ~S:~%  ~A" (car x) err)
;;;				  nil)
;;;				(invalid-vocab-ref (err)		
;;;				  (parser-warn "Invalid sense definition for ~S:~%  ~A" (car x) err)
;;;				  nil)
;;;				(invalid-sem-spec (err) 
;;;				  (parser-warn "Invalid sense definition for ~S:~%  ~A" (car x) err)
;;;				  nil)			
;;;				))	      
;;;			    senselist)
;;;	     ))
;;;
;;;(defun build-parser-lexicon-entry (entry pref)
;;;  (let* ((word (word-sense-definition-word entry))
;;;	 (LF (word-sense-definition-lf entry))
;;;	 (Sem (word-sense-definition-sem entry))
;;;	 (roles (word-sense-definition-roles entry))
;;;	 (maps (word-sense-definition-mappings entry))
;;;	 (syntax (word-sense-definition-syntax entry))
;;;	 (result nil) 
;;;	 (preference (* pref (word-sense-definition-pref entry)))
;;;	 )	
;;;
;;;    ;; (:leaf (LF lf) (SEM sem) <syntactic features>
;;;    ;;(print syntax)
;;;    (setf result
;;;      (append
;;;       (list ':leaf word preference
;;;	     (list 'LF lf) (list 'SEM (make-type-spec sem :semvar '?sem :feature-list-sign '$)))             
;;;       syntax))
;;;    
;;;    (setf result (concatenate 'list  result 
;;;			      (make-role-restrictions roles)
;;;			      (build-synt-arguments maps roles)
;;;			      ))
;;;    ))



