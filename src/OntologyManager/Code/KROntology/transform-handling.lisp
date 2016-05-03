;; transform-handling.lisp
;; handling for more complex transform definitions
;;
(in-package "ONTOLOGYMANAGER")

(defvar *verbose-mapping-warnings* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; New version of transforms
;;


;; A wrapper for error control
(defun add-frame-to-frame-transforms (name krontology lfontology &key preds arguments parent default sem abstract)
  "Adds predicate mappings with a given base name. "
  (handler-case (add-transform-entries name krontology lfontology preds default t t sem arguments parent abstract)
    (invalid-mapping-entry (err) 
      (krontology-warn "invalid class specification for ~S:~%  ~A" name err)
      nil)
    (invalid-sem-spec (err) 
      (krontology-warn "invalid semantic specification for ~S:~%  ~A" name err)
      nil)
    )
  )

;; A wrapper for error control
(defun add-frame-to-predicate-transforms (name krontology lfontology &key preds sem)
  "Adds frame to predicate mappings with a given base name. "
  (handler-case (add-transform-entries name krontology lfontology preds nil t nil sem)
    (invalid-mapping-entry (err) 
      (krontology-warn "invalid class specification for ~S:~%  ~A" name err)
      nil)
    (invalid-sem-spec (err) 
      (krontology-warn "invalid semantic specification for ~S:~%  ~A" name err)
      nil)
    )
  )

;; A wrapper for error control
(defun add-slot-transforms (name krontology lfontology &key preds sem)
  "Adds frame to predicate mappings with a given base name. "
  (handler-case (add-transform-entries name krontology lfontology preds nil nil nil sem)
    (invalid-mapping-entry (err) 
      (krontology-warn "invalid class specification for ~S:~%  ~A" name err)
      nil)
    (invalid-sem-spec (err) 
      (krontology-warn "invalid semantic specification for ~S:~%  ~A" name err)
      nil)
    )
  )

;; A wrapper for error control
(defun add-predicate-to-predicate-transforms (name krontology lfontology &key preds sem)
  "Adds frame to predicate mappings with a given base name. "
  (handler-case (add-transform-entries name krontology lfontology preds nil nil nil sem)
    (invalid-mapping-entry (err) 
      (krontology-warn "invalid class specification for ~S:~%  ~A" name err)
      nil)
    (invalid-sem-spec (err) 
      (krontology-warn "invalid semantic specification for ~S:~%  ~A" name err)
      nil)
    )
  )


(defun add-transform-entries (name krontology lfontology preds default from-frame to-frame sem &optional arguments parent abstract)
  "Adds predicate transforms with a given base name to the table"
  (let ((predmaps (parse-transform-pattern-list preds))
	(argmaps (parse-transform-argument-list arguments))
	(sm (make-typed-sem sem))	
	)

    ;; We have to add the original transform, because its name will be
    ;; specified by all transforms that consider it a parent. It'll be
    ;; used to get the proper arguments etc for the children transforms
    (add-transform 
     (make-lf-kr-transform
      :parent parent
      :name name
      :lf nil
      :arguments argmaps
      :pattern preds
      :default default
      :from-frame from-frame
      :to-frame to-frame
      :abstract abstract
      :sem sm
      )
     (kr-ontology-map-table krontology)
     lfontology
     )
    (mapc (lambda (x) (integrate-transform 
		       (make-lf-kr-transform
			:parent parent
			:name (full-transform-name name (transform-pattern-lf x))
			:lf (transform-pattern-lf x)
			:arguments argmaps
			:pattern x
			:default default
			:from-frame from-frame
			:to-frame to-frame
			:abstract abstract
			:sem sm
			)
		       krontology lfontology
		       ))
	  predmaps
	  )
    ))

(defun integrate-transform (map krontology lfontology)
  (if (lf-kr-transform-to-frame map)
      (integrate-to-frame-transform map krontology lfontology)
    (integrate-to-predicate-transform map krontology lfontology)
    ))

(defun integrate-to-frame-transform (map krontology lfontology)
  "adds the predicate mapping to the mapping table and links to it in corresponding lf predicate, also checks that all arguments are correct"
  (let* ((name (lf-kr-transform-name map))
	 (lf (lf-kr-transform-lf map))
	 (pattern (lf-kr-transform-pattern map))
	 (class (get-pattern-class-name pattern))
	 (lfdescr (get-lf-definition lf lfontology))
	 (classdescr (gethash class (kr-ontology-class-table krontology)))
	 )
    ;; verify the pattern
    (cond
     ((null lfdescr)
      (krontology-warn
       "Invalid LF name ~S in mapping ~S - not added to the table" 
       lf name))
     ((and class (null classdescr))
      (krontology-warn
       "Invalid class name ~S in mapping ~S - not added to the table" 
       class name))
     ;; sanity check - for default transforms, the class should be a constant!
     ((and (lf-kr-transform-default map) (not (eql class (first (transform-pattern-pattern pattern)))))
      (error 
       (make-system-condition 'invalid-mapping-entry
			      :format-control "Bad default transform ~S. The class in the pattern should be a constant in default transforms. Not added to the table"
			      :format-arguments (list map)
			      ))
      )	   								   
     (t
      (handler-case (mapc (lambda (x) (check-frame-argument-pattern x lfdescr classdescr krontology lfontology))
			  (lf-kr-transform-arguments map))
	(invalid-argument-slot-mapping (err) 
	  (if *verbose-mapping-warnings* (krontology-warn "invalid argument-slot-mapping for mapping ~S:~%  ~A" name err))
	  nil)
	)
      
      (add-transform map
		     (kr-ontology-map-table krontology)			 
		     lfontology
		     )
      (if (lf-kr-transform-default map)
	  (pushnew (list lf name) (lf-description-default-kr-transforms lfdescr))
	(pushnew (list lf name) (lf-description-kr-transforms lfdescr))
	) 
      ;;(print classdescr)
      (pushnew name (basic-class-description-lf-transforms classdescr))
      map
      ))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Handling the frame-to-predicate transforms
;;

(defun integrate-to-predicate-transform (map krontology lfontology)
  "adds the predicate mapping to the mapping table and links to it in corresponding lf predicate, also checks that all arguments are correct"
  (let* ((name (lf-kr-transform-name map))
	 (lf (lf-kr-transform-lf map))
	 (pattern (lf-kr-transform-pattern map))
	 (class (get-pattern-class-name pattern))
	 (lfdescr (get-lf-definition lf lfontology))
	 (classdescr (gethash class (kr-ontology-predicate-table krontology)))
	 )
    ;; verify the pattern
    (cond
     ((null lfdescr)
      (krontology-warn
       "Invalid LF name ~S in mapping ~S - not added to the table" 
       lf name))
     ((and class (null classdescr))
      (krontology-warn
       "Invalid predicate name ~S in to-predicate mapping ~S - not added to the table" 
       class name))
     ;; sanity check - for default transforms, the class should be a constant!
     ((and (lf-kr-transform-default map) (not (eql class (first (transform-pattern-pattern pattern)))))
      (error 
       (make-system-condition 'invalid-mapping-entry
			      :format-control "Bad default transform ~S. The class in the pattern should be a constant in default transforms. Not added to the table"
			      :format-arguments (list map)
			      ))
      )	   			
     ((and (lf-kr-transform-from-frame map) (not (consp (transform-pattern-pattern pattern))))
      ;; if we are transforming from a frame, no default arguments allowed!!!
      (error
       (make-system-condition 'invalid-mapping-entry
			      :format-constrol "Bad frame-to-predicate transform ~S. The pattern has to be a list using role names. Not added to the table"
			      :format-arguments (list map)
			      ))
      )     
     (t
      (setf (transform-pattern-pattern pattern)
	(make-default-list-pattern (transform-pattern-pattern pattern))) 
      
      ;; Another sanity check. Now that we have a pattern, it should
      ;; have as many args as the class definition
      (when (not (eql (length (cdr (transform-pattern-pattern pattern)))
		      (length (predicate-description-arguments classdescr))
		      ))
	(error 
	 (make-system-condition 'invalid-mapping-entry
				:format-control "Invalid mapping ~S: different number of arguments in the pattern and resulting predicate"
				:format-arguments (list map)
				))
	)				    
      ;; For now, no checking patterns themselves - anything that is
      ;; not roles will come out as simple constants      
      (add-transform map
		     (kr-ontology-map-table krontology)			 
		     lfontology
		     )
      (if (lf-kr-transform-default map)
	  (pushnew (list lf name) (lf-description-default-kr-transforms lfdescr))
	(pushnew (list lf name) (lf-description-kr-transforms lfdescr))
	) 
      (pushnew name (basic-class-description-lf-transforms classdescr))
      map
      ))
    ))


;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!1 STOPPED HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; ToDo: figure out what to do about default lf-form substitutions as specified in the patterns
;; add argval into operator checks
 
(defun check-frame-argument-pattern (argpat lfdescr classdescr krontology lfontology)
  "Verifies that an argument mapping is consistent with lf and class description"
  (when (not (find-lf-argument (transform-pattern-lf argpat) (lf-description-arguments lfdescr) lfontology :skey #'sem-argument-role))
    (error 
     (make-system-condition 'invalid-argument-slot-mapping
			    :format-control "Unknown LF argument name ~S"
			    :format-arguments (list (transform-pattern-lf argpat)))
     ))
  ;; we know that the map has a lf argument associated with it
  ;; now let's see if it maps to something that exists
  (let ((slotargs (get-pattern-argument-types (transform-pattern-lf argpat) (transform-pattern-pattern argpat) classdescr krontology))
	(patargs (rest (transform-pattern-pattern argpat)))
	)
    (cond 
     ((null slotargs)
      (error 
       (make-system-condition 'invalid-argument-slot-mapping
			      :format-control "Unknown slot or predicate name ~S"
			      :format-arguments (list (get-pattern-class-name argpat))
			      ))
      )
     ((not (eql (length slotargs) (length patargs)))
      (error 
       (make-system-condition 'invalid-argument-slot-mapping
			      :format-control "The number of arguments in the pattern ~S and in the slot/predicate ~S it maps to is different"
			      :format-arguments (list argpat (get-pattern-class-name argpat))
			      ))
      )
     ;; now slot arguments exist in the same number as pattern arguments, and we can check all actual transform arguments againse them
     (t (let ((res (mapcar (lambda (arg argtype)
			     (check-pattern-expression arg argtype
						       krontology
						       :argument-type (class-description-name classdescr)
						       :lf-form-default (transform-pattern-lf-form-default argpat)
						       ))
			   patargs
			   slotargs)))
	  
	  (some #'null res)
	  )
	))
   ))

(defun check-pattern-expression (expr expected-type krontology &key argument-type lf-form-default rolelist)
  "Given an expression, verifies that it is a correct operator or symbol, and that its type matches the expected type"
  (and
   (check-argument expr krontology :lfform lf-form-default :rolelist rolelist :argval argument-type)
   (subclass (evaluate-pattern-element expr :lfform lf-form-default :rolelist rolelist :argval argument-type)
	     expected-type krontology))
  )
				 
				 

(defun add-transform (transform table lfontology)
  "Adds a transform to the table, merging it with parent values and performing other processing as needed"
  (let* ((parent (lf-kr-transform-parent transform))
	 (parentdef (and parent (gethash parent table)))
	 )
    (when parent
      (when (null parentdef)
	(Error 
	 (make-system-condition 'invalid-mapping-entry
				:format-control "Invalid parent parent name ~S in mapping ~S" 
				:format-arguments (list parent (lf-kr-transform-name transform)))))
      (setf (lf-kr-transform-arguments transform) 
	(merge-in-defaults (lf-kr-transform-arguments transform) 
			   (lf-kr-transform-arguments parentdef) :key #'transform-pattern-lf :test (lambda (x y) (lex-sublf x y lfontology)))
	)
      (setf (lf-kr-transform-sem transform)
	(merge-typed-feature-lists (lf-kr-transform-sem transform) (lf-kr-transform-sem parentdef) lfontology))
      )    
    (sethash (lf-kr-transform-name transform) transform table)
    ))


(defun parse-transform-argument-list (args)
  (mapcar #'parse-transform-argument args)
  )


(defun parse-transform-pattern-list (namemaps)
  (mapcar #'parse-transform-pattern namemaps)
  )

(defun parse-transform-pattern (pat) 
  (make-transform-pattern :lf (convert-star-to-operator-form (first pat) ':*) 
			  :lf-form-default (second (assoc :lf-form-default (cddr pat))) :pattern (parse-conversion-pattern (second pat)))
  )

(defun parse-transform-argument (pat)  
  (make-transform-pattern 
   :lf (convert-star-to-operator-form (first pat) ':*) 
   :pattern (make-default-list-pattern (second pat))
   :oblig (second (assoc :oblig (cddr pat)))
   :coerce (cdr (assoc :coerce (cddr pat)))
   :lf-form-default (second (assoc :lf-form-default (cddr pat)))
))   

(defun full-transform-name (rulename lfname)
  "A helper to make a full map name in the format rulename-lfname-classname"
  (right-var '- rulename (if (consp lfname) (right-var (second lfname) (first lfname) (third lfname))  lfname))
  )

(defun parse-conversion-pattern (pat)
  "Parses a conversion pattern which is a list that may contain constants, special words or references to roles denoted as * in the symbol, which will then be stripped off and converted into (:role n) as a nicer inner representation"
  (cond 
   ((null pat) nil)
   ((listp pat) (mapcar #'parse-conversion-pattern pat))
   (t (parse-conversion-pattern-element pat))
  ))

(defun parse-conversion-pattern-element (pel)
  "converts a pattern element into internal representation. Currently only replaces * names with (:role name)"
  (let ((pname (format nil "~S" pel)))
    (cond
     ((equal (char pname 0) #\*)
      (list :role (read-from-string (subseq pname 1)))
      )
     ((equal (char pname (- (length pname) 1)) #\*)
      (list :role (read-from-string (subseq pname 0 (- (length pname) 1)))))
     (t pel))
    ))
    
(defun make-default-list-pattern (pat) 
  "If the passed in value is a symbol, converts it to a default pattern form. Currently
   (sym :arg-var :subcta-var) "
  (if (listp pat)
      pat
    (list pat :arg-var :subcat-var)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Common functions
;;


(defun get-pattern-class-name (map)
  "Revovers a class name for pattern checking. Uses the first value only. If it is a constant, returns it. If it is LF-FORM, returns KR-ROOT. If it is
PRED-VAR or ARG-VAR, raises an error."
  (cond
   ((listp map) nil)
   (t
    (let* ((pat (transform-pattern-pattern map))
	   (class (if (listp pat) (first pat) pat))
	   (pform (transform-pattern-lf-form-default map))
	   )
      ;; (print pat)
      ;;(print class)
      (cond
       ((listp class)
	;; there's some operation applied here - the result should be under the given pform
	pform)
       ((member class '(:SUBCAT-VAR :ARG-VAR))
	(Error 
	 (make-system-condition 'invalid-mapping-entry
				:format-control "Disallowed value  ~S in the first position in the pattern for lf ~S"
				:format-arguments (list (transform-pattern-lf map) class)))
	)
       (t
	(evaluate-pattern-element  class :lfform pform)
	)
       )
      ))
   ))
   

;; A helper to get the real class name based on the lf and the pattern
;; substitutes :lf-form variable for the actual value taken from lf
(defun lf-kr-transform-class (lf mapdef krontology)
  "Determines the sense class name given the actual lf or lf-form and the pattern. Mapdef is lf-kr-transform structure"
  (when mapdef
    (let* ((pat (lf-kr-transform-pattern mapdef))
	   (res (transform-pattern-valid-class lf pat
					       (get-transform-class-table mapdef krontology)
					       )))
      (when (and (not (null (transform-pattern-lf-form-default pat)))
		 (not (subclass res (transform-pattern-lf-form-default pat) krontology))
		 )
	(when *trace-transform-process*
	  (ontology-warn "Warning: LF ~S is not consistent with transform ~S ~%: resulting class ~S is in a different part of class hierarchy. The transform won't be applied" lf mapdef res))
	(setq res nil)
	)
      res
      )))

(defun transform-pattern-class (lf pat &key (lfform nil))
  "Finds a class of a transform pattern"
  (expression-class lf (transform-pattern-pattern pat) :lfform lfform)
  )

(defun transform-pattern-valid-class (lf pat ctable &key (lfform nil))
  "Finds a class of a transform pattern verifying its validity in the class table. DOES NOT CHECK THE SUBTYPE CONSISTENCY WITH DEFAULT LF"
  (let* ((pform (transform-pattern-lf-form-default pat))
	 (cname (transform-pattern-class lf pat :lfform lfform))	 
	 (cdef (gethash cname ctable))
	 )
    ;; when the real form does not exist, fall back on default
    (when (null cdef)
      (setq cname (or pform cname))
      (setq cdef (gethash cname ctable))
      )
    ;; this will return cname only if cdef is valid
    (and cdef cname)
    ))


(defun expression-class (lf expr &key (lfform nil))
  (evaluate-pattern-element
   (if (listp expr) (first expr) expr)
   :lf lf :lfform lfform
   )
  )

(defun get-transform-class-table (mapdef krontology)
  "Given a tansform, determines the table into which it maps - either the class table or the predicate table from the krontology"
  (if (lf-kr-transform-to-frame mapdef) (kr-ontology-class-table krontology) (kr-ontology-predicate-table krontology))
  )

(defun evaluate-pattern-expression (pat krontology &key lf lfform argval subcatval rolelist)
  "Evaluates a pattern expression by first getting the valid class for the pattern, and then evaluating all the arguments if the pattern is a list. Applicable to patterns that map to predicates only."
  (let ((expr (transform-pattern-pattern pat))
	(ptable (kr-ontology-predicate-table krontology))
	)
    (cons (transform-pattern-valid-class lf pat ptable :lfform lfform)
	  (mapcar (lambda (x) (evaluate-pattern-element 
			       x :lf lf :lfform lfform 
			       :argval argval :subcatval subcatval
			       :rolelist rolelist))
		  (cdr expr))
	  )
    ))

;; A helper to get the real class name based on the lf and the pattern
;; substitutes :lf-form variable for the actual alue taken from lf
(defun evaluate-pattern-element (el &key lf lfform argval subcatval rolelist operator-table)
  "Evaluates a pattern element in the context of a given var . Constants evaluate to themselves.
   :lf-form is replaced with either the lf-form defined by LF, or explicitly specified :lfform, :arg-var evaluates to argval,
   and :subcat-var evaluates to subcatval. If rolelist in the form (role value)* is given, 
   expressions (:role role) evaluate to corresponding values in rolelist.
   We can pass in 2 things as lfform : either a value as a single atom, 
    or a (:lf-form-default value) expression where we return the value if LF-FORM is requested"
  (cond
   ((null el) nil)   
   ((listp el)
    (evaluate-pattern-operation 
     (first el)  
     (mapcar (lambda (x) (evaluate-pattern-element x :lf lf :lfform lfform :argval argval :subcatval subcatval :operator-table operator-table))
	     (rest el))     
     :rolelist rolelist
     :operator-table operator-table
     ))
   ((eql el ':LF-FORM) 
    (cond
     (lf 
      (or (get-lf-form lf ':* :package :kr) lfform)
      )
     (t lfform)
     ))
   ((eql el ':ARG-VAR)
    argval)
   ((eql el ':SUBCAT-VAR)
    subcatval)
   (t el)
   ))

(defun evaluate-pattern-operation (op args &key rolelist operator-table)
  "expressions (:role role) evaluate to corresponding values in rolelist, expressions :concat args evaluate to concatenation of all arguments. Operators evaluate to their result types"
  (cond
   ((eql op :concat)
    (reduce (lambda (x y) (right-var x nil y)) args)
    )
   ((eql op :role)
    (second (assoc (first args) rolelist))) 
   ;; at this point we assume this is an operator, and if there's no
   ;; operator table, we simply return nil
   ((null operator-table) (cons op args))
   ((gethash op operator-table) ;; assume we got an operator, simply return its return type if it is defined
    (operator-description-result (gethash op operator-table))
    )
   (t nil)
   ))




(defun check-argument (expr krontology &key lf lfform rolelist argval)
  "Takes an argument in an expression, which can only be a type value from a class definition or an operator and checks it"
  (cond
   ((listp expr) (check-operator expr krontology :lf lf :lfform lfform :rolelist rolelist :argval argval))
   (t (gethash expr (kr-ontology-class-table krontology)))
   ))

;; Currently no checking for role name references!!!
(defun check-operator (expr krontology &key lf lfform rolelist argval)
  "Takes an expression which is an operator and verifies its validity. Checks that all the types match. Throws exceptions on errors. Returns t if check was completed, and nil if it was a special operator that is not checked"
  (let (
	(op (evaluate-pattern-element (first expr) :lf lf :lfform lfform))
	(args (rest expr))
	(optable (kr-ontology-operator-table krontology))
	)
    (cond
     ((special-operator op) ;; special operator
      (mapcar (lambda (arg) (check-argument arg krontology :lf lf :lfform lfform :rolelist rolelist :argval argval)) args)
      nil ;; always nil on special operators to indicate that they were present
      )     
     ((null (gethash op optable)) ;; operator not defined
      (error 
       (make-system-condition 'invalid-expression
			      :format-control "Bad expression ~S. Unknown operator ~S:" 
			      :format-arguments (list expr op)
			      ))
      )
     (t ;; known operator. For checking, we first check all arguments. If they all are valid, we evaluate them (to produce types) and verify that the result matches the definition
      (mapcar (lambda (x) (check-argument x krontology :lf lf :lfform lfform 
					  :rolelist rolelist :argval argval)) args)
      (let* ((opdef (gethash op optable))
	     (argtypes (mapcar (lambda (x) (evaluate-pattern-element x :lf lf :lfform lfform
								     :rolelist rolelist 
								     :argval argval)) 
			       args))
	     (deftypes (operator-description-arguments opdef))	     	     
	     )
	(dolist (argtype argtypes) 
	  (when (not (subclass argtype (pop deftypes) krontology))
	    (let ((ind (position argtype argtypes)))
	      (error 
	       (make-system-condition 'invalid-expression
				      :format-control "Bad argument ~S in expression ~S. Argument type ~S does not match declared type ~S."
				      :format-arguments (list (nth ind args) argtype expr (nth ind (operator-description-arguments opdef)))
				      ))
	      )))
	))
     )
    ))

(defun special-operator (op)
  "Special operators not checked during the regular verification process which looks up the operator definitions"
  (member op '(:concat :role))
  )


(defun get-pattern-argument-types (role pattern cdef krontology)
  ;; First we check for one special condition - a :subcat-var on top
  ;; of the role, which means it is bound by role definition
  (let* ((slotname (expression-class role pattern))
	 (slotdef (assoc slotname (class-description-slots cdef)))
	 )
    (cond
     (slotdef ;; if there's a slot definition, this is equiv. to a pred with 2 arguments, one the type of class, another the type of slot
      (list (class-description-name cdef) (second slotdef))
      )
     ((gethash slotname (kr-ontology-predicate-table krontology))
      (predicate-description-arguments (gethash slotname (kr-ontology-predicate-table krontology)))
      )
     (t nil)
     )
    ))

