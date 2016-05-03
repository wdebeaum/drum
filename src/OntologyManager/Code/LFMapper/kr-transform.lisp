;; Nate Chambers
;; IHMC
;;
;; This file contains the functions that do the KR to LF conversion
;;

(in-package "ONTOLOGYMANAGER")

(declaim (special *kr-atom-map*))
(declaim (special *dynamic-transforms*))

(defvar *ANY-VAR* 'V1ANY)

(defun kr-atom-to-lf (kr transforms)
  "@param kr A kr class, an atom.
   @returns A list of LF classes and/or triples
   @desc The function finds all possible transforms that produce this kr.
         It first tries to return one that has no obligatory constraints
         and no null argtransform expansions.  If there are none, it
         tries transforms with no obligatories.  If that fails, the
         transforms with obligatory args are then returned."

  (let ((quickie (assoc kr *kr-atom-map*))
	options)
    (cond
     ;; check the 'hack list' for a quick conversion
     (quickie (second quickie))
     ;; otherwise check each lf to kr rule, looking for the matching kr
     ((setq options (sort-by-relevance
		     (get-krtype-rules kr transforms :abstracts nil)))
      (debug-om 40 "kr-atom-to-lf options=~S~%" options)
      (mapcar #'(lambda (x) (reconstruct-lftype-given-kr x kr))
	      options))
     ;; last resort, try hard-coded mappings
      (t
  ;    (setq options (reverse-map kr))
  ;    (when options ;; create an ONT::ROOT object for lexical items
;	(if (equal "W" (get-package options))
;	    (list (list :* 'ont::root options))
	;  nil)))
      (list kr)) ;; use ont type
      )))

(defun kr-atom-default-to-lf (kr transforms)
  "@param kr A kr class, an atom.
   @returns A list of LF classes and/or triples
   @desc The function finds all possible transforms that produce this kr
         through a default construction."
  (let (options)
    (cond
     ;; otherwise check each lf to kr rule, looking for the matching kr
     ((setq options (sort-by-relevance
		     (get-krtype-defaults kr transforms :abstracts nil)))
      (debug-om 40 "kr-atom-to-lf options=~S~%" options)
      (mapcar #'(lambda (x) (reconstruct-lftype-given-kr x kr))
	      options))
     ;; last resort, try hard-coded mappings
     (t nil))))

;; create a transform on the fly when needed to support abstract transform matching for roles
(defun create-transform-from-lftype (lftype)
  (let* (
	 (this-typetransform (make-typetransform
			      :lftype lftype
			      :kr lftype
			      ))
	 (this-lftransform (make-lftransform
			    :name    'this-transform
			    :typevar '?vv
			    :typetransform this-typetransform
			    )))
    this-lftransform)	 
  )

(defun kr-role-to-lf (role value type transforms)
  "@param role The kr role to transform
   @param type The value of the role
   @param type The kr instance type that has the role
   @desc Returns a list of pairs: (LF roles, LF instance)
         -- LF instance could be (:* lf lex) or just lf"
  (debug-om 15 "kr-role-to-lf role=~S value=~S type=~S~%" role value type)
  (let ((options (append (rules-with-krtype-and-role type role transforms)
			 (rules-with-krtype-and-role '? role transforms)))
	rel arg returns parents final lfs krval binds functions aug-transforms)
    
    ;; find the first transform that has the kr role
    (dolist (therule options)
      (setq returns (kr-role-to-lfroles role therule))

      ;; get the kr argument from the rule
      (setq arg (get-argtransform-with-kr role (lftransform-argtransforms therule)))
      (setq krval (third (argtransform-kr arg)))
      (setq binds (simplekb::unify krval value))

      (debug-om 15 "arg: ~S krval: ~S binds: ~S~%" arg krval binds)
      ;; the values match up
      (when binds
	;; get functions we need to reverse call
	(setq functions (lftransform-functions therule))
	(setq functions (remove-nils (mapcar #'(lambda (x) (assoc (car x) functions))
					     binds)))
	;; reverse the functions and bind their argument variables
	(dolist (func functions) ;; func = (?v (func ?y ?z))
	  (let* ((nils (make-list (length (cddr (second func)))))
		 (rev (apply (first (second func))
			     (simplekb::lookup (first func) binds)
			     (append nils '(:reverse t)))))
	    (mapcar #'(lambda (a b) (setq binds (cons (cons a b) binds)))
		    (rest (second func)) rev)
	    ))
      
	;; replace the root variable with static ?*top* variable
	(setq binds (cons (cons (lftransform-typevar therule) '?*top*) binds))
	;; substitute these static bindings
	(setq returns (simplekb::subst-bindings binds returns))

	(setq final (cond
	     ;; matched an abstract rule, then we must find a main rule too
	     ((lftransform-abstract therule)
	      ;; If there's a real rule that is the parent of this one, we're good
	      (setq aug-transforms (append transforms *dynamic-transforms*))
	      ;; The real rule must have a krtype of the given type
	      (setq parents (or (get-krtype-rules type aug-transforms :abstracts nil) (list (create-transform-from-lftype type))))
	      (loop while parents do
		    (setq rel (most-relevant-transform parents))
		    
		    ;; rel must be a subtype of the matched abstract rule
		    (if (subtype (typetransform-lftype (lftransform-typetransform rel))
				 (typetransform-lftype (lftransform-typetransform therule)))
			;; return the roles and the lf type
			(return (list returns
				      (reconstruct-lftype (lftransform-typetransform rel) binds)))
		      ;; else remove the transform and try again
		      (setq parents
			    (remove-transform (lftransform-name rel) parents)))
		    ))
	     ;; else the rule is a main rule (not abstract)
	     (t (list returns
		      (reconstruct-lftype (lftransform-typetransform therule) binds))
		)))
	
	(if final (setq lfs (cons final lfs)))))
    lfs
    ))


(defun kr-role-default-to-lf (role value type transforms)
  "@param role The kr role to transform
   @param type The value of the role
   @param type The kr instance type that has the role
   @desc Returns a list of pairs: (LF roles, LF instance)
         -- LF instance could be (:* lf lex) or just lf"
  (debug-om 15 "kr-role-default-to-lf role=~S value=~S type=~S~%" role value type)
  (let ((options (append (default-rules-with-krtype-and-role type role transforms)
			 (default-rules-with-krtype-and-role '? role transforms)))
	rel arg returns parents final lfs krval binds functions)

    ;; find the first transform that has the kr role
    (dolist (therule options)
      (setq returns (kr-role-to-lfroles role therule))
      ;; ****NOTE: if returns is nil, it still matched the role, but there was no
      ;;           LF role on the LHS.
      
      ;; get the kr argument from the rule
      (setq arg (get-argtransform-with-kr role (lftransform-argtransforms
						therule)))
      (setq krval (third (argtransform-kr arg)))
      (setq binds (simplekb::unify krval value))

      ;; the values match up
      (when binds
	;; get functions we need to reverse call
	(setq functions (lftransform-functions therule))
	(setq functions (remove-nils (mapcar #'(lambda (x) (assoc (car x) functions))
					     binds)))
	;; reverse the functions and bind their argument variables
	(dolist (func functions) ;; func = (?v (func ?y ?z))
	  (let* ((nils (make-list (length (cddr (second func)))))
		 (rev (apply (first (second func))
			     (simplekb::lookup (first func) binds)
			     (append nils '(:reverse t)))))
	    (mapcar #'(lambda (a b) (setq binds (cons (cons a b) binds)))
		    (rest (second func)) rev)
	    ))
      
	;; replace the root variable with static ?*top* variable
	(setq binds (cons (cons (lftransform-typevar therule) '?*top*) binds))
	;; substitute these static bindings
	(setq returns (simplekb::subst-bindings binds returns))

	(setq final (cond
	     ;; matched an abstract rule, then we must find a main rule too
	     ((lftransform-abstract therule)
	      ;; If there's a real rule that is the parent of this one, we're good
	      ;; The real rule must have a krtype of the given type
	      (setq parents (get-krtype-rules type transforms :abstracts t))
	      (loop while parents do
		    (setq rel (most-relevant-transform parents))
		    
		    ;; rel must be a subtype of the matched abstract rule
		    (if (subtype (typetransform-lftype (lftransform-typetransform rel))
				 (typetransform-lftype (lftransform-typetransform therule)))
			;; return the roles and the lf type
			(return (list returns
				      (reconstruct-lftype (lftransform-typetransform rel) binds)))
		      ;; else remove the transform and try again
		      (setq parents
			    (remove-transform (lftransform-name rel) parents)))
		    ))
	     ;; else the rule is a main rule (not abstract)
	     (t (list returns
		      (reconstruct-lftype (lftransform-typetransform therule) binds))
		)))

	(if final (setq lfs (cons final lfs)))))
    lfs
    ))


(defun kr-role-to-lfroles (role transform)
  "@param role A kr role name
   @param transform A single transform rule
   @desc Returns a list of the LF roles that created the given KR role.  Takes
         into account all :obligatory constraints as well."
  (let ((args (lftransform-argtransforms transform))
	(constraints (lftransform-constraints transform))
	arg obligs returns)

    (setq arg (get-argtransform-with-kr role args))

    ;; if the :allobligatory constraint is present, concat all the args
    (if (contains-allobligatory transform)
	(setq returns (mapcan #'(lambda (x) (append (argtransform-content x)
						    (add-context-flag (argtransform-context x))))
			      args))
      ;; else use the LF role(s) that directly creates the kr role
      (setq returns (append (argtransform-content arg) (add-context-flag (argtransform-context arg)))))

    ;; else if there are constraints, append the required conditions      
    (when constraints
      (setq obligs (get-obligatories transform))
      (dolist (oblig obligs)
	;; don't add an obligatory if that role was already given
	(if (not (member oblig returns :test #'(lambda (x y) (eq x (car y)))))
	    (setq returns (cons (list oblig (read-from-string (format nil "?~A" oblig)))
				returns))))
      )

    returns))

(defun kr-role-value-to-lf (role value transforms)
  "@param role A single kr role
   @param value The kr value of the role
   @returns The LF type corresponding to the reified role.  Note that this
            is specific to the AKRL ROLE-VALUE construction and not a general
            role transform."
  (let (forms results)
    ;; get all rules that create a ONT::ROLE-VALUE
    (dolist (transform transforms)
      ;; if the rule uses the value as its KR production
      (if (eq value (typetransform-kr (lftransform-typetransform transform)))
	  ;; check all defaults for a ROLE-VALUE construction
	  (dolist (default (lftransform-defaults transform))
	    (if (eq 'ont::role-value (second default))
		(setq forms (cons transform forms))))))

    ;; check each transform for a ont::role argument and the matched role
    (dolist (transform forms)
      (dolist (arg (lftransform-argtransforms transform))
	(if (and (eq 'ont::role (car (argtransform-kr arg)))
		 (eq role (third (argtransform-kr arg))))
	    (let* ((type (lftransform-typetransform transform))
		   (lf (typetransform-lftype type))
		   (lex (typetransform-lflex type)))
	      (setq results (cons (if lex (list :* lf lex) lf)
				  results))))))
    results))

(defun resolve-krtype (transform kr)
  "@param transform An LF transform
   @param kr The KR type we are reversing...for function calls
   @returns The KR type from this function.  If the KR is a variable, then
            we reverse the function that created it, return the KR if it
            returned an LF when reversed."
  (debug-om 60 "resolving transform ~S for kr type ~S~%" transform kr)
  (let ((type (typetransform-kr (lftransform-typetransform transform)))
	(lf (typetransform-lftype (lftransform-typetransform transform)))
	func)
    (cond ((and (is-transform-var type) (not (eq '? type)))
	   (setq func (second (assoc type (lftransform-functions transform))))
	   (when func
	     (setq func (apply (car func) kr '(:reverse t)))
	     ;; if we got a result from reversing the function
	     (when (car func)
	       ;; check that the lex is valid with the LF
	       (if (check-lex-validity lf (car func)) 
		   (return-from resolve-krtype kr))))
	   nil)
	  (t type))))


(defun get-krtype-rules (kr transforms &key (abstracts nil))
  "@desc Returns all rules that have kr as its main type conversion.
         By default, it doesn't return abstract rules."
  (when transforms
    (if (and (or abstracts (not (lftransform-abstract (first transforms))))
;	     (eq kr (typetransform-kr (lftransform-typetransform (first transforms)))))
	     (eq kr (resolve-krtype (first transforms) kr)))
	(cons (first transforms)
	      (get-krtype-rules kr (rest transforms) :abstracts abstracts))
      (get-krtype-rules kr (rest transforms) :abstracts abstracts))))

(defun rules-with-krtype-and-role (type role transforms)
  "@desc Returns all rules that have kr as its main type conversion and the
         given role in its list of roles."
  (let ((options (get-krtype-rules type transforms :abstracts t))
	mainvar args remains)
    (debug-om 40 "rules-with-krtype-and-role type=~S role=~S~%" type role)
    ;; check each rule and keep the ones that have the role
    (dolist (option options)
      (setq args (lftransform-argtransforms option))
      (setq mainvar (lftransform-typevar option))
      (dolist (arg args)
	;; keep if we found the role
	(when (and (eq role (first (argtransform-kr arg)))
		   (eq mainvar (second (argtransform-kr arg))))
	  (setq remains (append remains (list option)))
	  (return))))

    (debug-om 40 "rules: ~S~%" (mapcar #'(lambda (x) (lftransform-name x)) remains))
    remains))

(defun get-krtype-defaults (kr transforms &key (abstracts nil))
  "@desc Returns all rules that have kr as a default construction.
         By default, it doesn't return abstract rules."
  (when transforms
    (if (and (or abstracts (not (lftransform-abstract (first transforms))))
;	     (eq kr (typetransform-kr (lftransform-typetransform (first transforms)))))
	     (member kr (lftransform-defaults (first transforms)) :key #'second))
	(cons (first transforms)
	      (get-krtype-defaults kr (rest transforms) :abstracts abstracts))
      (get-krtype-defaults kr (rest transforms) :abstracts abstracts))))

(defun default-rules-with-krtype-and-role (type role transforms)
  "@desc Returns all rules that have kr as its main type conversion and the
         given role in its list of roles."
  (let ((options (get-krtype-defaults type transforms :abstracts t))
	mainvar args remains)
;    (debug-om 40 "rules with kr: ~S~%" options)
    (debug-om 40 "default-rules-with-krtype-and-role type=~S role=~S~%" type role)
    ;; check each rule and keep the ones that have the role
    (dolist (option options)
      (setq args (lftransform-argtransforms option))
      (setq mainvar (caar (member type (lftransform-defaults option) :key #'second)))
      (dolist (arg args)
	;; keep if we found the role
	(when (and (eq role (first (argtransform-kr arg)))
		   (eq mainvar (second (argtransform-kr arg))))
	  (setq remains (append remains (list option)))
	  (return))))
    
    remains))


(defun get-argtransform-with-kr (kr argtransforms)
  (dolist (arg argtransforms)
    (if (eq kr (first (argtransform-kr arg))) (return arg))))

(defun contains-allobligatory (transform)
  (let ((constraints (lftransform-constraints transform)))
    (dolist (constraint constraints nil)
      (if (eq (first constraint) :allobligatory) (return t)))))

(defun contains-null-args (transform)
  "@desc Returns true if there are null argtransforms  (nil -> (pred ?x ?v))"
  (let ((args (lftransform-argtransforms transform)))
    (dolist (arg args nil)
      (if (and (null (argtransform-content arg))
	       (null (argtransform-context arg))) (return t)))))

(defun sort-by-relevance (transforms)
  "@desc Sorts the transforms in order of relevance.  Abstract rules are last,
         normal rules are first ... rules with constraints are pushed back...
         This function is not destructive, but is time intensive!"
  (let ((forms (copy-tree transforms)))
    (sort forms #'(lambda (x y)
		    (let ((yobligs (or (contains-allobligatory y)
				       (get-obligatories y)))
			  (ynulls (contains-null-args y))
			  )
		    (cond
		     ;; x is abstract
		     ((lftransform-abstract x)
		      (if (lftransform-abstract y)
			  (rule-more-specific x y)
			nil)
		      )
		     ;; x is not abstract
		     (t
		      (cond ((lftransform-abstract y)
			     t)
			    ((or (contains-allobligatory x)
				 (get-obligatories x))
			     (if yobligs
				 (rule-more-specific x y)
			       nil))
			    ((contains-null-args x)
			     (if ynulls
				 (rule-more-specific x y)
			       nil))
			    (t (if (or yobligs ynulls)
				   t
				 (rule-more-specific x y)))))))))
    ))

(defun most-relevant-transform (transforms)
  "@desc Returns the one transform in the given list that is the most
         relevant.  Non-abstract rules, without obligatory constraints always
         win.  Abstract rules are last."
  (let (obligs extras bases abstracts)
    ;; split possible transforms according to obligatories and args
    (dolist (option transforms)
      (cond ((lftransform-abstract option)
	     (setq abstracts (cons option abstracts)))
	    ((or (contains-allobligatory option) (get-obligatories option))
	     (setq obligs (cons option obligs)))
	    ((contains-null-args option)
	     (setq extras (cons option extras)))
	    (t (setq bases (cons option bases)))))
    
    ;; get the best transform
    (cond (bases (most-specific-transform bases))
	  (extras (most-specific-transform extras))
	  (obligs (most-specific-transform obligs))
	  (abstracts (most-specific-transform abstracts))
	  (t nil))))

(defun most-specific-transform (transforms)
  "@param transforms A list of lftransform structures
   @desc Returns the most specific transform in the list"
  (let ((best (first transforms)))
    (dolist (transform transforms nil)
      ;; LF type and lex most specific...
      (if (rule-more-specific transform best) (setq best transform)))
    best))

(defun get-obligatories (transform)
  "@param transform A valid LF transform
   @desc Returns a list of obligatory roles as specified in the :constraints
         allobligatory is ignored."
  (let ((constraints (lftransform-constraints transform))
	obligs)
    (dolist (constraint constraints)
      (if (eq (first constraint) :obligatory)
	  (setq obligs (cons (second constraint) obligs))))
    obligs))

(defun reconstruct-lftype (typet &optional bindings)
  "@param typet A typetransform structure
   @param bindings Variable bindings
   @desc Returns the LF type or a :* triple"
  (let ((type (typetransform-lftype typet))
	(lex (typetransform-lflex typet)))
    (if (is-transform-var lex) (setq lex (simplekb::lookup lex bindings)))
    (if lex (list :* type lex) type)))

(defun reconstruct-lftype-given-kr (form kr)
  "@param typet A transform structure
   @param kr A kr class, an atom
   @desc Returns the LF type or a :* triple.
         It always returns some LF, even if the KR doesn't match.  The KR is
         used to get the lexical item if it was atom-mapped."
  (let* ((typet (lftransform-typetransform form))
	 (type (typetransform-lftype typet))
	 (lex (typetransform-lflex typet))
	 (krtype (typetransform-kr typet))
	 func)
    (cond ((and (is-transform-var krtype) (not (eq '? krtype)))
	   (setq func (second (assoc krtype (lftransform-functions form))))
;	   (format t "functions=~S~%" (lftransform-functions form))
;	   (format t "func=~S~%" func)
	   (when func
	     (setq func (apply (car func) kr '(:reverse t)))
	     ;; if we got a result from reversing the function
	     (when (car func)
	       ;; check that the lex is valid with the LF
	       (if (check-lex-validity type (car func)) 
		   (return-from reconstruct-lftype-given-kr (list :* type (car func))))))
	   ;; return something anyway, even if KR didn't match
	   (if (or (null lex) (is-transform-var lex))
	       type (list :* type lex)))
	  ((or (null lex) (is-transform-var lex))
	   type)
	  (t (list :* type lex)))))

(defun remove-transform (name transforms)
  "@desc Removes the first transform with the given name from the given list."
  (if (null transforms) nil
    (if (eq name (lftransform-name (first transforms)))
	(rest transforms)
      (cons (first transforms)
	    (remove-transform name (rest transforms))))))


#|
(ont::term v1 :instance-of ont::laptop-computer
			  :assoc-with v2 :assoc-with v3)
(ont::term v2 :instance-of ont::frequency-value
	  :value (:pair 2.4 ont::*gigahertz))
(ont::term v3 :instance-of ont::computer-memory
	  :computer-storage-size v4)
(ont::term v4 :instance-of ont::computer-storage-size-value
	  :value (:pair 256 ont::*megabyte))
|#

;; ----------------------------------------
;; Nate's full KR to LF transform algorithm
;; ----------------------------------------

(defun kr-terms-to-lf (krs root transforms)
  "@param krs A list of kr terms  i.e. ((ont::term v1 :instance-of lf) ...)
   @param root The variable of the root kr term
   @returns A list of all the valid lists of lf terms"
  ;; We recurse on each KR term and get all possible transformations
  (let ((all (kr-terms-recurse krs root transforms))
	final)
    ;; Check each list of LFs and only save the ones that have a single root node
    (dolist (pair all)
      (if (has-single-root (car pair)) (setq final (cons pair final))))

    (debug-om 15 "final kr-to-lf list is ~S~%" final)
    (reverse final)
    ))

(defun kr-terms-recurse (krs root transforms)
  "@param krs A list of kr terms  i.e. ((ont::term v1 :instance-of lf) ...)
   @param root The variable of the root kr term
   @returns A list of all the valid lists of lf terms...without checking for a single root"
  (cond ((null krs) nil)
	((= 1 (length krs)) ;; only one kr term
	 (kr-term-to-lf (first krs) transforms))
	(t 
	 (let* ((kr (first krs))
		(kr-var (second kr))
		all merged newroot
		;; transform the first kr term
		(pairs (kr-term-to-lf kr transforms))
		;; recurse
		(possibles (kr-terms-recurse (cdr krs) root transforms)))
	   
	   (dolist (poss possibles)
	     (dolist (pair pairs)
	       ;; merge the lfs
	       (setq merged (merge-roles (first pair) (first poss)))
	       (debug-om 15 "merged roles ~S~%" merged)
	       (when merged
		 (setq newroot (if (eq root kr-var) (second pair) root))
		 (setq all (cons (list merged newroot) all))
		 )))
	   
	   (debug-om 15 "all possibles ~S~%" all)
	   all))))

(defun remove-generic-roles (roles)
  "Check for roles with the same lf but with and without lex, for example ((:* ont::wireless w::wireless) ont::wireless)
   if pairs like this are found, keep only the one with the lex. This allows greater control of the word choice for GM since it cuts down on 'bare' lfs (without words specified) that are passed on to Surface Generation, which chooses words more or less randomly
  @param   list of roles
  @returns the role list cleaned of 'duplicate' generic roles"
  (let ((cleaned-roles roles))
    (dolist (role1 roles)
        (dolist (role2 (cdr roles))
	(when role2
	  (if (and (listp role1) (not (listp role2)) (eq (second role1) role2))
	      (setq cleaned-roles (remove role2 cleaned-roles))))))
  cleaned-roles)
  )

; (defun discourse-filter (lfs)
;   "return only lftypes that appeared in the discourse history"
;   (let ((dc-filtered-lfs nil))
;     (dolist (lf lfs)
;       (let ((thistype (if (listp lf) (second lf) lf)))
;       (if (dc::get-latest-triple-for-lftype thistype)
;           (pushnew lf dc-filtered-lfs))))
;     ;; if nothing found return original list -- in case it's a quantifier term or some other term that dc doesn't track
;     (if (not dc-filtered-lfs) (setq dc-filtered-lfs lfs))
;     dc-filtered-lfs)
;   )

#| (
      (((ONT::A ONT::V30585 (:* ONT::PATIENT W::PATIENT))) ONT::V30585)
      (((ONT::A ONT::V30585 (:* ONT::PERSON W::PERSON))) ONT::V30585))
|#
(defun get-most-specific-term (termlist)
  (let (res  most-specific-type)
    (dolist (term termlist)
      (let* ((thislfterm (third (car (first term))))
	    (thislftype (if (listp thislfterm) (second thislfterm) thislfterm)) 
	    )
	(debug-om 20 "term is ~S lfterm is ~S lftype is ~S~% " term thislfterm thislftype)
	(if most-specific-type
	    (if (more-specific thislftype most-specific-type)
		(progn (setq most-specific-type thislftype)
		       (pushnew res term :test #'equal)))
	  (setq most-specific-type thislftype))
	)
      )
    (debug-om 20 "most specific type is ~S~%" res) 
   res)
  )


(defun valid-lf (lf)
  "@param lf An lf term with 3 parts: (identifier var lf-description)
   check the lf with the lexicon to see if the term identifier (i.e. ont::the) is consistent with the lexical item"
  (let ((res))
    (when (listp lf)
      (let ((identifier (first lf))
	    (lfdesc (third lf))
	    )
	;; only validate when the word is present
	(if (listp lfdesc)
	    (let* ((poslist (lxm::get-pos lfdesc :search-children t))) ;; get the pos list for this lftype - word pair
	      (ecase identifier
		;; for a definite id, the word must be a noun, pronoun or name
		(ont::the (setq res (or (member 'w::n poslist) (member 'w::pro poslist) (member 'w::name poslist))))
		;; for an indefinite or bare id, the word must be a noun
		(ont::a (setq res (member 'w::n poslist)))
		(ont::bare (setq res (member 'w::n poslist)))
		;; for a relational term, the word must be a verb, adverb or adjective
		(ont::f (setq res (or (member 'w::v poslist) (member 'w::adv poslist) (member 'w::adj poslist))))
		(ont::kind (setq res (member 'w::n poslist))) ;; sets
		(ont::term (setq res t)) ;; let these go through for now
		(t (setq res t))
		)
	      )
	  (setq res t)) ;; assume valid if no word present -- SG will fill in word
	  )
	)
    res)
  )

(defun kr-term-to-lf (kr transforms)
  "@param kr A full kr term    i.e. (ont::term v1 :instance-of lf)
   @returns List of pairs: (list of LF terms, variable of the root term)"
  (debug-om 1 "-----------------------------~%")
  (debug-om 1 "kr-term-to-lf ~S~%" kr)
  ;; remove any :equals or :force roles for processing
  ;; these roles are inserted by post-parsing components and do not transform
  (setq kr (remove-keyword-arg kr :equals)) 
  (setq kr (remove-keyword-arg kr :force))
  
  ;; begin normal processing
  (let* ((kr-instance (get-keyword-arg kr :instance-of))
	 (lf-instances (kr-atom-to-lf kr-instance transforms))
	 (main (car lf-instances))
	 def-instances final bestroles defroles)
    (setq kr (remove-keyword-arg kr :instance-of))

    (debug-om 15 "kr-atom options: ~S~%" lf-instances)
    (debug-om 5 "roles-in-context ~S~%" (cddr kr))

    ;;  if multiple options with same lf, e.g. ((:* ONT::WIRELESS W::WIRELESS) ONT::WIRELESS), keep only those with the lex
    ;; this measure improves the GM by not leaving the lex choice to Surface Realization
    (when (> (length lf-instances) 1)
      (setq lf-instances (remove-generic-roles lf-instances))
      (debug-om 15 "lf options after removing generics: ~S~%" lf-instances)
      )
    
    ;; ***NOTE***
    ;; We loop here to cover all possibilities, but by doing this, some more generalized
    ;; rules are included with specific ones.  We run the risk of not choosing the better,
    ;; more specific rules in the end (inside terms-to-lf).
    ;; ** ms 11/2007 added call to remove-generic-roles to favor more specific rules when they exist
    
    ;; find some roles, try each lf-instance in order
    (loop while (and lf-instances (not bestroles)) do

	  (if lf-instances (setq main (car lf-instances)))
	  (setq lf-instances (cdr lf-instances))
	  (debug-om 15 "main is ~S roles are ~S instance is ~S~%" main (keywords-to-kr (cddr kr)) kr-instance)
	  (setq bestroles (roles-in-context (keywords-to-kr (cddr kr)) kr-instance (second kr)
					    (if (listp main) (second main) main) transforms))
	  (debug-om 15 "best roles are: ~S~%" bestroles)
	  ;; if we found some normal rules to match
	  (when bestroles
	    (debug-om 15 "working option: ~S~%" main)

	    (dolist (pair bestroles)
	      (let ((lfroles (first pair))
		    (lex (second pair))
		    lf lfs root-var)
		
		;; begin building the main lf term (add lexical item if available)
		
		 ;; first step is to create the identifier,  ont::the, ont::f & add the ONT::var
		(setq lf (list (convert-term-type (first kr)) (second kr)))
		;; now the main lf term
		(if (and (listp main) lex (not (eq lex (third main))))
		    (debug-om 15 "OM WARNING: mismatched lexical items ~S/=~S~%" lex (third main)))
		(setq lf (append lf (list (if (and lex (atom main))
					      (list :* main lex) main))))

		;; swift -- only update the lfs if this is a valid lf insofaras the identifier is consistent with the lexeme (i.e. don't create a term for "the here")
		;; 20090724 remove this check now that we're using unknown words
;;		(when (valid-lf lf)
		  (debug-om 15 "lf is valid ~S ~%" lf)
		  ;; attach the roles
		  (dolist (role lfroles)
		    (if (keywordp (car role)) (setq lf (append lf role))
		      (setq lfs (cons role lfs))))
		  
		  ;; find the root term
		  (setq lfs (cons lf lfs))
		  (setq root-var (has-single-root lfs :ignore '(:of)))
		  (setq final (cons (list lfs root-var) final));)		
		)))

	  ;; remove duplicates
	  (if final (setq final (remove-duplicates final :test #'equal)))
	  (debug-om 15 "final is ~S~%" final)
	  ;; reset loop
	  (setq bestroles nil))

    ;; Try searching in the default creation rules
    (when (not final)
      (setq def-instances (kr-atom-default-to-lf kr-instance transforms))
      ;; find any roles from the default kr's
      (loop while (and def-instances (not defroles)) do
	    (if def-instances (setq main (car def-instances)))
	    (setq def-instances (cdr def-instances))
	    (setq defroles (roles-in-context (keywords-to-kr (cddr kr)) kr-instance (second kr)
					     (if (listp main) (second main) main) transforms
					     :defaults t)))

      (debug-om 15 "working default: ~S~%" main)
      (if (null defroles) (setq main nil))

      (dolist (pair defroles)
	(let ((lfroles (first pair))
	      (lex (second pair))
	      lf lfs root-var)
	  
	  ;; begin building the main lf term (add lexical item if available)
	  (setq lf (list (convert-term-type (first kr)) *ANY-VAR*))
	  (if (and (listp main) lex (not (eq lex (third main))))
	      (debug-om 15 "OM WARNING: mismatched lexical items ~S/=~S~%" lex (third main)))
	  (setq lf (append lf (list (if (and lex (atom main))
					(list :* main lex) main))))
	  
	  ;; attach the roles
	  (dolist (role lfroles)
	    (if (keywordp (car role)) (setq lf (append lf role))
	      (setq lfs (cons role lfs))))

	  ;; attach :context t
	  (setq lf (append lf '(:context t)))

	  ;; find the root term
	  (setq lfs (cons lf lfs))
	  (setq root-var (has-single-root lfs :ignore '(:of)))
	  (setq final (cons (list lfs root-var) final))
	  ))
      )
    
    (debug-om 5 "kr-term-to-lf returns: ~S~%" (reverse final))
    (reverse final)
    ))


(defun roles-in-context (roles instance var lf-instance transforms &key lf-roles defaults)
  "@param roles List of roles and values in the kr package:
                  (ont::assoc-with v123 ont::agent v124)
   @param lf-instance An lf instance, an atom
   @param transforms The list of transforms we're using
   @returns List of pairs: 1. List of lf roles
                           2. Lexical item for this instance, if available"
  (cond
   ((null roles) (list (list lf-roles nil)))
   (t
    (let ((role (first roles))
	  (val (second roles))
	  forms merged lex final)

      (debug-om 5 "convert role ~S value ~S~%" role val)
      ;; transform the role into an LF role  (lf roles, and an lf parent type)
      (setq forms (if defaults (kr-role-default-to-lf role val instance transforms)
		    (kr-role-to-lf role val instance transforms)))
      
      (debug-om 15 "roles: ~S~%" forms)
      
      ;; remove any that have a different lf-type than our parent lf
      (setq forms
	    (mapcan #'(lambda (x) (if (or (equal lf-instance (second x))
					  (and (listp (second x))
					       (eq lf-instance (second (second x)))))
				      (list x) nil))
		    forms))
     
      ;; substitute our variable for any ?*top* occurences
      (setq forms (simplekb::subst-bindings `((?*top* . ,var)) forms))
        
      ;; add :mods if necessary
      (setq forms (mapcar #'(lambda (x) (list (add-mods (car x) var)
					      (second x)))
			  forms))
      
      ;; add filler variables where needed  i.e. ?v -> V912
      (setq forms (randomize-variables forms))
      
      (debug-om 15 "remaining roles: ~S~%" forms)
		   
      ;; cycle through the forms till we have a match
      (dolist (newforms forms nil)
	;; merge the results if we can
	(setq merged (merge-roles (first newforms) lf-roles))

	
	;; recurse on the rest of the roles
 	(when (or merged (null (first newforms)))
	  (setq lex (if (listp (second newforms)) (third (second newforms))))
	  ;; recurse
	  (mapcar #'(lambda (pair)
		      ;; lexical items can't clash
		      (when (or (null (second pair)) (eq lex (second pair)))
			;; must have one root
			(if (has-single-root (first pair) :ignore '(:of))
			    (setq final (cons (list (first pair) lex) final)))))
		  (roles-in-context (cddr roles) instance var lf-instance transforms
				    :lf-roles merged :defaults defaults))
	  )
	) ;; dolist
      (debug-om 40 "final to fill variables ~S~%" final)
      ;; remove any unfilled variables in the roles; this overgenerates by adding spurious vars
      ;; can't do this here because it prevents spontaneous modifier terms, e.g. to-locs, from being generated; -- need to remove spurious vars at a later processing stage, before output
      ;; (remove-unfilled-variables final))))
      (fill-variables final))))
  )

;; ----------
(defun has-single-root (roles &key ignore)
  "@param ignore A list of features to ignore in traversing the tree
   @returns True if the feature roles covered everything, or return the
            root term if there is a single root term that covers all,
            else return nil"
  (let (terms vals temp)
    (dolist (role roles)
      (if (keywordp (first role))
	  (setq vals (append (traverse (second role) roles
				       :visited vals :ignore ignore)
			     vals))
	(setq terms (cons role terms))))
    (setq vals (remove-duplicates vals))

    (if (all-traversed terms vals) t
      ;; find one term that is parent of everything...
      (dolist (term terms nil)
	(setq temp (append (traverse (second term) terms :ignore ignore) vals))
	(if (all-traversed terms (remove-duplicates temp))
	    (return (second term)))))
    ))

(defun all-traversed (lfs visited)
  "@param lfs List of lf terms
   @param visited List of variables"
  (dolist (lf lfs t)
    (if (not (member (second lf) visited)) (return nil))))

(defun traverse (var lfs &key visited ignore)
  "@param ignore A list of features to ignore
   @returns A list of variables that are visited from the given root var"
  (if (member var visited) visited
    (let (vals)
      (dolist (lf lfs)
	;; found the matching lf
	(when (and (not (keywordp (first lf)))
		   (eq var (second lf)))
	  (setq vals (get-role-values lf :ignore ignore))
	  (setq vals (mapcan #'(lambda (x) (traverse x lfs :visited (cons var visited) :ignore ignore))
			     vals))
	  (return (remove-duplicates (append vals visited (list var)))))
	))))

(defun get-role-values (lf &key ignore)
  "@param ignore A list of features to ignore
   @returns A list of role values in the given lf"
  (cond ((null lf) nil)
	((and (keywordp (first lf)) (not (member (first lf) ignore)))
	 (cons (second lf) (get-role-values (cddr lf) :ignore ignore)))
	(t (get-role-values (cdr lf) :ignore ignore))))
;; ----------

(defun reverse-map (kr)
  "@returns The inverse of the *lf-atom-map* file"
  (dolist (m *lf-atom-map* nil)
    (if (or (equal kr (second m))
	    (and (stringp kr) (stringp (second m)) (string-equal kr (second m))))
	(return (first m)))))

(defun remove-unfilled-variables (x &optional binds)
  "removes any terms with ?v unfilled vars"
  (cond ((null x) (values nil binds))
	((listp x)
	 (multiple-value-bind (y bs)
	     (remove-unfilled-variables (car x) binds)
	   (multiple-value-bind (z bs2)
	       (remove-unfilled-variables (cdr x) bs)
	     (values (cons y z) bs2))))
	((is-transform-var x) ;; ?v
	 (let ((temp (simplekb::lookup x binds)))
	   (debug-om 15 "unfilled vars: ~S ~S ~S ~%" temp x binds)
	   (when (null temp)
	  (values nil nil))))
	(t (values x binds))))

(defun fill-variables (x &optional binds)
  "Converts any ?v variables into a trips V123 type variable"
  (cond ((null x) (values nil binds))
	((listp x)
	 (multiple-value-bind (y bs)
	     (fill-variables (car x) binds)
	   (multiple-value-bind (z bs2)
	       (fill-variables (cdr x) bs)
	     (values (cons y z) bs2))))
	((is-transform-var x) ;; ?v
	 (let ((temp (simplekb::lookup x binds)))
	   (when (null temp)
	     (setq temp (gentemp "V00"))
	     (setq binds (cons (cons x temp) binds)))
	   (values temp binds)))
	(t (values x binds))))

(defun randomize-variables (x &optional binds)
  "Converts any ?v variables into a random ?v3487 variable"
  (cond ((null x) (values nil binds))
	((listp x)
	 (multiple-value-bind (y bs)
	     (randomize-variables (car x) binds)
	   (multiple-value-bind (z bs2)
	       (randomize-variables (cdr x) bs)
	     (values (cons y z) bs2))))
	((is-transform-var x) ;; ?v
	 (let ((temp (simplekb::lookup x binds)))
	   (when (null temp)
	     (setq temp (gentemp "?V"))
	     (setq binds (cons (cons x temp) binds)))
	   (values temp binds)))
	(t (values x binds))))

(defun add-mods (role-list root-var)
  "@param role-list A list of argtransforms from converted krs
                    i.e. ((:ASSOC-WITH V13) (ONT::QUANTITY-TERM V13 ONT::BLAH))
   @param root-var The variable of the kr term we're converting
   @returns The same role-list with :MODS added if necessary"
  (cond ((null role-list) nil)
	((keywordp (caar role-list))
	 (cons (car role-list) (add-mods (cdr role-list) root-var)))
	(t
	 (let* ((role (car role-list))
		(of (get-keyword-arg role :of)))
	   (if (and of (eq of root-var))
	       (append (list role `(:mods ,(second role)))
		       (add-mods (cdr role-list) root-var))
	     (cons (car role-list) (add-mods (cdr role-list) root-var)))))))


(defun merge-roles (roles1 roles2)
  "@returns A merged list of the roles, or nil if it cannot be merged.
   @desc This function combines two lists of roles and removes any that
         subsume each other.  Returns nil if there are conflicts.
         If there is a full term with :context t, it merges the term with
         a matching term, if possible. "
  (let (merged roles temp weak)
    (setq roles (append roles1 roles2))
    (debug-om 15 "merging roles ~S and ~S ~%" roles1 roles2)
    (dolist (role roles merged)
      (setq temp nil)
      (setq weak nil)
      
      (cond
       ((null merged) (setq temp (list role)))
       (t
	;; loop through the current merged list 
	(dolist (merge merged)
	  (case (role-relationship role merge)
	    (equal
	     nil ;; do nothing, skip this merged role
	     )
	    (dominate
;	     nil
	     (if (not (keywordp (car role))) ;; subsume the term
		 (setq role (subsume-term role merge)))
	     )
	    (independent
	     (setq temp (cons merge temp))
	     )
	    (weak
	     (setq weak t)
	     (if (not (keywordp (car role))) ;; subsume the term
		 (setq merge (subsume-term merge role)))
	     (setq temp (cons merge temp))
	     )
	    (conflict
	     (return-from merge-roles nil)
	     )
	    (t (break "Improper returned result~%")))
	  )
	(if (not weak) (setq temp (cons role temp)))))
      (setf merged temp)
      )
    ))


(defun role-relationship (role1 role2)
  "equal       - both are equal (use one or the other)
   independent - the roles are completely separate
   conflict    - the two roles interfere with each other
   dominate    - the roles are friendly, and the first one is more specific
   weak        - the roles are friendly, and the first one is less specific"
  (cond
   ;; (:feat val)
   ;; if both are keywords
   ((and (keywordp (first role1)) (keywordp (first role2)))
    (if (eq (first role1) (first role2))
	(if (eq (second role1) (second role2))
	    'equal
	  (if (member (first role1) '(:MODS :ASSOC-WITH :suchthat)) 'independent
	    (if (is-transform-var (second role2)) 'dominate
	      (if (is-transform-var (second role1)) 'weak
		'independent))))
      'independent))
   ;; role1 or role2 is not a keyword, but the other is
   ((or (keywordp (first role1)) (keywordp (first role2))) 'independent)
   ;; any variable indicators, put into :context terms
   ((or (eq *ANY-VAR* (second role1))
	(eq *ANY-VAR* (second role2)))
    (if (not (lfs-related (third role1) (third role2)))
	'independent
      (if (get-keyword-arg role1 :context)
	  'weak
	(if (get-keyword-arg role2 :context)
	    'dominate
	  ;; no :context present, so find which is more specific
	  (if (lf-more-specific (third role1) (third role2))
	      'dominate
	    'weak)))))
   ;; (ont::f v12 lf)
   ((eq (second role1) (second role2)) ;; terms have the same variable
    (if (not (lfs-related (third role1) (third role2)))
	'conflict
      ;; same variable, check for :context role
      ;; If :context t, then we subsume this context role and get rid of it
      (if (get-keyword-arg role1 :context)
	  'weak
	(if (get-keyword-arg role2 :context)
	    'dominate
	  ;; no :context present, so find which is more specific
	  (if (lf-more-specific (third role1) (third role2))
	      'dominate
	    'weak))))
    )
   (t 'independent)))


(defun lfs-related (lf1 lf2)
  "@param lf1/lf2 An lf like (:* lf lex) or just lf
   @returns Non-nil if they are related in the ontology hierarchy, nil otherwise"
  (let ((type1 (if (listp lf1) (second lf1) lf1))
	(type2 (if (listp lf2) (second lf2) lf2)))
    (or (subtype type1 type2) (subtype type2 type1))))


(defun lf-more-specific (lf1 lf2)
  (let ((type1 (if (listp lf1) (second lf1) lf1))
	(type2 (if (listp lf2) (second lf2) lf2))
	(lex1  (if (listp lf1) (third lf1) nil))
	(lex2  (if (listp lf2) (third lf2) nil)))
    (cond
     ;; equal LF types
     ((eq type1 type2)
      ;; one has a lex, two does not
      (cond 
       ((null lex1)
	;; If the types are equal, but one does not have a var, it cannot be more specific
	nil)
       ;; after this point one definitely has a lflex
       ((null lex2)
	;; so if two doesn't, one is more specific
	t)
       ((and (not (is-transform-var lex1))
	     (is-transform-var lex2))
	t ;; if one does not have a lexical variable, but two does, then two is more specific
	)
       (t nil)
       ))	     
     ;; LF type of one is more specific
     ((subtype type1 type2)
      t)
     ;; LF type is not equal or a subtype
     (t nil))))

(defun subsume-term (dominate weak)
  "@param dominate A full term
   @param weak A full term
   @returns The combination of the two, merging their roles together, keeping the LF
            of the dominate term, and if their are duplicate roles, keeping the dominate ones"
  ;; remove any :context features still hanging around...
  (setq weak (remove-keyword-arg weak :context))
  ;; create the new term
  (append (subseq dominate 0 3) (subsume-role-values (cdddr dominate) (cdddr weak))))

(defun keywords-to-kr (x)
  "@x A keyword/val list:  (:key1 val :key2 val2 ...)
   @returns Converted x where all keywords are in the KR package"
  (cond ((null x) nil)
	((and (listp x) (< 1 (length x)))
	 (append (list (util::convert-to-package (car x) :ont :convert-keywords t)
		       (second x))
		 (keywords-to-kr (cddr x))))
	(t x)))

(defun add-context-flag (args)
  "@param args A list of argtransforms
   @returns The same list, but with (:context t) appended to each term"

  (mapcar #'(lambda (x) (if (not (keywordp (car x)))
			    (append x '(:context t)) x))
	  args))

(defun convert-term-type (type)
  "@param type A term type: e.g. ont::the, ont::reln
   @return The LF term type"
  (case type
    (ont::the 'ont::the)
    (ont::a 'ont::a)
    (ont::reln 'ont::f)
    (ont::kind 'ont::kind) ;; sets
    ;; default, unknown
    (t 'ont::term)))

