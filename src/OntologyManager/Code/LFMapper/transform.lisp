;; Nate Chambers
;; IHMC
;;
;; This file contains the functions that do the LF conversion to a KR
;;

(in-package "ONTOLOGYMANAGER")

(defvar *lf-atom-map* nil)
(defvar *dynamic-transforms* nil)


(defun old-lf-to-kr (LF transforms)
  "@param LF A list of TRIPS LF terms or a valid TRIPS UTT.
   @param transforms A pair (<list of normal lftransforms> <list of abstracts>)
   @returns A list of KRs that have been syntactically converted from a base
            lambda form.  The result may be a lambda form, depending on the
            amount of syntactic conversion."
  (setf LF (utt-to-terms LF)) ;; extract just the list of terms, if needed
  (setf LF (remove-setofs LF))

  (multiple-value-bind (lambdas used-terms)
      (lf-to-lambdas LF transforms)
    (values (mapcar #'transform-syntax lambdas) ;; transform syntax
	    (mapcar #'(lambda (x) (unused-terms LF x)) used-terms))))

(defun lf-to-kr (LF transforms &key all)
  "@param LF A list of TRIPS LF terms or a valid TRIPS UTT.
   @param transforms A pair (<list of normal lftransforms> <list of abstracts>)
   @param all Set to true if you want all possibilities returned
   @returns A list of KRs that have been syntactically converted from a base
            lambda form.  The result may be a lambda form, depending on the
            amount of syntactic conversion."
  (setf LF (utt-to-terms LF)) ;; extract just the list of terms, if needed
;  (setf LF (remove-setofs LF))

  (let ((forms (lf-to-lambdas LF transforms :all all)))
    (values (mapcar #'(lambda (x) (transform-syntax (car x))) forms)
	    (mapcar #'second forms)
	    (mapcar #'third forms)
	    )))

(defun lf-to-lambdas (LF transforms &key all)
  "@param LF A list of TRIPS LF terms.
   @param transforms A triple (<list of normal lftransforms> <list of abstracts> <list of defaults>)
   @param all Set to true if you want all possibilities returned
   @returns A list of triples:
            The KR, the LF that was left over (not matched against), and the matching report.
            Each KR is a possibility from the rules, in order of specificity"

  (let ((triples (list (list nil LF nil :defaults nil)))
	;; triples = ((KR LF Report :defaults nil) ...)
	trip results finished)

    (loop while triples do

      ;; Continue converting this LF
      (setq trip (pop triples))
      (let ((theKR (car trip))
	    (theLF (second trip))
	    (theReport (third trip))
	    (defaults (get-keyword-arg trip :defaults))
	    converted)
	(dolist (term theLF)
	  (debug-om 1 "processing term ~S~%" term)
	  (setq results
		(transform-lf-term theLF (get-termvar term)
				   (if defaults (list (third transforms) (second transforms)) transforms)
				   :all all))
	  (debug-om 1 "results for term ~S are ~S~%" term results)
	  ;; if we transformed the term, add its triple to the stack	 
	  (when results
	    (setq triples
		  (append (mapcar #'(lambda (x) (list (append theKR (car x))
						      (second x)
						      (append theReport (third x))
						      :defaults defaults
						      )) ;; new LF
				  results)
			  triples))
	    (setq converted t)
	    (return))
	  )

	(cond
	 ;; if normal transforms are finished, apply the default ones
	 ((and (null converted) (null (get-keyword-arg trip :defaults)))
	  (setf (second (member :defaults trip)) t)
	  (setq triples (cons trip triples)))
	 ;; everything finished, add to the finished converting list
	 ((null converted)
	  (setq finished (cons trip finished))))
	)
      )
    
    (reverse (remove-duplicates finished))))
	  
(defun old-lf-to-lambdas (LF transforms)
  "@param LF A list of TRIPS LF terms.
   @param transforms A pair (<list of normal lftransforms> <list of abstracts>)
   @returns Two values: A list of KRs in lambda form that can be created from
                          the given LF using the list of transforms.
                        A list of LF variables of terms we matched against."
  (let ((krs nil) (krterms nil) used used-terms new-used)
    (dolist (term LF)
      (multiple-value-setq (krterms used)
	(transform-lf-term LF (get-termvar term) transforms))
      ;; save the used terms
      (when krterms
	(setq new-used nil)
	(dolist (uu used)
	  (dolist (u used-terms)
	    (setq new-used (cons (union u uu) new-used))))
	(setf used-terms (or (reverse new-used) used)))
      
      (if (null krs) (setf krs krterms)
	;; add the new term transforms to each kr already created
	(let ((newkrs nil))
	  (dolist (term krterms) ;; for each new kr term
	    (dolist (kr krs) ;; for each kr already created
	      (setf newkrs (cons (append term kr) newkrs))))
	  ;; reverse the newkrs to maintain an 'ordering' at least
	  (if newkrs (setf krs (reverse newkrs)))))
      )
    (values krs used-terms)))


(defun transform-lf-term (LF var transforms &key all)
  "@param transforms A pair (<list of normal lftransforms> <list of abstracts>)
   @param all Set to true if you want all possibilities returned
   @desc Transforms a single LF term indicated by the parameter var.
         LF is a list of terms, containing the var term.
   @returns A list of triples in order of specificity:
            1. the KR that was created
            2. the remaining LF that wasn't matched
            3. the application report"
  (let* ((term (term-with-var LF var))
	 (fulls (match-transforms LF term (car transforms)))
	 newLF res results report)
    (debug-om 1 "Converting term ~S~%" term)
    ;; perform the transformation on each non-abstract transform
    (dolist (specific-full fulls)
      (multiple-value-setq (res newLF report)
	(lfterm-to-kr LF var specific-full (second transforms)))
      (when res
	;; remove the main LF term we are transforming
	(setq newLF (remove var newLF :key #'second))
	;; return the first result unless we want all possibles
	(if all (setf results (cons (list res newLF report) results))
	  (return-from transform-lf-term (cons (list res newLF report) results)))
	))

    ;; return the KRs and remaining LFs
    (reverse results)
    ))


(defun lfterm-to-kr (LF lfvar transform abstracts)
  "@desc Transform a single lfterm into a KR form using the given transform and
   a list of abstract transforms.
   @param LF Full list of LF terms.
   @param lfvar The variable of the LF term to transform.
   @param transform The single non-abstract transform to use.
   @param abstracts The list of abstract transforms to use.
   @returns 3 values: The KR form we created (a list of lambdas)
                      The remaining LF we did not match against
                      The list of transform application structures
"
  (debug-om 1 "Applying transform ~S%" (lftransform-name transform))
  
  (let* ((bindings nil)   ;; variable bindings within the transform
	 (kr nil)         ;; variable to kr-lambda bindings (new objects)
	 (rootvar  (lftransform-typevar transform))
	 (typetran (lftransform-typetransform transform))
	 (typekr (typetransform-kr typetran))
	 (lfterm (term-with-var LF lfvar))
	 (lftype (second lfterm)) 
	 (lfterm-report (make-transform-application-report :transform transform))
	 (lfarg-reports nil)
	 temp)
    (debug-om 1 "lftype is ~S lfterm is ~S~%" lftype lfterm)
    ;; bind the root transform variable
    (push (cons rootvar lfvar)
	  bindings) 
    ;; bind the :typetransform lexical variable    
    (if (is-transform-var (typetransform-lflex typetran))
	(push (cons (typetransform-lflex typetran)
		    (extract-lflex (term-with-var LF lfvar)))
	      bindings))

    (cond ((listp typekr) ;; (:map ?v)
	   (setq temp (or (get-lf-atom-map (is-bound (second typekr) bindings)) lftype))
	   (if temp
	       ;; create the new object from the atom map
	       (setf kr (create-object kr lfvar temp))	     
	     ;; atom-map failed, return nil right now
	     (return-from lfterm-to-kr nil))
	  (debug-om 1 "created term ~S~%" kr)
	   )

	  ;; resolve the variable binding first
	  ((is-transform-var typekr)
	   (let ((bound (is-bound typekr bindings))
		 func)
	     (cond (bound
		    (setf kr (create-object kr lfvar bound)))
		   ;; apply a function to get the variable
		   ((setq func (second (assoc typekr (lftransform-functions transform))))
		    (setq bindings (apply-function func typekr bindings
						   (lftransform-functions transform)))
		    (setq temp (is-bound typekr bindings))
		    (if temp
			(setf kr (create-object kr lfvar (is-bound typekr bindings)))
		      (return-from lfterm-to-kr nil))
		    )
		   ;; else make a nil object
		   (t (setf kr (create-object kr lfvar nil))))))
	  ;; else just create the new object
	  (t (setf kr (create-object kr lfvar typekr))))

    
    (debug-om 15 "lfterm-to-kr: created new object kr=~S~%" kr)
    (when kr
      (push lfterm (transform-application-report-lfterms lfterm-report))
      (push kr (transform-application-report-krterms lfterm-report))
      )

    ;; apply the argument transforms in this transform
    (multiple-value-setq (kr bindings LF lfarg-reports)
      (arg-transform-term LF kr bindings (term-with-var LF lfvar) transform))

    (setq abstracts (match-transforms LF (term-with-var LF lfvar) abstracts))
    
    ;; step through each abstract, attempt each arg transform
    (dolist (abstract abstracts)
      ;; Check that we still match - new 5-11-06
      (when (is-transform-match LF (term-with-var LF lfvar) abstract)
	
	;; reset the bindings for the next abstract transform
	(let ((typevar (lftransform-typevar abstract))
	      (lflex (typetransform-lflex (lftransform-typetransform abstract)))
	      (abstract-report nil)
	      )
	  (if (is-transform-var lflex)
	      (setf bindings (cons (cons lflex (extract-lflex (term-with-var LF lfvar)))
				   (list (cons typevar lfvar))))
	    (setf bindings (list (cons typevar lfvar))))
	  
	  ;; if the type of the kr object that was just created is the same as
	  ;;    the type of the kr transform object, then apply it
	  ;; or if the abstract's kr type is a ? mark, apply it
	  (when (or (eq '? (typetransform-kr (lftransform-typetransform abstract)))
		    (eq (get-type (get-kr-object lfvar kr))
			(typetransform-kr (lftransform-typetransform abstract))))
	    (debug-om 1 "Applying abstract transform ~S~%"
		      (lftransform-name abstract))
	    ;; perform the argument transforms
	    (multiple-value-setq (kr bindings LF abstract-report)
	      (arg-transform-term LF kr bindings (term-with-var LF lfvar) abstract))
	    (setq lfarg-reports (append lfarg-reports abstract-report))
	    ))))

    (values kr LF (cons lfterm-report lfarg-reports)))
  )

(defun arg-transform-term (LF kr thebindings lfterm transform)
  "Use each argument in the main transform and apply it to the lfterm
   and/or other terms in the LF.
   @returns The KR that was created with the arguments in the transform.
               --> a list of lambdas
            A variable binding list.
            The new LF with the :content arguments removed
            The list of transform-application-report structures as a report
"
  (let (vars func argkr bindings newbinds oldbinds
	report arg-report
	)

    ;; bind the LHS variables and save the matched arguments' RHS
    (dolist (arg (lftransform-argtransforms transform))
      (multiple-value-bind (match argbindings)
	  (is-argtransform-match thebindings LF lfterm arg)
	(debug-om 20 "Argument match=~S~%" match)
	(when match
	  (setq arg-report (make-transform-application-report :transform (list transform arg)))
	  ;; The bindings list is a list of lists...usually just of length one.
	  ;; But if the argument matched twice on the same LF, it will contain
	  ;; both bindings lists.  We add each one...keeping new bindings and
	  ;; overwrite the duplicated ones.
	  (dolist (binds argbindings)
	    (setf bindings (merge-bindings thebindings binds))
	    (setq oldbinds bindings)
	    (setq argkr (argtransform-kr arg))
	    (setq vars (remove-duplicates (extract-vars argkr)))
	    ;; check each variable, we should know each one 
	    (dolist (var vars)
	      ;; if we don't know a variable, check :functions or :defaults
	      (when (not (assoc var bindings))
		(cond ((setq func (second (assoc var (lftransform-functions transform))))
		       (setq bindings (apply-function func var bindings
						      (lftransform-functions transform)))
		       )
		      ((and (assoc var (lftransform-defaults transform))
			    (eq var (third argkr)))
		       (debug-om 15 "Add a new object ~S~%" argkr)
		       (multiple-value-setq (kr bindings)
			 (add-object kr bindings (lftransform-defaults transform)
				     (first argkr) (second argkr) (third argkr)))
		       )
		      (t (signal-error "Error: unknown variable ~S~%" var)
			 nil))))
	    ;; bind the RHS variables
	    (setq argkr (subst-bindings bindings argkr))
	    ;; now apply the RHS
	    (let ((kr-pred (first argkr))
		  (kr-location (second argkr))
		  (kr-value (third argkr)))
	      ;; if 'value' is a map, (:map ?v), resolve the mapping
	      ;; **NOTE: this is deprecated, but keeping it in for compatibility
	      (if (and (listp kr-value) (eq :map (first kr-value)))
		  (setq kr-value (get-lf-atom-map (second kr-value))))

	      ;; add the argument to the kr object
	      (if argkr
		  (setq kr (add-argument kr bindings
					 kr-pred kr-location kr-value))))
	    ;; find what was added (new objects)
	    (setq newbinds (get-new-bindings oldbinds bindings))
	    ;; save the added bindings for the next argument
	    (setq thebindings (append thebindings newbinds))
	    ;; remove the matched arguments from the LF
	    (setq LF (remove-from-LF
		      (subst-bindings binds (argtransform-content arg))
		      LF
		      (get-termvar lfterm)))
	    (setf (transform-application-report-lfterms arg-report) (subst-bindings binds (argtransform-content arg)))
	    (setf (transform-application-report-krterms arg-report) (list kr))
	    (push arg-report report)
	    )
	  ;; set the bindings to the last possible argument binding
	  ;; ...if there were more than one possibles, we're essentially ignoring
	  ;;    the first ones.
	  (setq thebindings bindings))))
    (values kr bindings LF report)))

(defun remove-from-LF (args LF rootvar)
  "@param args A list of valid argtransform LHS lists, with all variables bound
   @param LF The full list of LF terms
   @param rootvar Variable of the root term
   @returns A new LF with the args removed from it"
  (let ((root (term-with-var LF rootvar)))
    (setq LF (remove rootvar LF :key #'second))
    (dolist (arg argS)
      (cond ((keywordp (car arg))
	     (setq root (remove-featval root arg)))
	    (t (setq LF (remove (second arg) LF :key #'second)))))
    (cons root LF)))

(defun remove-featval (thelist featval)
  "@param thelist A list
   @param featval A pair (:feature value)
   @returns The given list with the featval removed if it exists"
  (cond ((null thelist) nil)
	((and (< 1 (length thelist))
	      (eq (car thelist) (car featval))
	      (eq (second thelist) (second featval)))
	 (cddr thelist))
	(t (cons (car thelist) (remove-featval (cdr thelist) featval)))))
	    
	     
(defun apply-function (func func-var bindings functions)
  "@desc Works recursively by calling all the functions until the variables
         are all bound and the given function can be applied.
   @returns A new bindings list with all variables bound"
  (setq func (subst-bindings bindings func))

  (let ((vars (extract-vars func))
	other)
    ;; recurse on each variable by calling the proper function
    (dolist (var vars)
      (setq other (assoc var functions))
      (if other
	  (setq bindings (apply-function (second other) (car other) bindings functions))
	(signal-error "Error: unknown variable in function ~S" var))
      (setq func (subst-bindings bindings func))))
      
    ;; apply the original function
    (if (null (extract-vars func))
	(cons (cons func-var (apply (car func) (cdr func))) bindings)))
    
  
(defun create-object (kr var krtype-to-create)
  "Creates a new KR object identified by the variable var.  Adds the object
   to the current kr and returns the kr."
  (debug-om 15 "create a new object kr ~S type-to-create ~S~%" kr krtype-to-create)
  (cond ((member var kr)
	 (signal-error "Error: creating two objects in the KR with the same variable: ~S~%" var)
	 nil)
	;; handle (:lex-kr ?lex) as new kr type
	(;(and (listp krtype-to-create) (eq ':lex-kr (first krtype-to-create)))
	 (listp krtype-to-create)
	 (cons (list var (list 'lambda var (list 'ont::instance-of var (lex-kr (second krtype-to-create))))) kr))
	 (t (cons (list var
			(list 'lambda var
			      (list 'ONT::INSTANCE-OF var krtype-to-create)))
		  kr))))

(defun add-object (kr bindings defaults pred location newvar)
  "@desc Creates a new kr object as defined in the defaults.
   @returns 2 values: The new kr with the added object and the new bindings list."
  (let* ((object-var (if (is-transform-var location) (is-bound location bindings) location)))
    (debug-om 15 "adding object kr ~S bindings ~S defaults ~S pred ~S location ~S newvar ~S~%" kr bindings defaults pred location newvar)
    (cond ((null object-var)
	   (signal-error "ERROR: add-object has an unbound variable ~S~%" location)
	   nil)
	  ;; create a new object, add the argument to the original object
	  (t (let ((newkr-var (unique-id)))
	       ;; create a new object if a variable is given as newvar
	       (when (is-transform-var newvar)
		 ;; add the new binding
		 (setf bindings (cons (cons newvar newkr-var) bindings))
		 ;; create the new object
		 (setf kr (create-object kr newkr-var
					 (get-krtype kr bindings defaults
						     pred location newvar))))
	       )))
    (values kr bindings)
    ))

(defun add-argument (kr bindings pred location value)
  "Add a triple to the current KR.  i.e. (ACTOR ?vv ?v2)
   @returns The new KR."
  (debug-om 1 "Adding argument to the kr (~S ~S ~S)~%" pred location value)
  (let* ((object-var (if (is-transform-var location) (is-bound location bindings) location))
	 (object (get-kr-object object-var kr))
	 ;; if 'value' is a variable, get its bound value
	 (resolved (if (is-transform-var value) (is-bound value bindings)
		     value)))
    
    (when (null object-var)
      (signal-error "ERROR (transform.lisp): variable unbound in add-argument: ~S with pred ~S ~%" location pred)
      (return-from add-argument kr))
    ;; map the value if we can
    (if (and (listp resolved) (eql (first resolved) ':map))
	(setf resolved (if (is-transform-var (second resolved))
			   (get-lf-atom-map (is-bound (second resolved) bindings))
			 (get-lf-atom-map (second resolved)))))
    
    ;; resolve the predicate if it is a variable
    (if (is-transform-var pred) (setf pred (is-bound pred bindings)))
    ;; add the argument
    (setf object (append object (list (list pred object-var resolved))))
    (setf object (cons object-var (list object)))
    ;; remove previous from kr
    (setf kr (remove-object kr object-var))
    ;; add the new one to the kr
    (cons object kr)))

(defun remove-object (kr var)
  "Remove the object with variable var.  Non-destructive."
  (cond ((null kr) nil)
	((eq (first (first kr)) var)  (rest kr))
	(t (cons (first kr) (remove-object (rest kr) var)))))

(defun create-transform-from-lf (lf)
  (let* ((lftriple (third lf))
	 (onttype (second lftriple))
	 (this-typetransform (make-typetransform
			      :lftype onttype
			      :lflex (third lftriple)
			      :kr onttype
			      ))
	 (this-lftransform (make-lftransform
			    :name    'this-transform
			    :typevar '?vv
			    :typetransform this-typetransform
			    )))
    this-lftransform)	 
  )

(defun is-content-lftriple (lfterm)
  (and (listp (third lfterm))
       (eq ':* (first (third lfterm)))
       ;; exclude pronouns
       ;; include ont::quantifier to handle quantified NPs such as "more results"
       (member (first lfterm) '(ont::a ont::the ont::f ont::kind ont::bare ont::quantifier)))
  )

(defun match-transforms (full-lf lf transforms)
  "@param lf A single LF term.  e.g. (F V12 (:* ONT::MOVE TAKE))
   @param transforms A list of transforms
   @returns A list of matching transforms"
  (debug-om 10 "Checking LF ~S~%" lf)
  (let (forms)
    (dolist (transform transforms)
      (debug-om 50 "   Checking transform ~S~%" (lftransform-name transform))
      (when (is-transform-match full-lf lf transform)
	(debug-om 1 "   Matched transform ~S~%" (lftransform-name transform))
	(setq forms (add-transform-in-order transform forms))
	))
    ;; if there are no matching transforms, create one based on the lf
    (when (and (not forms) (is-content-lftriple lf))
	(setq forms (list (create-transform-from-lf lf)))
	(pushnew (car forms) *dynamic-transforms* :test #'equal) ;; add to this so it is available if needed for argtransforms
	)
    (debug-om 1 "forms are ~S~%" forms)
    forms))

(defun is-transform-match (full-lf lf transform)
  "@param lf A single LF term.  e.g. (F V12 (:* ONT::MOVE TAKE))
   @param transform A single transform structure
   @returns t or nil if the transform's type matches the lf"
  ;; now check that the transform type matches the lf type
  (let* ((ttypestruct (lftransform-typetransform transform))
	 (ttype (typetransform-lftype ttypestruct))
	 (tlex (typetransform-lflex ttypestruct))
	 (lftype (extract-lftype lf))
	 (lflex  (extract-lflex lf)))
    (cond
     ;; if lftype is not subtype of ttype, we have to stop right here
     ((not (lf-subtype lftype ttype))
      nil)
     ;; If it's subtype, we check more
     ((or (null tlex)
	  (is-transform-var tlex)
	  (eql tlex lflex)
	  )
      ;; lexical check succeeded, too. Now see if all the other constraints are satisfied with the lf
      (if (constraints-satisfied full-lf lf transform)
	  t nil)
      )
     (t nil) ;; if we got here, there was a subtype, but a lexical failure, so this is nil
     )
  ))


(defun is-argtransform-match (bindings LF lfterm argtransform)
  "@returns 3 values: True if it is a match,
                      A list of bindings lists that matches this argtransform"
  (let ((LHS (append (argtransform-content argtransform)
		     (argtransform-context argtransform)))
	matches newbinds fbinds terms)
    (debug-om 10 "Checking argument ~S~%" LHS)
    ;; matches is a list of possible binding lists for each argument on the LHS
    ;; - each argument might have multiple possibilities
    (setf matches (mapcar (lambda (x) (cond
				       ((is-lf-feature (first x))
					(find-matching-lfroles bindings lfterm x))
				       (t
					(find-matching-lfterms bindings LF x))))
			  LHS))
    ;; if the LHS is a nil rule
    (if (and (null LHS) (null matches)) (setf matches (list (list bindings))))
    (if (member nil matches) nil ;; nil if one of the LHS does not unify
      (setf newbinds (find-unified-matches matches)))

    (cond (newbinds
	   (debug-om 15 "      ...matched~%")
	   (dolist (binds newbinds)
	     ;; get all the terms we matched off
	     (setf terms (assoc-all '?*term* binds))
	     ;; remove the terms from the bindings now
	     (dotimes (i (length terms))
	       (setf binds (remove-binding '?*term* binds)))
	     (setf fbinds (cons binds fbinds)))
	   ;; return our new bindings
	   (values t fbinds))
	  (t (debug-om 15 "      ...not matched~%") nil))
    ))


(defun unified-matches (bindings)
  "@param matches A list of bindings lists
   @returns True if each bindings list can unify with each other"
  (let (temp)
    ;; check if everything is consistent, shouldn't be duplicate variables
    (if (dolist (bind bindings t)
	  ;; keep the ?*term* variables, storing the terms we matched off
	  (if (eq '?*term* (first bind)) (setf temp (cons bind temp))
	    ;; there shouldn't be any duplicate variables in here
	    (if (assoc (first bind) temp) (return nil)
	      (setf temp (cons bind temp)))))
	temp)))

(defun find-unified-matches (matches)
  "@param matches A complicated list of lists of bindings lists.  Each list of
                  matches represents each LHS of an argtransform rule.  These
                  lists are lists of bindings, each one representing a
                  different match in the LF.  i.e. (:assoc-with ?a) may match
                  two :assoc-with features on one LF term.
   @returns A list of variable bindings lists of which each LHS match is
            valid together.  The list gives every possible match."
  (let (temp goods combos ncombos)

    ;; make a list of all possible combinations
    (dolist (possibles matches)
      (cond ((null combos) (setq combos possibles))
	    (t
	     (setq ncombos nil)
	     (dolist (binds combos)
	       (dolist (poss possibles)
		 (setq ncombos (cons (union binds poss :test #'equal) ncombos))
		 ))
	     (setq combos ncombos))))

    ;; find which combos can unify
    (dolist (combo combos)
      (setq temp (unified-matches combo))
      (if temp (setq goods (cons temp goods))))

    goods))

    
;; Working function, but it only returned the first unified match.  We need
;; all possibilities.
#|
(defun find-unified-matches (matches)
  "@param matches A complicated list of lists of bindings lists.  Each list of
                  matches represents each LHS of an argtransform rule.  These
                  lists are lists of bindings, each one representing a
                  different match in the LF.  i.e. (:assoc-with ?a) may match
                  two :assoc-with features on one LF term.
   @returns A variable bindings list of which each LHS match is valid together"
  (let (bindings temp)
    ;; use the first possible bindings list on each match
    (dolist (possibles matches)
      (setf bindings (union (first possibles) bindings :test #'equal)))
    ;; check if everything is consistent, shouldn't be duplicate variables
    (when (dolist (bind bindings t)
	    ;; keep the ?*term* variables, storing the terms we matched off
	    (if (eq '?*term* (first bind)) (setf temp (cons bind temp))
	      ;; there shouldn't be any duplicate variables in here
	      (if (assoc (first bind) temp) (return nil)
		(setf temp (cons bind temp)))))
	(return-from find-unified-matches temp))
    ;; recurse, remove one of the possible bindings
    (dotimes (i (length matches))
      (when (> (length (nth i matches)) 1)
	(let ((cp-matches (copy-tree matches)) result)
	  ;; remove the first possible of the list
a	  (setf (nth i cp-matches) (rest (nth i cp-matches)))
	  ;; recurse
	  (setf result (find-unified-matches cp-matches))
	  ;; return if we got a match
	  (if result (return-from find-unified-matches result))
	  ))
      )
    ;; failed if we reached here
    nil))
|#

(defun unify-term-pattern (bindings term pattern)
  "@param term A single LF term.  e.g. (F V1 ONT::MOTION :THEME V2)
   @param pattern A single argtransform pattern.  e.g. (F ?v1 ONT::MOTION ...)
   @returns Two values, t|nil if the pattern matches, and the new bindings."
  (let ((match nil))
    ;; check term types
    (multiple-value-setq (match bindings)
      (unify-types bindings (first term) (first pattern)))

    ;; check variable position
    (if match
	(multiple-value-setq (match bindings)
	  (unify-types bindings (second term) (second pattern))))

    ;; check lftypes
    (if match
	(multiple-value-setq (match bindings)
	  (unify-lf-types bindings (third term) (third pattern))))

    ;; check arguments
    (if match
	(multiple-value-setq (match bindings)
	  (unify-arguments bindings term (rest (rest (rest pattern))))))

    ;; return true if the full pattern has matched
    (if match (values t bindings) nil)
    ))

(defun unify-types (bindings x y)
  "Returns t|nil if x and y match and the new bindings to do the unification.
   x and y are atoms."
  (cond ((eq x y)
	 (values t bindings))
	((is-transform-var y)
	 (let ((bound (is-bound y bindings)))
	   (cond ((null bound) (values t (cons (cons y x) bindings)))
		 ((eq bound x) (values t bindings))
		 (t (values nil bindings)))
	   ))
	(t (values nil bindings))))

;; (:* LF lex)  = LF
;; (:* LF lex) /= ?v           -> variables can't bind to lists
;; (:* LF lex)  = (:* LF ?v2)
;; (:* LF lex)  = (:* ?v ?v2)
;;          LF  = ?v
;;          LF /= (:* LF ?v2)  -> don't allow nil bindings
;;          LF /= (:* ?v ?v2)  -> ditto
(defun unify-lf-types (bindings lftype pattern)
  "Returns t|nil if x and y match and the new bindings
   x and y is an LF term and pattern. -> ONT::MOVE and (:* ONT::MOVE ?v)"
  (let ((ltype (if (listp lftype) (second lftype) lftype))
	(llex  (if (and (listp lftype) (= 3 (length lftype)))
		   (third lftype) nil))
	(ttype (if (listp pattern) (second pattern) pattern))
	(tlex  (if (and (listp pattern) (= 3 (length pattern)))
		   (third pattern) nil))
	(newbindings bindings))
    (cond
     ;; (:* ONT::X lex) = ONT::X
     ((and (listp lftype) (atom pattern) (lf-subtype ltype ttype))
      (values t bindings))
     ;; both must be atoms or both lists
     ((or (and (listp lftype) (atom pattern) (not (is-transform-var pattern)))
	  (and (atom lftype) (listp pattern))
	  (and (listp lftype) (listp pattern)
	       (/= (length lftype) (length pattern))))
      (values nil bindings))
     (t
      ;; convert type/lex variables to values
      (if (is-transform-var ttype)
	  (if (is-bound ttype newbindings)
	      (setf ttype (is-bound ttype newbindings))
	    (progn
	      (setf newbindings (cons (cons ttype ltype) newbindings))
	      (setf ttype (is-bound ttype newbindings)))))
      (if (is-transform-var tlex)
	  (if (is-bound tlex newbindings)
	      (setf tlex (is-bound tlex newbindings))
	    (progn
	      (setf newbindings (cons (cons tlex llex) newbindings))
	      (setf tlex (is-bound tlex newbindings)))))

      ;; If the two are subtypes and have matching lexical items, return true
      (cond ((and (or (null tlex) (eq tlex llex))
		  (lf-subtype ltype ttype))
	     (values t newbindings))
	    (t (values nil bindings)))
      ))))

(defun unify-arguments (bindings term pattern)
  "@param term A single LF term e.g. (F V12 ONT::MOTION :theme V13 ...)
   @param pattern Just the argument part of an LF argument pattern
                  e.g. (:theme ?v1 :agent ?v2 ...)
   @returns Two values, t|nil if the pattern matches, and the bindings list
            that includes the new bindings to make the pattern match.
            If it does not match, the original bindings list is returned."
  (cond ((null pattern) (values t bindings))
	;; if the first atom of the pattern is a feature
	((is-lf-feature (first pattern))
	 (let ((m (member (first pattern) term))
	       (b (if (is-transform-var (second pattern))
		      (is-bound (second pattern) bindings))))
	   (cond ((not m) (values nil bindings))
		 ;; two values are equal
		 ((equal (second m) (second pattern))
		  (unify-arguments bindings term (rest (rest pattern))))
		 ;; if pattern is a variable that is already bound
 		 (b (if (eq b (second m))
			(unify-arguments bindings term (rest (rest pattern)))
		      (values nil bindings)))
		 ;; if it's an unbound variable
		 ((is-transform-var (second pattern))
		  (let ((newbinds (cons (cons (second pattern) (second m))
					bindings)))
		    (multiple-value-bind (match newbinds2)
			(unify-arguments newbinds term (rest (rest pattern)))
		      (if match (values t newbinds2)
			(values nil bindings)))))
		 ;; else nil
		 (t (values nil bindings)))
	   ))
	(t nil)))

(defun constraints-satisfied (full-lf lf transform)
  "@param full-lf The list of LF terms we are converting.
   @param lf A single LF term to check constraints on.  e.g. (F V12 ONT::MOVE)
   @param transform A single transform definition.
   @returns True if all :obligatory and :prohibit constraints are ok"
  (let ((typevar (lftransform-typevar transform))
	(constraints (lftransform-constraints transform)))
    ;; check each constraint
    (dolist (constraint constraints)
      (case (first constraint)
	;; the given role must be in the LF
	(:obligatory
	 (when (not (member (second constraint) lf))
	   (debug-om 25 "      failed constraint ~S ~%" constraint)
	   (return-from constraints-satisfied nil)))
	;; the given role must not be in the LF
	(:prohibit
	 (when (member (second constraint) lf)
	   (debug-om 25 "      failed constraint ~S ~%" constraint)
	   (return-from constraints-satisfied nil)))
	(:nonrecursive
	 (let ((lftype (extract-lftype lf))
	       (transformtype (typetransform-lftype (lftransform-typetransform transform))))
	   (when (not (eql lftype transformtype))
	     (debug-om 25 "      failed constraint ~S ~%" constraint)
	     (return-from constraints-satisfied nil))
	   ))		    
	(:lex-forms
	 (let ((lflex (extract-lflex lf)))
	   (when (not (member lflex (second constraint)))
	     (debug-om 25 "      failed constraint ~S~%" constraint)
	     (return-from constraints-satisfied nil))
	   ))	   
	;; all arguments must match the LF
	(:allobligatory
	 (let* ((termvar (get-termvar lf))
		(bindings (list (cons typevar termvar))))
	   ;; check each argument
	   (dolist (arg (lftransform-argtransforms transform))
	     (multiple-value-bind (match binds)
		 (is-argtransform-match bindings full-lf lf arg)
	       ;; return nil if the argument did not match
	       (if (null match) 
		   (progn ()
			  (debug-om 25 "      failed constraint ~S ~%" constraint)
			  (return-from constraints-satisfied nil))
		 (setf bindings (car binds)))))
	   ))
	;; Binding to a specified variable must match the specified constraint
	(:arg-lex-form
	 (let* ((var (first (second constraint)))
		(vals (second (second constraint)))
		(termvar (get-termvar lf))
		(bindings (list (cons typevar termvar)))
		(relevant-argtransforms (remove-if-not #'(lambda (x) (argtransform-contains-var x var)) (lftransform-argtransforms transform)))
		)
	   ;;	   (format t "checking arg-lex-form ~%")
	   (dolist (arg relevant-argtransforms)
	     (multiple-value-bind (match binds)
		 (is-argtransform-match bindings full-lf lf arg)
	       ;;  (format t "bindings are ~S ~%" binds)
	       (cond
		((null match)
		 (debug-om 25 "      failed constraint ~S ~%" constraint)
		 (return-from constraints-satisfied nil))
		((not (member (cdr (assoc var (car binds))) vals)) ;; some other arg form -- not the specified one
		 (debug-om 25 "      failed constraint ~S ~%" constraint)
		 (return-from constraints-satisfied nil))
		(t
		 (setf bindings (car binds))))
	       )
	     )
	   ))	   	   
	(t (signal-error "ERROR: bad constraint ~S in transform ~S~%"
			 constraint (lftransform-name transform)))))
    t))

(defun argtransform-contains-var (arg var)
  "A helper to constraints-satisfied. True if a given argtransform contains a given variable"
  (or (tree-find var (argtransform-context arg))
      (tree-find var (argtransform-content arg))
      (tree-find var (argtransform-kr arg)))
  )

(defun tree-find (x tree)
  "True is x is somewhere in a tree represented as a nested list"
  (cond ((null tree) nil)
	((eql x tree) t)
	((not (consp tree)) nil)
	(t (or (tree-find x (first tree)) (tree-find x (rest tree))))
	))
	  
(defun get-krtype (kr bindings defaults argument object-var arg-var)
  "Return the kr object type of an argument in the kr object object-var.
   arg-var is the variable pointing to the argument's object."
;  (declare (ignore bindings)) ;; for now
  (let ((main-object (get-kr-object object-var kr)))
    ;; if we have access to the KR, query it for the type
    (cond (*kr-available-for-mapping*
	   (if main-object
	       (query-kr (get-type main-object) argument)
	     nil))
	  ;; else check if the defaults give the kr type
	  (t (let ((default (assoc arg-var defaults)))
	       (cond (default
		       (debug-om 25 "default is ~S~%" default)
		       (if (listp (second default)) ;; handle (:lex-kr ?lex) as new kr type
			   (let* ((default-var (second (second default)))
				  (dv-binding (is-bound default-var bindings)))
			     (when dv-binding (list (first default) dv-binding)))
			     (second default)))
		     (t (signal-error "ERROR: No kr-type given for variable ~S~%" arg-var)
			nil)))))))

(defun merge-bindings (binds1 binds2)
  "Return the merged list of two variable bindings.  If the same variable
   appears in both, it should not have different values.  An error message is
   printed if they conflict, but the conflict is then ignored."
  (cond ((null binds1) binds2)
	(t (let* ((x (first binds1))
		  (m (assoc (first x) binds2)))
	     (cond (m
		    (if (not (equal (cdr x) (cdr m)))
			(signal-error "ERROR: merge-bindings: variable is bound to conflicting values: ~S ~S" x m))
		    (merge-bindings (rest binds1) binds2))
		   (t (cons x (merge-bindings (rest binds1) binds2))))))))

(defun add-transform-in-order (transform transforms)
  "@desc Adds a transform to a list of transforms, in order of specificity.
         If two are equal in specificity, then the order in the rules.lisp
         file determines which is listed first.
   @param transform A lftransform structure.
   @param transforms A list of transforms, in order of most specific to most
                     general."
  (debug-om 30 "add-transform-in-order top~%")
  (cond ((null transforms) (list transform))
	(t 
	 (cond
	  ;; the transform is more specific than the head of the sublist
	  ((rule-more-specific transform (first transforms))
	   (debug-om 30 "Transform ~S more specific than head of the list ~S~%" transform (first transforms))
	   (cons transform transforms)
	   )
	  ;; the head of a transforms is more specific or equal
	  ((rule-more-specific-or-equal (first transforms) transform)
	   (debug-om 30 "Transform ~S is less specific or equal than head of the list ~S~%" transform (first transforms))
	   (let ((sublist nil)
		 (added-flag nil))
	     (dolist (form transforms)
	       ;; when we find the transform's spot
	       (when (rule-more-specific transform form)
		 (setf sublist (cons transform sublist))
		 (setf added-flag t))
	       (setf sublist (cons form sublist)))
	     ;; add the transform to the end if it hasn't been added
	     (if (null added-flag)
		 (setf sublist (cons transform sublist)))
	     ;; reverse the list
	     (reverse sublist)))
	  ;; return recursively
	  (t (format t "ERROR: om transform rules not related~%")))
	 )))

;; This is the old one which maintained a list of lists...
;; Each list had a specificity order to it, but some rules may not be related
;; at all, so there was a different list for each one.
;; -- The above function assumes all rules that match an LF term are related.
;;;#|
;;;(defun add-transform-in-order (transform transforms)
;;;  "@desc Adds a transform to a list of transform lists, each list is in order
;;;         of specificity.  If two are equal in specificity, then the order in
;;;         the rules.lisp file determines who is listed first.
;;;   @param transform A lftransform structure.
;;;   @param transforms A list of lists of transforms.  Each sublist is a list
;;;          of transforms that are all subtypes of each other, in order of most
;;;          specific to most general."
;;;  (debug-om 30 "add-transform-in-order top~%")
;;;  (cond ((null transforms) (list (list transform)))
;;;	(t (let ((sublist (first transforms)))
;;;	     (debug-om 30 "add-transform-in-order cond top sublist=~S~%" sublist)
;;;	     (cond
;;;	      ;; the transform is more specific than the head of the sublist
;;;	      ((rule-more-specific transform (first sublist))
;;;	       (debug-om 30 "Transform ~S more specific than head if the list ~S~%" transform (first sublist))
;;;	       (cons (cons transform sublist) (rest transforms))
;;;	       )
;;;	      ;; the head of a sublist is more specific or equal
;;;	      ((rule-more-specific-or-equal (first sublist) transform)
;;;	       (debug-om 30 "Transform ~S is less specific or equal than head of the list ~S~%" transform (first sublist))
;;;	       (let ((new-sublist nil)
;;;		     (added-flag nil))
;;;		 (dolist (form sublist)
;;;		   ;; when we find the transform's spot
;;;		   (when (rule-more-specific transform form)
;;;		     (setf new-sublist (cons transform new-sublist))
;;;		     (setf added-flag t))
;;;		   (setf new-sublist (cons form new-sublist)))
;;;		 ;; add the transform to the end if it hasn't been added
;;;		 (if (null added-flag)
;;;		     (setf new-sublist (cons transform new-sublist)))
;;;		 ;; reverse the list
;;;		 (cons (reverse new-sublist) (rest transforms))))
;;;	      ;; return recursively
;;;	      (t (cons sublist
;;;		       (add-transform-in-order transform (rest transforms)))))
;;;	     ))))
;;;|#

(defun rule-more-specific-or-equal (one two)
  (or (rule-equal one two) (rule-more-specific one two)))

(defun rule-equal (rule-one rule-two)
  "Returns true if the LFs of 'one' and 'two' are equal
   @param one A lftransform structure
   @param two A lftransform structure"
  (let ((one (lftransform-typetransform rule-one))
	(two (lftransform-typetransform rule-two)))
    (cond
     ;; (1) equal LF types and (2a) both have lex or (2b) neither have lex
     ((and (eq (typetransform-lftype one) (typetransform-lftype two))
	   (or (and (typetransform-lflex one) (typetransform-lflex two))
	       (and (null (typetransform-lflex one))
		    (null (typetransform-lflex two)))))
      t)
     (t nil))))

(defun rule-more-specific (rule-one rule-two)
  "Returns true if LF 'one' is more specific than LF 'two', taken from the
   left hand side of the :typetransform field in the rules.
   @param one A lftransform structure
   @param two A lftransform structure"
  (let ((one (lftransform-typetransform rule-one))
	(two (lftransform-typetransform rule-two)))
    (cond
     ;; equal LF types
     ((eq (typetransform-lftype one) (typetransform-lftype two))
      ;; one has a lex, two does not
      (cond 
       ((null (typetransform-lflex one))
	;; If the types are equal, but one does not have a var, it cannot be more specific
	nil)
       ((null (typetransform-lflex two))
	;; after this point one definitely has a lflex
	;; so if two doesn't, one is more specific
	t)
       ((and (not (is-transform-var (typetransform-lflex one)))
	     (and (is-transform-var (typetransform-lflex two))
		  (not (assoc :lex-forms (lftransform-constraints rule-two)))))
	t ;; if one does not have a lexical variable, but two does, then two is more specific
	)
      (t nil)
       ))	     
     ;; LF type of one is more specific
     ((lf-subtype (typetransform-lftype one) (typetransform-lftype two))
      t)
     ;; LF type is not equal or a subtype
     (t nil))))

;; **NOTE: nobody calls this function anymore
(defun find-matching-lfterm (bindings lf pattern)
  "@param lf A list of lf terms
   @param pattern An argument pattern from a transform rule - (F ?v ONT::type)
   @returns t|nil if a match was found and the variable bindings list"
  (dolist (term lf nil)
    (multiple-value-bind (match bind)
	(unify-term-pattern bindings term pattern)
      ;; found a matching term
      (if match (return (values t bind))))))

(defun find-matching-lfterms (bindings lf pattern)
  "@param lf A list of lf terms
   @param pattern An argument pattern from a transform rule - (F ?v ONT::type)
   @returns A list of variable bindings lists, one for each match"
  (let ((newbinds '(((t . t))))
	(base-pattern (subseq pattern 0 3))
	(feat-pattern (subseq pattern 3))
	matches rolebinds)
    (dolist (term lf)
      (multiple-value-bind (match bind)
	  (unify-term-pattern bindings term base-pattern)
	;; found a matching term for the base pattern, with no attributes
	(when match
	  ;; now find all role matches
	  (when feat-pattern
	    (setq rolebinds nil) ;; reset!
	    (dolist (pair (make-pairs feat-pattern))
	      (setq rolebinds (cons (find-matching-lfroles bindings term pair) rolebinds)))
	    ;; nil if one of the LHS does not unify
	    (setf newbinds (if (member nil rolebinds) nil
			     (find-unified-matches rolebinds))))
	  ;; we matched all the arguments
	  (when newbinds
	    (setf bind (cons (list '?*term* (get-termvar term)) bind))
	    (setf matches (mapcar #'(lambda (x) (merge-bindings bind x)) newbinds)))
	  )))
    matches))

(defun find-matching-lfterms-orig (bindings lf pattern)
  "@param lf A list of lf terms
   @param pattern An argument pattern from a transform rule - (F ?v ONT::type)
   @returns A list of variable bindings lists, one for each match"
  (let (matches)
    (dolist (term lf)
      (multiple-value-bind (match bind)
	  (unify-term-pattern bindings term pattern)
	;; found a matching term
	(when match
	  (setf bind (cons (list '?*term* (get-termvar term)) bind))
	  (setf matches (cons bind matches))
	  )))
    matches))

(defun find-matching-lfroles (bindings lfterm pattern)
  "@param lf An LF term
   @param pattern An argument pattern from a transform rule - (:theme ?t)
   @returns A list of variable bindings lists, one for each match"
  (let ((matches (get-all-role-values lfterm (first pattern)))
	(bound (is-bound (second pattern) bindings)))
    
    (cond ((is-transform-var (second pattern))
	   (cond ((null bound) ;; variable, but not yet bound
		  (mapcar (lambda (x)
			    (cons (cons (second pattern) x) bindings))
			  matches))
		 ;; variable already bound, don't look for more bindings
		 ((member bound matches)
;		  (list (cons (cons (second pattern) bound) bindings)))
		  (list bindings))
		 (t nil))
	   )
	  ;; if the two are equal, not variables
	  (t (if (member (second pattern) matches :test #'equal)  ;; could be lists
		 (list bindings) nil)))
    )
  )
  

(defun unused-terms (terms used-vars)
  "@param terms A list of LF terms
   @param used-vars A list of variables  i.e. (v123 v931 v14)
   @returns All terms that are not in the given list of used variables."
  (cond ((null terms) nil)
	((atom terms) nil)
	(t
	 (if (member (get-termvar (first terms)) used-vars)
	     (unused-terms (rest terms) used-vars)
	   (cons (first terms) (unused-terms (rest terms) used-vars))))))

(defun get-new-bindings (binds1 binds2)
  "@returns The variable cons' in binds2 that aren't in binds1"
  (let ((vars (mapcar #'car binds1))
	diff)
    (dolist (bind binds2)
      (if (not (member (car bind) vars))
	  (setq diff (cons bind diff))))
    diff))

(defun is-bound (var bindings)
  (let ((bound (assoc var bindings)))
    (if bound (cdr bound) nil)))

(defun remove-binding (var bindings)
  (cond ((null bindings) nil)
	(t (if (eq var (first (first bindings))) (rest bindings)
	     (cons (first bindings)
		   (remove-binding var (rest bindings)))))))
  
(defun get-kr-object (var kr)
  (let ((bound (assoc var kr)))
    (if bound (second bound) nil)))

(defun get-type (kr-lambda)
  "@param kr-lambda A kr expression: (lambda var (ONT::INSTANCE-OF var TYPE1)...)
   @returns The type of the expression: TYPE1"
  (let ((type (if (valid-kr-object kr-lambda)
		  (assoc 'ONT::INSTANCE-OF (rest (rest kr-lambda))) nil)))
    (if type (third type) nil)))

(defun is-pred-present (pred object)
  "@param pred A KR type
   @param object A KR object we are building
   @returns The predicate pred list inside object if there is one."
  (if (valid-kr-object object)
      (assoc pred (rest (rest object))) nil))

(defun valid-kr-object (object)
  (if (and (listp object) (eq 'lambda (first object))) t nil))

(defun get-default (var transform)
  "@returns The default value for a variable as specified in the transform."
  (let* ((defaults (lftransform-defaults transform))
	 (default (assoc var defaults)))
    (if default (second default) nil)))

(defun get-lf-atom-map (lex &key no-warning reverse)
  (if reverse
      ;; reverse the function
      (let ((rev (reverse-map lex)))
	(if rev (list rev) nil))
    ;; *** normal processing ***
    (let ((result (assoc lex *lf-atom-map* :test (lambda (x y) (equal x y)))))
      (if (null result)
	  (if (not no-warning)
	      (debug-om 10 "OM: WARNING no mapping for term ~S~%" lex))
	(setf result (second result)))
      result)))



;; ----------------------------------------
(defun transform-lftype (type transforms)
  "@param type An lf type, :* list, or any atom that is mapped in lf-atom-map
   @param transforms A triple (<list of normal lftransforms> <list of abstracts> <list of defaults>)
   @returns A single kr transformation
   @desc This function is used when people need to transform single LF objects
         and not entire LF terms or lists of terms."
  (or (get-lf-atom-map type :no-warning t) ;; try the direct map first
      (let ((fulls (match-transforms `((ont::a v1 ,type)) `(ont::a v1 ,type)
				     (append (car transforms) (third transforms))))
	    lf kr func bindings temp)
	(if fulls
	;; try the matched transform rules
	(dolist (transform fulls)
	  ;; get the rule's kr type
	  (setq lf (lftransform-typetransform transform))
	  (setq kr (typetransform-kr lf))

	  ;; bind the lexical variable if it exists
	  (if (and (is-transform-var (typetransform-lflex lf))
		   (listp type))
	      (setq bindings (list (cons (typetransform-lflex lf) (third type)))))

	  ;; if the kr is a variable, and if the lexical item is bound
	  (cond ((and bindings (is-transform-var kr))
		 (when (setq func (second (assoc kr (lftransform-functions transform))))
		   (setq bindings (apply-function func kr bindings
						  (lftransform-functions transform)))
		   (setq temp (simplekb::lookup kr bindings))
		   (if temp (return temp)))
		 )
		;; kr variable, but no matching lexical input to bind
		((is-transform-var kr) nil)
		;; normal KR, return it
		(t (return kr)))
	  )
	type)
	)))
