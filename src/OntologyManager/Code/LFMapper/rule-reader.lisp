; Nate Chambers
;;
;; This code creates transform structures from define-transform definitions.
;; It checks heavily for formatting mistakes in the definitions.
;;

(in-package "ONTOLOGYMANAGER")

;; make *lftransforms* global
(declaim (special *lftransforms*))
(unless (boundp '*lftransforms*)  (defvar *lftransforms* '(nil nil nil)))
;; *lftransforms* keeps transforms in three separate lists:
;; (first *lftransforms*) - all the 'normal' transforms
;; (second *lftransforms*) - all the abstract transforms
;; (third *lftransofrms*) - all the default-rule transforms


(defun clear-transforms ()
  "Remove all transforms from memory."
  (setf *lftransforms* (list nil nil nil)))
  
(defun add-lftransform (name &key abstract default-rule typevar typetransform
			     argtransforms constraints defaults functions)
  "Called with a full transform definition.  Creates a new lftransform
   structure and pushes it onto the global list of transforms."
  (let ((transform (make-lftransform
		       :name name
		       :abstract abstract
		       :default-rule default-rule
		       :typevar (create-typevar name typevar)
		       :typetransform (create-typetransform name typetransform)
		       :argtransforms (create-argument-transforms argtransforms)
		       :constraints (create-constraints constraints)
		       :defaults (create-defaults defaults)
		       :functions (create-functions functions)
		       )))
    ;; check that all LF types are in the ontology
    (check-lf-consistency transform)
    ;; save the transform
    (cond (abstract
	   (setf (second *lftransforms*)
		 (append (second *lftransforms*) (list transform))))
	  (default-rule
	   (setf (third *lftransforms*)
		 (append (third *lftransforms*) (list transform))))
	  (t
	   (setf (car *lftransforms*)
		 (append (car *lftransforms*) (list transform)))))))

(defun create-typevar (name typevar)
  "Checks that the typevar is in correct variable form."
  (if (null typevar)
      (signal-error "ERROR: no :typevar given in transform ~S~%" name))
  (if (is-transform-var typevar) typevar nil))

(defun create-typetransform (name type)
  "Creates a typetransform structure from a type definition in a transform."
  (cond ((and (listp type) (= 3 (length type)))
	 (let ((lf (first type))
	       (kr (third type)))
	   (cond ((and (listp lf) (= 3 (length lf)))
		  (make-typetransform
		   :lftype (second lf)
		   :lflex (third lf)
		   :kr kr))
		 ((and (atom lf) (atom kr))
		  (make-typetransform
		   :lftype lf
		   :lflex nil
		   :kr kr))
		 ;; error bad format
		 (t (signal-error "Incorrect LF or KR format in :type: ~S~%" type)
		    nil)
		 )))
	;; error bad format
	(t (signal-error "ERROR: bad :typetransform format in transform ~S~%  needed (<LF> -> <KR>)~%" name)
	   nil)))

(defun create-argument-transforms (args)
  "Return a list of argument structures."
  (let ((res nil) (new-arg nil))
    (dolist (arg args)
      (setq new-arg (create-argument-transform arg))
      ;; abort when an error in the format is found
      (when (null new-arg)
	(return-from create-argument-transforms nil))
      ;; push the new structure onto the list
      (push new-arg res))
    (reverse res)))

(defun create-argument-transform (arg)
  "Check the argument for valid formatting and create an argument
   transform structure if it is valid."
  (let ((pos (if (listp arg) (position '-> arg)))
	(cpos (if (listp arg) (position :context arg)))
	(kr (if (listp arg) (first (last arg)))))
    (if (null cpos) (setq cpos pos))
    
    (cond ((and (null kr) (not (null (first arg)))) ;; empty kr production
	   (make-argtransform
	    :content (subseq arg 0 cpos)
	    :context (if (/= pos cpos) (subseq arg (1+ cpos) pos))
	    :kr nil))
	  ((not (and (listp kr) (= 3 (length kr))))
	   (signal-error "Incorrect kr format in :argtransforms -> ~S~%  needed (PREDICATE VARIABLE VALUE)~%" kr)
	   nil)
	  ((null (first arg)) ;; empty content
	   (make-argtransform
	    :content nil
	    :context (if (/= pos cpos) (subseq arg (1+ cpos) pos))
	    :kr (first (last arg))))
	  (pos
	   (make-argtransform
	    :content (subseq arg 0 cpos)
	    :context (if (/= pos cpos) (subseq arg (1+ cpos) pos))
	    :kr (first (last arg))))
	  ;; error bad format
	  (t (signal-error "Incorrect :argtransforms format: ~S~%  needed ((<LF> -> <KR>)+)~%" arg)
	     nil))))

(defun create-constraints (constraints)
  "This is really just a check that the constraints are in the correct format.
   If so, the original list is returned, nil otherwise."
  (dolist (constraint constraints)
    (if (not (valid-constraint constraint :verbose t))
	(return-from create-constraints nil)))
  constraints)

(defun create-defaults (defaults)
  "Just checks if the defaults are in the right format.  Returns the original
   list if all is ok."
  (dolist (default defaults)
    (if (not (valid-default default :verbose t))
	(return-from create-defaults nil)))
  defaults)

(defun create-functions (functions)
  "Just checks if the functions are in the right format.  Returns the original
   list if all is ok."
  (dolist (func functions)
    (if (not (valid-function func :verbose t))
	(return-from create-functions nil)))
  functions)

(defun valid-constraint (constraint &key (verbose nil))
  "Returns non-nil if constraint is in the correct format, nil otherwise:
      e.g. (:obligatory :theme) | (:lf-type ?vv ONT::MOVE)"
  (cond ((listp constraint)
	 (case (first constraint)
	   ;; e.g. (:obligatory :theme)
	   (:obligatory (cond ((/= 2 (length constraint))
			       (signal-error "Incorrect :constraints format for :obligatory constraint: ~S~%" constraint)
			       nil)
			      (t constraint)))
	   ;; e.g. (:prohibit :theme)
	   (:prohibit (cond ((/= 2 (length constraint))
			     (signal-error "Incorrect :constraints format for :prohibit constraint: ~S~%" constraint)
			     nil)
			    (t constraint)))
	   ;; e.g. (:lf-type ?vv ONT::LAND-VEHICLE)
	   (:lf-type (cond ((or (/= 3 (length constraint))
				(not (is-transform-var (second constraint))))
			    (signal-error "Incorrect :constraints format for :lf-type constraint: ~S~%" constraint)
			    nil)
			   (t constraint)))
	   ;; e.g. (:allobligatory)
	   (:allobligatory (cond ((/= 1 (length constraint))
				  (signal-error "Incorrect :allobligatory format for :lf-type constraint: ~S~%" constraint)
				  nil)
				 (t constraint)))
	   ;; e.g. (:nonrecursive)
	   (:nonrecursive (cond ((/= 1 (length constraint))
				  (signal-error "Incorrect :nonrecursive format for :lf-type constraint: ~S~%" constraint)
				  nil)
				(t constraint)))
	   ;; e.g. (:lex-forms (w::between w::in-between))
	   (:lex-forms (cond ((or (/= 2 (length constraint))
				  (not (consp (second constraint))))
			      (signal-error "Incorrect :lex-forms format for :lf-type constraint: ~S~%" constraint)
			      nil)
			     (t constraint)))	   
	   ;; e.g. (:arg-lex-form (?wf (w::between w::in-between)))
	   (:arg-lex-form (cond ((or (/= 2 (length constraint))
				     (not (consp (second constraint)))
				     (not (consp (second (second constraint))))
				     )
				 (signal-error "Incorrect :arg-lex-form format for :lex-forms constraint: ~S~%" constraint)
				 nil)
				(t constraint))) 
	   ;; otherwise signal an error
	   (otherwise (signal-error "Incorrect :constraints format for unknown constraint type: ~S~%" (first constraint))
	     nil)))
	(t (if verbose (signal-error "Incorrect :constraints format, each must be in list form: ~S~%" constraint))
	   nil)))

(defun valid-default (default &key (verbose nil))
  "Return non-nil if default is the correct format:
     (?vv ONT::MOVE) | (*1 MOVE)
   Signal an error if verbose is true and there is a formatting problem."
  (cond ((and (listp default) (= 2 (length default)))
	 (cond ((not (is-transform-var (first default)))
		(if verbose (signal-error "Incorrect :defaults format, must start with a valid variable: ~S~%" default))
		nil)
	       (t default)))
	(t (if verbose (signal-error "Incorrect :defaults format: ~S~%" default))
	   nil)))

(defun valid-function (func &key (verbose nil))
  "Return non-nil if func is the correct format:
     (?vv (function args...))
   Signal an error if verbose is true and there is a formatting problem."
  (cond ((and (listp func) (= 2 (length func)))
	 (cond ((not (is-transform-var (first func)))
		(if verbose (signal-error "Incorrect :functions format, must start with a valid variable: ~S~%" func))
		nil)
	       ((atom (second func))
		(if verbose (signal-error "Incorrect :functions format, need a function with arguments specified: ~S~%" func)))
	       (t func)))
	(t (if verbose (signal-error "Incorrect :functions format: ~S~%" func))
	   nil)))

(defun check-lf-consistency (transform)
  "@param transform A fully built LFTransform structure
   @desc Checks that all LF types are still current in the LF ontology, and
         that any lexical items are still members of that LF"
  (let* ((typetran (lftransform-typetransform transform))
	 (type (typetransform-lftype typetran))
	 (lex  (typetransform-lflex typetran))
	 (argtrans (lftransform-argtransforms transform))
	 temp)
    ;; check the main lf 
    (check-lf-existence type lex)
    ;; check each argument
    (dolist (arg argtrans)
      (setq temp (append (argtransform-content arg) (argtransform-context arg)))
      ;; check each LHS of the argument
      (dolist (x temp)
	(cond ((keywordp (car x)) ;; keyword
	       (check-feature-validity type (car x)))
	      (t ;; full LF description
	       (if (listp (third x))
		   (check-lf-existence (second (third x)) (third (third x)))
		 (check-lf-existence (third x) nil))
	       ;; now check any features listed
	       (dolist (obj (cdddr x))
		 (if (keywordp obj)
		     (check-feature-validity (if (listp (third x)) (second (third x)) (third x)) obj)))
	       )))
      )
    ))

(defun check-lf-existence (lf lex)
  "@param lf An lf type
   @param lex A lexical item (i.e. w::move)
   @returns true if lf and lex are consistent with the lf ontology"
  (cond ((is-transform-var lf) t)
	((member lf '(ont::from-to ont::more-val ont::max-val ont::less-val ont::min-val ont::condition ont::ellipsis ont::s-conjoined
		      ont::time-loc ont::event-time-rel ont::quan-distance ont::set-of ont::time-range w::clause-condition)) ;; lfs defined in the grammar
	 t) 
	((defined-p lf)
	 (if (and (not (member lf '(ont::qmodifier))) ;; list of lfs to leave out
		  (not (check-lex-validity lf lex)))
	     (signal-error "WARNING: Invalid lexical item ~S for ~S~%" lex lf)
	   t))	
	(t
	 (signal-error "WARNING: Unknown LF type ~S~%" lf)
	 nil)))

(defun check-lex-validity (lf lex)
  "@param lf A valid lf type (assumed to be in the ontology)
   @param lex A lexical item
   @param true if lex is a member of lf in the lexicon"
  (let (defs thelf)
    ;; only run when the LxM package is defined
    (cond ((and (boundp 'lxm::*lexicon-data*) lex (not (is-transform-var lex)))
	   (setq defs (lxm::get-word-def (first-lex lex) nil))
	   (dolist (def defs)
	     (setq thelf nil)
	     ;; find the W::LF definition
	     (dolist (x (fourth def))
	       (when (and (listp x)
			  (eq 'w::lf (car x)))
		 (setq thelf x)
		 (return)))
	     ;; e.g. (w::lf (:* ont::disk-drive w::cd-burner))
	     (if (and (listp (second thelf)) ;; sometimes it is not a :* list
		      (eq lex (third (second thelf)))
		      (subtype (second (second thelf)) lf))
		 (return t)))
	   )
	  (t t))))

(defun check-feature-validity (lf feat)
  "@param lf An lf type (doesn't have to be valid, will return nil)
   @param feat A keyword feature such as :agent or :theme
   @returns true if the feature is a valid argument to the lf type"
  (let (found)
    (if (member feat '(:second :minute :hour :day :month :year :am-pm :sequence :from :to
		       :assoc-with :mods :of :val :value :is :quan :quantity :size
		       :name-of :suchthat :condition :content :context-rel :number :members :operator :equals
		       :intensity :orientation :unit :amount :assoc-poss :functn :figure :ground))
	(return-from check-feature-validity t))
    
    (dolist (arg (lf-arguments lf))
      (when (equal (symbol-name (second arg)) (symbol-name feat))
	(setq found t)
	(return)))
    (if (and (not found) (not (member lf '(ont::root ont::any-sem ont::in-relation ont::time-range
						       ont::location ont::coordinate ont::time-loc ;; x, y, coord1, coord2
						       ont::situation-root ont::qmodifier ont::frequency ont::s-conjoined ont::ellipsis))))
	(signal-error "WARNING: Invalid feature ~S for type ~S~%" feat lf))
    found))

(defun first-lex (lex)
  "@param lex A lexical item in the W package
   @returns If lex is a hyphenated word, it returns just the first word before
            the first hyphen...otherwise it returns the original lex."
  (let ((split (split-hyphens (string lex))))
    (cond ((= 1 (length split)) lex)
	  (t
	   (util::convert-to-package (car split) :W)))))
