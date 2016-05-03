;;;; load.lisp - stuff to use for reading lexicon data; mostly macros

(in-package :dsl)

;;; lexical context

(defvar *concept-stack* nil)
(defun current-concept () (car *concept-stack*))
(defvar *current-provenance* nil)
(defvar *current-input-text* nil) ; not really used but set by non-concept-class
(defvar *current-word* nil)
(defvar *current-pos* nil)
(defvar *current-morph* nil)

(defvar *unknown-templates* nil)

(defmacro with-clean-load-context (&body body)
  `(let (*concept-stack*
         *current-word*
	 *current-pos*
	 *current-morph*)
    ,@body))

(defun load-dsl-file (filename &key provenance-name)
  (let (
        ;; reset lexical context
        *concept-stack*
        (*current-provenance*
	  (when provenance-name
	    (make-instance 'provenance
	        :name provenance-name :filename (namestring filename))))
	*current-input-text*
	*current-word*
	*current-pos*
	*current-morph*
	;; make sure we read in the LD package
        (*package* (find-package :lexicon-data))
	)
;;  (load filename)
;;  FIXME temporary workaround for large numbers of undefined templates being used in :ont-w resource
  (handler-case (load filename)
    (undefined-function (c)
      (let ((name (cell-error-name c)))
	(cond
	  ((template-name-p name)
	    (warn "unknown template ~s; aborting load of ~a" name filename)
	    (pushnew name *unknown-templates*)
	    )
	  (t
	    (warn "called undefined function ~s; aborting load of ~a" name filename)
	    )
	  )))
    )))

(defun template-name-p (sym)
  (let* ((ont-pkg (find-package :ont))
         (sym-name (symbol-name sym))
	 (len (length sym-name)))
    (and (eq ont-pkg (symbol-package sym))
	 (< 6 len)
	 (string= "-TEMPL" (subseq sym-name (- len 6)))
	 ))) 

;;; macro helper functions

(defun ld-to-dsl-package (s)
  "If s is a symbol in the lexicon-data package, convert it to the dsl package."
  (if (and (symbolp s) (eql (symbol-package s) (find-package :lexicon-data)))
    (intern (symbol-name s) :dsl)
    s))

(defun adjust-feature-packages (f)
  "Convert feature names and values to dsl package, and ORs to W package."
  (list (ld-to-dsl-package (first f))
	(cond
	  ((symbolp (second f))
	    (intern (symbol-name (second f)) :dsl))
	  ((and (consp (second f))
		(every #'symbolp (second f))
		(string= "OR" (symbol-name (first (second f)))))
	    (cons 'w::or
		(mapcar
		    (lambda (s)
		      (intern (symbol-name s) :dsl))
		    (cdr (second f))
		    )))
	  (t (error "bogus feature: ~s" f))
	  )
	))

(defun concept-formula (x)
  "Convert symbols representing concepts in a concept formula like (w::and
   (w::or VN::foo WN::bar) FN::baz (concept PB::glarch ...)). Only recurses on
   ands and ors. Also wrap all of x in a let resetting certain lexical context."
  `(with-clean-load-context
    ,(cond
      ((and (consp x) (member (util::convert-to-package (car x) :dsl) '(and or)))
	(cons (car x) (mapcar #'concept-formula (cdr x))))
      ((symbolp x) `(get-or-make-concept ',x))
      (t x)
      )))

(defmacro operator-cond ((op-var form-var body-forms &key (convert-operator-package t)) &body cases)
  "For each of the body-forms, if the operator satisfies one of the cases'
   tests, use the body of that case to convert body-form to an expression to
   evaluate. Otherwise keep the body-form unchanged."
  `(mapcar
       (lambda (,form-var)
	 (let ((,op-var
	 	  (when (consp ,form-var)
	 	    ,(if convert-operator-package
	              `(ld-to-dsl-package (car ,form-var))
		      `(car ,form-var)
		      ))))
	   (cond
	     ,@cases
	     (t ,form-var)
	     )))
       ,body-forms))

(defun non-concept-class (cls body)
  "Return code to be evaluated to instantiate a non-concept class cls with the
   given body forms. Forms starting with a slot name set the slot to the second
   value in the form, while others are left to be evaluated normally (with
   *current-<cls>* set to the new instance)."
  (let ((slots (class-slot-names cls))
	(current-var (intern (concatenate 'string "*CURRENT-" (symbol-name cls) "*") :dsl)))
    `(let ((new-ncc (make-instance ',cls)))
      ,(unless (eq 'provenance cls)
        `(when (and (current-concept) (slot-exists-p (current-concept) ',cls))
	  (setf (slot-value (current-concept) ',cls) ,current-var)))
      (setf ,current-var new-ncc)
      ,@(operator-cond (operator form body)
	((and (not (and (eq operator 'provenance)
			(eq cls 'provenance))) ; FIXME ick.
	   (member operator slots))
	;; FIXME maybe the rule should be if the operator names a slot and /is not fboundp/, we just set the slot; otherwise the macro/function should handle setting the slot if it needs to
	;; need to be careful to test fboundp for the operator in the original package; slots are fboundp in :dsl package because of accessors
	  `(setf (slot-value ,current-var ',operator)
	         ',(ld-to-dsl-package (second form))))
	)
      ,current-var)))

(defun relation (label targets)
  "Return code to be evaluated to make relations with the given label from the
   current concept to the targets."
  `(let ((*current-provenance* *current-provenance*))
    ,@(mapcar
	(lambda (target)
	  (cond
	    ((and (listp target) (eq 'ld::provenance (car target)))
	      target)
	    (t
	      `(add-relation (current-concept) ',label ,(concept-formula target)
			     *current-provenance*))
	    ))
	targets)
    ))

(defun trips-sense-name-p (x)
  (and (listp x)
       (= 3 (length x))
       (eq :* (first x))
       (symbolp (second x))
       (symbolp (third x))
       (eq (symbol-package (second x)) (find-package :ont))
       (eq (symbol-package (third x)) (find-package :w))
       ))

(defun trips-sense-name-to-symbol (x)
  (intern (concatenate 'string (symbol-name (second x)) "*" (symbol-name (third x))) :ont))

(defun optionally-named-concept-subtype (concept-type name-and-body)
  "Return code to be evaluated to instantiate or add to a concept class with
   the given body forms (the first of which may be a name). This handles
   binding the lexical context variables, instantiating the concept if
   necessary, and adding inheritance relations implicit in the lexical
   nesting. Callers should handle transforming any body forms that aren't meant
   to be evaluated as-is before calling this function."
  (let (name (body name-and-body))
    (cond
      ((symbolp (car body))
        (setf name (pop body)))
      ((trips-sense-name-p (car body))
        (setf name (trips-sense-name-to-symbol (pop body))))
      )
    `(let* (
	    ;; bind the lexical context variables so they get reset when we
	    ;; leave this let
	    (*concept-stack*		*concept-stack*)
            (*current-provenance*	*current-provenance*)
	    (*current-input-text*	*current-input-text*)
            (*current-word*		*current-word*)
            (*current-pos*		*current-pos*)
	    (*current-morph*		*current-morph*)

            (anonymous ,(null name))
	    (outer-type (type-of (current-concept)))
	    (inner-part-of-outer (concept-part-of-p ',concept-type outer-type))
	    (outer-part-of-inner (concept-part-of-p outer-type ',concept-type))
	    )
      ;; From email "deep semantic lexicon model and file format" 2013-06-07:
      ;; Nested instantiations can have one of two meanings. If the inner class
      ;; is part of the outer one, it is taken to define that part of the outer
      ;; class (e.g. in (sense (sem-frame ...)), the sem-frame is part of the
      ;; sense). If the outer class is part of or the same as the inner one,
      ;; the inner instance gets the properties of the outer one (e.g. in (word
      ;; foo (morph ...)), the morph gets "foo" as the base form). This happens
      ;; via an inheritance relation if they're both concepts.
      (cond
	((not anonymous)
	  ;; separate the inner named concept from its outer concept, and link
	  ;; them with an inheritance relation
	  (push (get-or-make-concept ',name ',concept-type)
		*concept-stack*)
	  (cond
	    ((null (second *concept-stack*))
	      nil)
	    (outer-part-of-inner
	      (add-relation 
		  (current-concept) :inherit (second *concept-stack*)
		  *current-provenance*
		  ))
	    (inner-part-of-outer
	      (add-relation 
		  (second *concept-stack*) :inherit (current-concept)
		  *current-provenance*
		  ))
	    )
	  )
	((null *concept-stack*)
	  ;; no outer concept, just make the (anonymous) inner one
	  (push (make-instance ',concept-type) *concept-stack*)
	  )
	(outer-part-of-inner
	  ;; outer concept is same type or part of anonymous inner concept,
	  ;; inner is separate from but inherits outer
	  (push (make-instance ',concept-type) *concept-stack*)
	  (add-relation
	      (current-concept) :inherit (second *concept-stack*)
	      *current-provenance*
	      )
	  )
	(inner-part-of-outer
	  ;; anonymous inner concept is strictly part of outer concept,
	  ;; get or make the part of outer that inner is, and put it on the
	  ;; stack
	  (push (get-or-make-part-of ',concept-type (current-concept))
	        *concept-stack*))
	(t
	  (error "incompatible nested concept types: ~s inside ~s" ',concept-type outer-type))
	)
      (unless (null *current-provenance*)
        (pushnew *current-provenance* (provenance (current-concept))
	         :test #'provenance-equalp))
      ;; mostly just keep body the way it is, but special case OR and
      ;; provenance forms
      ,@(operator-cond (operator form body)
	    ((eq 'w::or operator)
	      `(add-relation (current-concept) :inherit
		   (with-clean-load-context ,form)))
	    ((eq 'provenance operator)
	       `(pushnew ,form (provenance (current-concept))
	                 :test #'provenance-equalp))
	    )
      (current-concept))
    ))

(defun make-word-from-spec (spec)
  "Make an instance of the word class from a specification like the
  following:
    singleword
    still-a-single-word
    (multiple contiguous words)
    (word with (particle))
  "
  (setf spec (convert-to-package spec :w))
  (cond
    ((symbolp spec)
      (make-instance 'word
          :first-word spec))
    ((not (listp spec))
      (error "bogus word spec; expected symbol or list, but got: ~s" spec))
    ((every #'symbolp spec)
      (make-instance 'word
          :first-word (car spec)
	  :remaining-words (cdr spec)
	  ))
    ((and (every #'symbolp (butlast spec))
          (typep (car (last spec)) '(cons symbol null)))
      (make-instance 'word
          :first-word (car spec)
	  :remaining-words (butlast (cdr spec))
	  :particle (caar (last spec))
	  ))
    (t
      (error "bogus word spec; expected list of symbols with possible final list of one particle symbol, but got: ~s" spec))
    ))

(defun current-morph (&key (add-maps t))
  "Get the current morph, look up the default, make one, or return nil, as
   appropriate."
  (cond
    (*current-morph*
      ;; we have a morph already, make it more complete if possible
      (when (and *current-pos*
                 (not (slot-boundp *current-morph* 'pos)))
        (setf (pos *current-morph*) *current-pos*))
      (when (and *current-word*
      		 add-maps
                 (slot-boundp *current-morph* 'pos)
		 (null (maps *current-morph*)))
        (add-morph-maps-for-word *current-morph* *current-word*))
      *current-morph*)
    ((and (null *current-morph*) *current-word* *current-pos*)
      ;; we don't have a current morph, but we do have enough information for a
      ;; complete default one
      (let* ((default-morphs (gethash *current-word* (morphs *db*)))
             (default-for-pos (find *current-pos* default-morphs :key #'pos)))
        (setf *current-morph*
	  (if default-for-pos
	    ;; we have a default one stored, just get it
	    default-for-pos
	    ;; store a new default morph for this word/pos
	    (let ((new-morph (make-instance 'morph :pos *current-pos*)))
	      (when add-maps
	        (add-morph-maps-for-word new-morph *current-word*))
	      (push new-morph (gethash *current-word* (morphs *db*)))
	      new-morph
	      )
	    ))))
    (t
      ;; we don't have a current morph or enough information to make/get a
      ;; default one, just return nil
      nil)
    ))

;;; constructor/reference macros

(defmacro ld::alias (&rest names)
  `(progn
    (setf (aliases (current-concept)) (union (aliases (current-concept)) ',names))
    ;; FIXME this isn't the right way to do this, but it works in some
    ;; important cases; really need to merge concepts... see
    ;; make-db.lisp:get-or-make-concept
    (dolist (name ',names)
      (setf (gethash name (concepts *db*)) (current-concept)))
    ))

;; oops, some confusion about the plurality of this slot...
(defmacro ld::aliases (&rest names) `(ld::alias ,@names))

(defmacro ld::provenance (&body body)
  ;; convert initial symbol to `(name ,symbol)
  (when (symbolp (car body))
    (setf body
          (cons (list 'lexicon-data::name
	              (intern (symbol-name (car body)) :dsl))
	        (cdr body))
	  ))
  (non-concept-class 'provenance
      (operator-cond (operator form body)
	((eq 'provenance operator)
	  `(push (let (*current-provenance*) ,form)
		 (provenance *current-provenance*)
		 ))
	))
  )

(defmacro ld::definition (&body body)
  `(let ((*current-provenance* *current-provenance*))
     ,(non-concept-class 'input-text body)
     (unless (slot-value *current-input-text* 'provenance)
       (setf (provenance *current-input-text*) *current-provenance*))
     (push *current-input-text* (definitions (current-concept)))
     *current-input-text*))

(defmacro ld::example (&body body)
  `(let ((*current-provenance* *current-provenance*))
     ,(non-concept-class 'input-text body)
     (unless (slot-value *current-input-text* 'provenance)
       (setf (provenance *current-input-text*) *current-provenance*))
     (push *current-input-text* (examples (current-concept)))
     *current-input-text*))

(defmacro ld::concept (&body body)
  (optionally-named-concept-subtype 'concept body))

(defmacro ld::sem-frame (&body body)
  (optionally-named-concept-subtype 'sem-frame
      (operator-cond (operator form body)
        ((typep operator '(or sem-role (and cons (list-of sem-role))))
	  (destructuring-bind (roles restriction &optional optional) form
	    `(let* ((restr ,(concept-formula restriction))
	            (rrmap
		      (make-instance 'role-restr-map
			  :roles ',(ld-to-dsl-package (if (listp roles) roles (list roles)))
			  :restriction restr
			  :optional ,(not (null optional))
			  )))
	      (if (consp restr)
	        (add-references-from-concept-formula restr)
		(push rrmap (references restr))
		)
	      (push rrmap (maps (current-concept)))
	      )))
	)
      ))

(defmacro ld::sem-feats (&body body)
  (optionally-named-concept-subtype 'sem-feats
      (operator-cond (operator form body)
        ((typep operator 'sem-feat)
	  `(push ',(adjust-feature-packages form) (features (current-concept)))
	  ))))

(defmacro ld::entailments (&body body)
  (optionally-named-concept-subtype 'entailments
      (mapcar
          (lambda (form)
	    (if (stringp form)
	      `(push ,form (rules (current-concept)))
	      form))
	  body)))

(defmacro ld::semantics (&body body)
  (optionally-named-concept-subtype 'semantics body))

(defun separate-head-word-from-syn-cat (syn-cat)
  "Given a syn-cat that might have a head-word attached to it, return two
   values: the syn-cat without the head-word attached, and the head-word or
   nil."
  ;; FIXME what to do when syn-cat is actually completely
  ;; unspecified, e.g.:
  ;;   (OPERATOR-TEMPL
  ;;    (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
  ;;     (ARGUMENTS
  ;;      (ARGUMENT (% ?argcat) ONT::OF)
  ;;      ))
  ;; answer: comment it out :-P
  (cond
    ;; just the syn-cat
    ((symbolp syn-cat)
      (values (util::convert-to-package syn-cat :dsl) nil))
    ;; work around the case where syn-cat is a disjunction,
    ;; e.g. (or advbl adjp)
    ((and (consp syn-cat)
	  (symbolp (car syn-cat))
	  (string= "OR" (symbol-name (car syn-cat))))
      (values
	    (cons 'w::or
		   (util::convert-to-package (cdr syn-cat) :dsl))
	    nil))
    ;; (syn-cat head-word)
    ((and (listp syn-cat) (= 2 (length syn-cat)))
      (values (util::convert-to-package (first syn-cat) :dsl)
	      (util::convert-to-package (second syn-cat) :w)
	      ))
    (t (error "bogus syn-cat: ~s" syn-cat))
    ))

(defmacro ld::syn-sem (&body body)
  (optionally-named-concept-subtype 'syn-sem
      (operator-cond (operator form body)
        ((typep operator 'syn-arg)
	  (destructuring-bind (syn-arg syn-cat &optional sem-role optional) form
	    (multiple-value-bind (dsl-syn-cat head-word)
	        (separate-head-word-from-syn-cat syn-cat)
	      `(push
		  (make-instance 'syn-sem-map
		      :syn-arg ',(util::convert-to-package syn-arg :dsl)
		      :syn-cat ',dsl-syn-cat
		      :head-word ',head-word
		      :sem-role ',(ld-to-dsl-package sem-role)
		      :optional ,(not (null optional))
		      )
		  (maps (current-concept))
		  )
	      )))
	)))

(defmacro ld::syn-feats (&body body)
  (optionally-named-concept-subtype 'syn-feats
      (operator-cond (operator form body)
        ((typep operator 'syn-feat)
	  `(push ',(adjust-feature-packages form) (features (current-concept)))
	  ))))

(defmacro ld::template-call (call)
  (setf (template-call (current-concept)) call))

(defmacro ld::syntax (&body body)
  (optionally-named-concept-subtype 'syntax body))

(defmacro ld::word (word-spec &body body)
  (if body
    `(let ((*current-word* (make-word-from-spec ',word-spec)))
      ,@body
      *current-word*)
    ;; no body, just set current
    `(setf *current-word* (make-word-from-spec ',word-spec))
    ))

(defmacro ld::pos (pos &body body)
  (if body
    `(let ((*current-pos* ',(ld-to-dsl-package pos)))
      ,@body
      *current-pos*)
    ;; no body, just set current
    `(setf *current-pos* ',(ld-to-dsl-package pos))
    ))

(defmacro ld::morph (&body body)
  (non-concept-class 'morph body))

(defmacro ld::forms (&rest forms)
  `(progn
    (setf (irregularities (current-morph :add-maps nil))
	  (append (irregularities *current-morph*) '((ld::forms ,@forms))))
    ;; TODO move this to a separate function in make-db.lisp
    ;; FIXME add implicit base form if *current-word* is set
    ;; maybe instead of :add-maps above, :add-non-base-maps or :add-regular-morph-maps...
    ;; not sure that's really necessary; at least in the TRIPS lexicon, all words with w::morph != nil also have :forms, and all :forms have a non-suffixed form
    ,@(mapcar
	(lambda (form)
	  (if (consp form)
	    ;; an irregular form
	    `(let* ((form-name ',(ld-to-dsl-package (first form)))
		    (morphed (make-word-from-spec ',(second form)))
		    (pos (if (eq 'nom form-name)
		           'N
			   (pos *current-morph*)
			   ))
		    (feature-query `((pos ,pos)
				     (form ,form-name)))
		    (syn-feats (first
		      (find-if
			  (lambda (p)
			    (null (first (nth-value 2
			      (unify-feats feature-query
			                   (features (first p)))))))
			  *syn-feats-to-suffix*)))
		    (_ (when (null syn-feats)
		         (error "no syn-feats found for feature-query ~s" feature-query)))
		    (morph-map
		       (make-instance 'morph-map
			   :syn-feats syn-feats
			   :morphed morphed
			   ))
		    )
	        (declare (ignore _))
	      (push morph-map (maps *current-morph*)))
	    ;; shorthand for a set of regular forms
	    `(dolist (form-name 
	              ',(ecase (ld-to-dsl-package form)
			(-er '(none er est))
			(-ly '(none ly))
			(|-S-3P| '(sing plur))
			(-vb '(|12S123PBASE| |3S| ing past pastpart nom))
			(-none '(none sing))
			))
	      (dolist (sf-suff *syn-feats-to-suffix*)
		(destructuring-bind (sf . suffix) sf-suff
		  (let ((feature-query `((pos ,(pos *current-morph*))
		                         (form ,form-name))))
		    (multiple-value-bind (unified-feats bindingses remainders)
		        (unify-feats feature-query (features sf))
			(declare (ignore unified-feats bindingses))
		      (unless (first remainders)
			(push
			    (make-instance 'morph-map
				:syn-feats sf
				:morphed
				  (add-suffix-to-word *current-word* suffix)
				)
			    (maps *current-morph*)
			    )))))))
	    ))
	forms)))

(defmacro ld::sense (&body body)
  (optionally-named-concept-subtype 'sense `(
    ,@body
    ;; add the current morph if the sense doesn't already have one
    (when (and (not (slot-boundp (current-concept) 'morph)) (current-morph))
      (setf (morph (current-concept)) *current-morph*))
    ;; if the sense now has a morph, add it to (senses *db*)
    (when (slot-boundp (current-concept) 'morph)
      (add-morphed-sense-to-db *db* (current-concept)))
    )))

;;; relation macros

(defmacro ld::inherit (&body body) (relation :inherit body))
(defmacro ld::overlap (&body body) (relation :overlap body))
(defmacro ld::subtype-of (&body body) (relation :subtype-of body))
(defmacro ld::> (label &body body) (relation label body))

;;; boolean formula literals

(defmacro w::and (&body body)
  `(list 'w::and ,@body))

(defmacro w::or (&body body)
  `(list 'w::or ,@body))

(import '(w::and w::or) :lexicon-data)

