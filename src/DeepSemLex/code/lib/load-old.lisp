(in-package :dsl)

;;; loading of old TRIPS lexicon/ontology
;; TODO make sure this actually treats things the same as old lex/ont, to the extent possible

;; Define in-package as a no-op so that we stay in the lexicon-data package
;; while loading the old TRIPS lex/ont. This way it won't interfere with
;; LexiconManager/OntologyManager loading the same files if DSL is in the same
;; Lisp instance as they are.
(defmacro ld::in-package (name)
    (declare (ignore name))
  nil)

(defun convert-variables-to-disjunctions (x)
  "Convert things of the form (? var opt1 opt2) to (w::or opt1 opt2),
   (? var foo) to just foo, and (? var) to t."
  (cond
    ((not (consp x))
      x)
    ;((not (member (car x) '(? ld::? om::? lxm::?)))
    ((not (util::is-variable-symbol (car x)))
      (mapcar #'convert-variables-to-disjunctions x))
    ((= 2 (length x))
      t)
    ((= 3 (length x))
      (third x))
    ((< 3 (length x))
      (cons 'w::or (cddr x)))
    (t (mapcar #'convert-variables-to-disjunctions x))
    ))

(defun convert-old-sem (old-sem &key part-p)
  "Convert an old sem feature list to an expression to evaluate to get a
   corresponding concept."
  (let* ((old-sem-disj
           (convert-variables-to-disjunctions
	       (util::convert-to-package old-sem :ld)))
         (old-fl-type (car old-sem-disj))
	 (old-fl (cdr old-sem-disj))
	 )
    ;; for now just flatten :required and :default
    (setf old-fl
          (mapcan
	      (lambda (x)
	        (if (member (car x) '(:required :default))
		  (cdr x)
		  (list x)
		  ))
	      old-fl))
    (cond
      ((and old-fl-type old-fl)
          `(ld::sem-feats (ld::inherit ,old-fl-type) ,@old-fl))
      (old-fl-type
        (if part-p
	  `(ld::inherit ,old-fl-type)
	  old-fl-type))
      (old-fl
        `(ld::sem-feats ,@old-fl))
      (t 'ld::t)
      )))

(defun convert-feature-value-hierarchy (tree)
  (mapcar
    (lambda (child)
      `(ld::concept ,(intern (symbol-name (car child)) :f) ; for f::+/-
        ,@(convert-feature-value-hierarchy (cdr child))))
    tree))

(defun prepend-any (feat-name)
  (intern (concatenate 'string "ANY-" (symbol-name feat-name)) :F))

(defmacro ld::define-feature (feat-name &key values name-only)
  `(ld::concept ,feat-name
    ;; for F::foo, make an alias F::ANY-foo
    (ld::alias ,(prepend-any feat-name))
    ;; check that values/name-only are specified correctly; turn values tree
    ;; into a hierarchy of concepts named by the feature values
    ,@(cond
        ((and values name-only)
	  (error "expected feature ~s definition to have either :values or :name-only, but got both" feat-name))
	(name-only nil)
	(values (convert-feature-value-hierarchy values))
	(t (error "expected feature ~s definition to have either :values or :name-only, but got neither" feat-name))
	)))

(defmacro ld::define-feature-rule (rule-name &key feature implies)
    (declare (ignore rule-name)) ; FIXME?
  ;; FIXME: this assumes that feature values used in rules like this are not
  ;; shared among multiple features, which is true for now, but only because
  ;; the shared +/- values aren't used for rules
  `(ld::concept ,(second feature)
    ,(convert-old-sem implies :part-p t)))

(defmacro ld::define-feature-list-type (fltype-name &key features defaults)
  (unless (assoc 'f::type defaults)
    (push '(f::type ont::any-sem) defaults))
  ;; the default default for a feature is the feature name with "ANY-"
  ;; prepended, except for F::type/ONT::any-sem handled above
  (dolist (f features)
    (unless (assoc f defaults)
      (push (list f (prepend-any f)) defaults)))
  `(ld::sem-feats ,fltype-name
    ;; FIXME: for historical reasons, feature names/values are referred to as
    ;; plain symbols (in LD package) instead of the concepts (with names in F
    ;; package) they now are (also need to account for f::type values that are
    ;; ONT types instead of F symbols like other feature values)
    ,@(convert-variables-to-disjunctions
          (util::convert-to-package defaults :ld))))

(defun convert-sense-key (sk-str)
  "Convert a WordNet sense key string as it appears in the Lisp TRIPS ontology
   to a symbol compatible with other resources."
  (intern
    (case (count #\: sk-str :start (position #\% sk-str))
      (2 (concatenate 'string sk-str "::"))
      (4 sk-str)
      (t (error "Bogus sense key in TRIPS ontology: ~s" sk-str))
      )
    :WN))

(defmacro ld::define-type (type &key parent sem arguments coercions wordnet-sense-keys comment definitions entailments)
    (declare (ignore coercions definitions entailments)) ;; TODO include definitions, entailments
  `(ld::concept ,type
    ,@(when comment
      `((ld::comment ,comment)))
    (ld::provenance TRIPS)
    ,@(when parent
      `((ld::inherit ,parent)))
    ,@(when wordnet-sense-keys
      `((ld::overlap ,@(mapcar #'convert-sense-key wordnet-sense-keys))))
    ,@(when sem
      (list (convert-old-sem sem :part-p t)))
    ,@(when arguments
      `((ld::sem-frame
        ,@(mapcar
	    (lambda (arg)
	      (destructuring-bind (optionality role &optional restr-sem &rest params) arg
		(setf role (util::convert-to-package role :ONT))
		(unless (typep role 'ont::sem-role)
		  (error "expected ~s to name a semantic role in (define-type ~s ...), but it's not listed in src/DeepSemLex/code/lib/symbol-types.lisp (maybe you forgot to add it?)" role type))
	        (let* ((implements (util::convert-to-package (second (assoc :implements params)) :ONT))
		       (roles (if (and implements (not (eq implements role)))
		                (list role implements)
				role))
		       (restr (convert-old-sem restr-sem))
		       )
		  `(,roles ,restr ,@(when (eq :optional optionality) '(ld::optional))))))
	    arguments))))
    ;; TODO coercions is only used once in the entire ontology, is it worth it?
    ))

(defun get-syn-cat-from-constit (constit)
  "Get syn-cat and optionally head words from a (% syn-cat . features) constit."
  (let ((syn-cat (convert-variables-to-disjunctions (second constit)))
        (head-words
	  (mapcan
	      (lambda (x)
		(if (consp (second x)) ; (? var opt1 opt2...)
		  (cddr (second x))
		  (cdr x)
		  ))
	      (remove-if-not
		  (lambda (x)
		    (and (member (car x) '(w::ptype w::lex))
		         (not (util:is-variable-name (second x)))))
		  (cddr constit)
		  )
	      )))
    ;; FIXME what about other features?
    (when (util::is-variable-name syn-cat)
      (setf syn-cat t))
    (cond
      ((null head-words)
        syn-cat)
      ((= 1 (length head-words))
        (cons syn-cat head-words))
      (t
        (list syn-cat (cons 'ld::or head-words)))
      )))

(defun get-defun-params-from-templ-args (args)
  (mapcan
      (lambda (arg)
        (when (eq :parameter (caadr arg))
	  (let ((param-name (second (second arg)))
	        (default (assoc :default (cddr (second arg)))))
	    ;; FIXME maybe this isn't actually what :default means; there can
	    ;; also be :required, which suggests that the features from xp may
	    ;; override the :default ones but not the :required ones, and
	    ;; otherwise all the :default and :required features are still
	    ;; there even if an xp parameter is supplied
	    (if default
	      ;; TODO also get syn-feats (see also define-template-constant)
	      `((,param-name ',(get-syn-cat-from-constit (second default))))
	      (list param-name)
	      ))))
      args))

(defmacro define-template-function (name args syn-feats)
  (let ((defun-params (get-defun-params-from-templ-args args)))
    `(defun ,name (&key ,@defun-params)
      (ld::syntax
	;; set template-call to the reconstructed call to this function
	(setf (template-call (current-concept)) (list ',name))
	,@(mapcar
	  (lambda (dp)
	    ;; add the argument to the list unless it's the default
	    `(unless (equalp ,@dp)
	      (push ,(repkg (car dp) :keyword)
	            (template-call (current-concept)))
	      (push (list 'quote ,(car dp))
	            (template-call (current-concept)))
	      ))
	  defun-params)
	(setf (template-call (current-concept))
	      (nreverse (template-call (current-concept))))
	;; add syn-feats
	,@(when syn-feats
	  `((ld::syn-feats ,@syn-feats)))
	;; add each arg
	;; see also ld::syn-sem in load.lisp
	,(optionally-named-concept-subtype 'syn-sem
	    (mapcar
	      (lambda (arg)
		(destructuring-bind (syn-arg constit sem-role
				     &optional optional) arg
		  `(let ((syn-cat
			   ,(if (eq :parameter (car constit))
			     (second constit)
			     ;; TODO also get syn-feats (see also
			     ;; define-template-constant)
			     `',(get-syn-cat-from-constit constit)
			     )))
		    (multiple-value-bind (dsl-syn-cat head-word)
		        (separate-head-word-from-syn-cat syn-cat)
		      (push
			  (make-instance 'syn-sem-map
			      :syn-arg ',(util::convert-to-package syn-arg :dsl)
			      :syn-cat dsl-syn-cat
			      :head-word head-word
			      :sem-role
			        ',(util::convert-to-package sem-role :ont)
			      :optional ,(not (null optional))
			      )
			  (maps (current-concept))
			  )))))
	      args))
	))))

(defmacro define-template-constant (name args syn-feats)
  `(progn
    (ld::syntax ,name
      (setf (template-call (current-concept)) '(,name))
      ,@(when syn-feats
	`((ld::syn-feats ,@syn-feats)))
      ,@(when args
	`((ld::syn-sem
	  ,@(mapcar
	      (lambda (arg)
		(destructuring-bind (syn-arg constit sem-role
				     &optional optional) arg
		  `(,(util::convert-to-package syn-arg :ld)
		    ;; TODO also get syn-feats (where to put them? how to deal
		    ;; with variables?)
		    ,(get-syn-cat-from-constit constit)
		    ,(util::convert-to-package sem-role :ont)
		    ,@(when optional '(ld::optional))
		    )))
	      args))))
      )
    ;; define it as a function anyway so we can always use templates
    ;; the same way
    (defun ,name () (gethash ',name (concepts *db*)))
    ))

;; debug
(defun count-variable-occurrences (x &optional o)
  (cond
    ;; (? var ...)
    ((and (listp x) (<= 2 (length x))
          (util::is-variable-symbol (car x)) ; '? in whatever pkg
	  (symbolp (second x)))
      ;; translate to (?var ...)
      (count-variable-occurrences
          (cons
              (intern (concatenate 'string "?" (symbol-name (second x))))
	      (cddr x))
	  o))
    ;; other conses
    ((consp x)
      (count-variable-occurrences
          (cdr x)
	  (count-variable-occurrences (car x) o)))
    ;; ?var
    ((util::is-variable-name x)
      (let* ((rx (repkg x))
             (p (assoc rx o)))
        (if p
	  (incf (cdr p))
	  (push (cons rx 1) o)
	  )
	o))
    ;; anything else
    (t o)
    ))

;; TODO the use of variables in templates can be complicated, e.g.
;; THEME-PRED-EXPERIENCER-OPTIONAL-TEMPL
;; AFFECTED-COST-COMPLEX-SUBJCONTROL-TEMPL
;; ...
(defmacro ld::define-templates ((&rest templ-specs))
  `(with-clean-load-context
    (ld::provenance TRIPS)
    ,@(mapcar
      (lambda (templ-spec)
        (let ((name (util::convert-to-package (car templ-spec) :ONT))
	      (syn-feats
	        (convert-variables-to-disjunctions
		    (util::convert-to-package
		        (cdr (assoc 'ld::syntax (cdr templ-spec))) :ld)))
	      (args (cdr (assoc 'ld::arguments (cdr templ-spec))))
	      )
;	  ;; debug
;	  (let ((var-counts (count-variable-occurrences templ-spec)))
;	    (when (some (lambda (x) (not (= 1 (cdr x)))) var-counts)
;	      (format t "template ~s has multiple occurrences of the same variable:~%  ~s~%" name var-counts)))
	  (setf syn-feats
	        (delete-if
		    (lambda (x)
		      (member (car x) '(
		        ld::morph ; TODO handle morph separately
			ld::arg ld::sa-id ; just variables, useless
			ld::sem ; not syntactic
			ld::qcomp ld::qof ; constit-valued (not symbols)
			)))
		    syn-feats))
	  (if (find :parameter args :key #'caadr)
	    `(define-template-function ,name ,args ,syn-feats)
	    `(define-template-constant ,name ,args ,syn-feats)
	    )))
      templ-specs
      )))

(defun convert-templ-call (templ-call)
  (cons (util::convert-to-package (car templ-call) :ONT)
        (mapcan
	    (lambda (arg)
	      `(,(intern (symbol-name (car arg)) :keyword)
	        ',(get-syn-cat-from-constit
		    (second arg))))
	    (cdr templ-call))
        ))

(defun meta-data-comments-to-string (comments)
  "Convert whatever is in the :comments field of the meta-data of a TRIPS sense
   to a string."
  (typecase comments
    (string comments)
    (symbol (symbol-name comments))
    (list (format nil "~{~a~^ ~}" (mapcar #'meta-data-comments-to-string comments)))
    (otherwise (format nil "~s" comments))
    ))

(defmacro ld::define-words (&key pos templ boost-word tags words)
    (declare (ignore boost-word)) ; TODO use boost-word
  (when templ
    (setf templ (list (util::convert-to-package templ :ONT))))
  `(with-clean-load-context
    (ld::pos ,(util::convert-to-package pos :ld))
    (ld::provenance TRIPS
      ,@(mapcar (lambda (tag) `(ld::provenance ,tag)) tags))
    ,@(mapcar
        (lambda (word-senses-spec)
	  (let* ((word-spec (first word-senses-spec))
	         (wordfeats (cdr (assoc 'ld::wordfeats (cdr word-senses-spec))))
		 (morph (second (assoc 'w::morph wordfeats)))
		 (syn-wordfeats (remove 'w::morph wordfeats :key #'car))
		 (abbrev (cdr (assoc 'ld::abbrev (cdr word-senses-spec))))
		 (sense-specs (cdr (assoc 'ld::senses (cdr word-senses-spec))))
		 )
	    (when (eq 'ld::nil morph) (setf morph nil)) ; blech
	    `(ld::word ,word-spec
	      ,@(when morph
		`((ld::morph
		  (ld::forms
		    ,@(let* ((have-forms (member :forms morph))
		             (forms (second have-forms))
			     (null-forms (or (null forms) (eq 'ld::nil forms))))
		        (cond
			  ((not have-forms) nil) ; default regular forms
			  (null-forms (list '-none)) ; just the base form
			  (t forms) ; specified forms
			  ))
		    ,@(loop for tail = morph then (cddr tail)
		            for key = (car tail)
			    for val = (cadr tail)
			    while tail
			    unless (member key '(:forms :nomsubjpreps :nomobjpreps)) ; TODO use :nom*preps somehow
			      collect
			        ;; FIXME add particle to val
			        (util::convert-to-package (list key val) :ld
				    :convert-keywords t)
			    )
		    ))))
	      ,@(when syn-wordfeats
	        ; TODO how to represent word-level syn-feats?
		; . add the feats to all morph-maps (blech)
		; . wrap all the senses in a syn-feats so that it becomes a parent
	        nil)
	      ,@(when abbrev nil) ; TODO
	      ,@(mapcar
	          (lambda (sense-spec)
		    (let* ((parent (second (assoc 'ld::lf-parent sense-spec)))
		           (sense-templ (cdr (assoc 'ld::templ sense-spec)))
			   (sem-feats (cdr (assoc 'ld::sem sense-spec)))
			   (meta-data (cdr (assoc 'ld::meta-data sense-spec)))
			   (examples (mapcar #'second (remove-if-not (lambda (f) (eq 'ld::example (car f))) sense-spec)))
			   ;; TODO lf, lf-form, syntax (which includes morph), preference, non-hierarchy-lf, prototypical-word
			   (effective-templ
			     (cond
			       (sense-templ (convert-templ-call sense-templ))
			       (templ templ)
			       (t (error "missing template for (:* ~s ~s)"
				 	 parent word-spec))
			       ))
			   )
		      `(ld::sense ; TODO :* name?
			,@(when parent
			  `((ld::inherit ,parent)))
		        ,@(when sem-feats
			  `((ld::semantics ; FIXME icky extra level
			      (ld::sem-feats
			        ,@(convert-variables-to-disjunctions
				    (util::convert-to-package sem-feats :ld)
				    )))))
			,@(when effective-templ
			  `((add-relation (current-concept) :inherit
			      (let (*concept-stack*
			            *current-word*
				    *current-pos*
				    *current-morph*)
			        ,effective-templ)
			      *current-provenance*)))
			,@(when meta-data
			  (destructuring-bind (&key vn comments &allow-other-keys)
			      ; TODO more meta-data fields
			      ; in particular :wn, but it's tricky because it
			      ; often refers to a sense of the wrong word
			      meta-data
			    `(
			      ,@(when vn
				`((ld::overlap
				   ,@(mapcar (lambda (str)
					       (intern (string-upcase str) :vn))
					     vn)
				   )))
			      ,@(when (and comments
					   (not (eq 'ld::nil comments)))
			        `((ld::comment
				   ,(meta-data-comments-to-string comments))))
			      )))
			,@(mapcar
			    (lambda (ex)
			      `(ld::example (ld::text ,ex)))
			    examples)
			)))
		  sense-specs)
	      )))
	words)
    ))

