;;;;
;;;; make-vocab.lisp
;;;;

;;
;; The file for handling vocabulary file.
;; Reads in the vocabulary file and makes a list which can be used by expand
;;
;;TODO: add support for multiword entries
;; ?? change the process so that semantics is added last
(in-package "LEXICONMANAGER")

(defvar *default-transforms-applied* nil)
(defvar *coercion-decrement* 0.98)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initial processing for vocab files. Reads the external
;; representation and converts it into internal structures that can
;; later be processed by lexicon functions
;;

;; Takes a list of word definitions, parses them and adds the resulting structures to the table
(defun add-words-to-vocab-table (wlist vocab-table &key (pos nil))
  
  (mapcar (lambda (x)
            ;; right now we insist that there is one location where a word is defined
            (if (gethash (first x) vocab-table) 
              (lexiconmanager-warn "Attempting to define word ~S twice" (first x))
              (setf (gethash (first x) vocab-table) (parse-vocab-table-entry x :pos pos))
             ))
	  wlist
	  )
     
  )

(defun parse-vocab-table-entry (entry &key (pos nil) (default-template nil) (boost-word nil))
  "Takes a list representation of the entry and makes it into vocabulary-entry structure"
  (let* (
	(wdef (first entry))
	(pronunciation (get-arg-value-list 'pronunciation (rest entry)))
	(wfeat (get-arg-value-list 'wordfeats (rest entry)))
	(abbrev (get-arg-value-list 'abbrev (rest entry)))
    	(senses (get-arg-value-list 'senses (rest entry)))
	(morph (cadr (assoc 'w::morph wfeat)))
	(nomobjpreps (if (eq pos 'w::V)
			 (or (find-arg morph :nomobjpreps)
			     '(w::of))))
	(nomsubjpreps (if (eq pos 'w::V)
			   (or (find-arg morph :nomsubjpreps)
			       '(w::by))))
	)
    ;;  for verbs,  nomobjprops and nomsubjpreps  get promoted into the sense
    ;;(format t "~%senses = ~S nomobjpreps = ~S nomsubjpreps = ~S~%" senses nomobjpreps nomsubjpreps)
    (if nomobjpreps (setq senses (mapcar #'(lambda (sensespec)
					     (let ((synt (get-arg-value-list 'syntax sensespec)))
					       (append sensespec (list (list 'syntax (list 'w::nomobjpreps (list* '? 'objp nomobjpreps))
									     (list 'w::nomsubjpreps (list* '? 'subjp nomsubjpreps)))))))
					 senses)))
    ;;(format t "~%UPDATED senses = ~S" SENSES)
    (multiple-value-bind
      (word part rest)
      (parse-word-spec wdef)
      
      (when (null senses)
        (lexiconmanager-warn "Word ~S has no senses defined in entry ~S" word entry))
      
      (handler-case (make-vocabulary-entry
                     :name (gen-id (if rest (cons word rest) word))
		     :word word
                     :particle part
                     :remaining-words rest
		     :pos pos
		     :wfeats wfeat
		     :pronunciation pronunciation
		     :abbrev abbrev
		     :senses (remove nil
				     (mapcar (lambda (x) 
					       (parse-word-sense wdef x 
								 :boost-word boost-word :pos pos :default-template default-template))
					     senses))
		     :boost-word boost-word
		     )
        (invalid-vocab-entry (err) 
	  (lexiconmanager-warn "Invalid vocabulary entry for ~S:~%  ~A" word err)
	  nil)
        )
      ))
  )

(defun parse-word-spec (w)
  "Extracts out complex word entries, either with particles or compound words. returns
     three values: the base word, the particle (if present) and the remaining words (for a compound)"
  (cond ((symbolp w) w)
        ((and (consp w)
              (eql (list-length w) 2) (consp (second w)))
           (values (car w) (car (second w)) nil))
        ((and (consp w) (every #'symbolp w))
             (values (car w) nil (cdr w)))
        (t  (lexiconmanager-warn "Bad word specification: ~S" w))))
           
(defun make-into-symbol (x)
  "converts a list into a symbol"
  (convert-to-syntax-package ;; w::
   (cond ((symbolp x) x)
	 ((consp x) 
	  ;;  special case where as have a list with a single number
	  (if (and (eq (list-length x) 1) (numberp (car x)))
	      (setq x (list 'N x)))
	  (reduce #'(lambda (x y)
		      (read-from-string (format nil "~A-~A" x y)))
		  (flatten x))))
   )
  )

;;  GEN-SYMBOL generates a unique identifier to identify a constituent (dupe of "gen-symbol" in util package)
(defun gen-symbol-lxm (name)
  (gentemp (string name)))

(defun gen-id (x)
  "Generates a unique ID related to the word, or list of words for compounds"
  ;; Myrosia added interning of the id into :w package
  (convert-to-syntax-package
   (cond
     ((symbolp x)
      (gen-symbol-lxm x))
     ((consp x)
      (if (or (symbolp (car x)) (numberp (car x)))
	  (gen-symbol-lxm (make-into-symbol x))))
     (t (gen-symbol-lxm 'comp)))
  ))


(defun add-pos-to-lf-table (lf pos)
"create index of lf-types and their pos"
  (let ((poslist (gethash lf (lexicon-db-lf-table *lexicon-data*))))
    (if (not poslist)
	(setf (gethash lf (lexicon-db-lf-table *lexicon-data*)) (list pos))
	(if (not (find pos poslist))
	    (setf (gethash lf (lexicon-db-lf-table *lexicon-data*)) (pushnew pos poslist))
	    ))
 ;   (print-debug "~% poslist for lf ~S is ~S ~%" lf (gethash lf (lexicon-db-lf-table *lexicon-data*)))
    )
  )
  
;;; example of a word sense as input to parse-word-sense
;;;  ((LF-PARENT LF::TOP) 
;;;	(SEM (f::Aspect f::bounded) (f::Time-span f::atomic))
;;;	(TEMPL THEME-TEMPL)
;;;	)

(defun parse-word-sense (word sense &key (pos nil) (default-template nil) (boost-word nil) (remaining-words nil))
  "The main parsing function. Take a sense and convert it into internal represenation for easier processing later on. Doesn't do any error checking"
  (declare (ignore remaining-words)) ;; not used here
  (if (every #'consp sense)
    (let* ((lf (get-arg-value 'LF sense))
	   (lf-parent (get-arg-value 'LF-PARENT sense))
	   (lf-form (get-arg-value 'LF-FORM sense))
	   (templ (or (get-arg-value-list 'TEMPL sense) (and default-template (list default-template))))
	   (synt (get-arg-value-list 'syntax sense)) 
	   (sem (get-arg-value-list 'SEM sense))
	   (pref (or (get-arg-value 'preference sense) *no-kr-probability*))
	   (nonhier-lf (get-arg-value 'non-hierarchy-lf sense))
	   (meta-data (get-arg-value-list 'meta-data sense))
	   (prototypical-word (get-arg-value 'prototypical-word sense))
	   )
      (when (and templ (null (retrieve-template (car templ))))
	(format t "~%DEFINE-WORDS: Warning, unknown template: ~S:" templ))
      (when (and (null lf) (null lf-parent) (null lf-form))
        (error
         (make-system-condition 'invalid-vocab-ref
			        :format-control "No LF information declared for word ~S in the sense ~S"
			        :format-arguments (list word sense)
			        )))
       (when (and lf (not nonhier-lf))
        (error
         (make-system-condition 'invalid-vocab-ref
			        :format-control "Can't define an LF unless it's non hierarchical ~S in the sense ~S"
			        :format-arguments (list word sense)
			        )))
      (when (and lf (or lf-parent lf-form))
        (error
         (make-system-condition 'invalid-vocab-ref
			        :format-control "Both LF and LF-PARENT or LF-FORM are declared for word ~S in the sense ~S"
			        :format-arguments (list word sense)
			        )))
      (when (and lf-form (not lf-parent))
        (error
         (make-system-condition 'invalid-vocab-ref
			        :format-control "LF-FORM declared without LF-PARENT in word ~S the sense ~S"
			        :format-arguments (list word sense)
			        )))
      (when (or (and (not nonhier-lf) (not (symbolp lf))) (not (symbolp lf-parent)) (not (symbolp lf-form)))
        (error
         (make-system-condition 'invalid-vocab-ref
			        :format-control "LF, LF-FORM or LF-PARENT value is not a symbol in word ~S in the sense ~S"
			        :format-arguments (list word sense)
			        )))
      (when (and lf-parent pos) (add-pos-to-lf-table lf-parent pos))
      (make-sense-definition :lf lf :pos pos :lf-parent lf-parent :lf-form (if (null lf) (or lf-form 
                                                                                             (make-into-symbol word)))
			     :nonhierarchy-lf nonhier-lf
			     :templ (first templ) :params (rest templ) :syntax synt
			     :sem sem  :pref pref
			     :boost-word boost-word 
			     :meta-data meta-data
			     :prototypical-word prototypical-word
			     )
      )
    (progn
     (lexiconmanager-warn "Bad sense specification in word ~S: ~S"
                 word sense)
     nil
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Main processing functions. Integrate vocabtable structures with
;; synttable, LF and KR ontologies to obtain the sense definition
;; lists
;;
;;
;;

;; Given a vocabulary entry, make a list of corresponding sense definitions
(defun make-word-sense-definitions (worddef synttable)
  ;;(format t "~%MAKE-WORD-SENSE-DEFINITIONS: ~S" worddef)
  (if (vocabulary-entry-p worddef)
      (let ((word (vocabulary-entry-word worddef))
	    (wfeat (vocabulary-entry-wfeats worddef))
	    (senses (vocabulary-entry-senses worddef))
	    (name (vocabulary-entry-name worddef))
	    (remaining-words (vocabulary-entry-remaining-words worddef))
	    (particle (vocabulary-entry-particle worddef))
	    )
	(let ((res (remove-if #'null 
		   (mapcar (lambda (sense)
			     (handler-case (make-sense-definition-entry word particle remaining-words name wfeat sense synttable)
			       (invalid-vocab-ref (err) 
				 (lexiconmanager-warn "Invalid sense definition for word ~S:~%  ~S~%  ~A" word sense err)
				 nil)
			       (invalid-sem-spec (err) 
				 (lexiconmanager-warn "Invalid sense definition for word ~S:~%  ~S~%  problem with sem~%  ~A" word sense err)
				 nil)			
			    ))
			   senses
			   ))
		))
	  ;;(format t "~%MAKE-WORD-DENSE-DEINFITIONS returns ~S" res)
	  res))
      
    (list worddef)))


;; given a word, a morph, sense in sense-definition format and a set of tables, makes a list
;; of resulting sense definitions in the internal format
(defun make-sense-definition-entry (word particle remaining-words name wfeat sense synttable)
  (let ((wordfeat wfeat)		
	)
    ;; push the particle as (% word (lex part)) on the features
    ;; Myrosia 2004-06-17 changed to part of speech PART so that it can be properly boosted up
    (when particle
      (push `(w::part (% w::part (w::lex ,particle)))
	    wordfeat)
      )    
    (make-lexicon-definitions word remaining-words name wordfeat sense synttable)
    ))


;; given a word, its syntactic features, a sense definition and a set
;; of tables, integrates the information and makes a lexicon definition in
;; internal definition format
(defun make-lexicon-definitions (word remaining-words name wfeat sense synttable)
  "A top level function that checks for errors in the sense definition, and integrates it into a word-sense-definition as necessary, and adds lf coercions"
  (etypecase sense
    ;; we have an entry with lf and template definitions already integrated
    (word-sense-definition
     (add-lf-coercions (copy-word-sense-definition sense))
     )
    ;; otherwise we have to get the lf and template definitions and integrate them
    (sense-definition
     (let* ((res nil)
	    ;; we have a basic sense definition that needs some extra processing	    
	    (lfdef (establish-lf word sense))
	    (templdef (gethash (sense-definition-templ sense) synttable))
	    )
       ;; Error checking first then call the main merge function
       (cond
	((null templdef)
	 (error
	  (make-system-condition 'invalid-vocab-ref
				 :format-control "Invalid template name ~S" 
				 :format-arguments (list (sense-definition-templ  sense))
				 )))	
	(t ;; no errors - merge
	 (setq res (make-word-sense-from-basic-sense word remaining-words name wfeat sense templdef lfdef))
	 ))
       (add-lf-coercions res)
       res))
    ))

(defun make-word-sense-from-basic-sense (word remaining-words name wfeat sense templdef lfdef)
  "Given a word and a sense definition, merges in the template and lf definitions, and creates a word sense def"
  (let* ((maps (create-word-synsem-mappings (syntax-template-mappings templdef) 
					    (sense-definition-params sense)))
	 (sem (make-untyped-sem (sense-definition-sem sense)))
	 (fullsem '-) 
	 (semargs nil) ;; derived if there is a LF definition in the table
	 )	
    (when lfdef
      ;; get full semantic specification by merging lf and sem
      (setf fullsem (merge-sem-with-lf lfdef sem))
      ;; We make arguments for the LF, and then for feature list, and merge them together
      (setf semargs (make-roles-from-lf-arguments 
		     (lf-description-arguments lfdef))	    
	    ))    
  
    (check-synsem-mappings (mapcar #'second maps) semargs)
    (make-word-sense-definition			     
     :word word :remaining-words remaining-words
     :name name :lf (or (and lfdef (lf-description-name lfdef)) (sense-definition-lf sense)) 
     :sem fullsem :roles semargs
     :pos (sense-definition-pos sense)
     ;;  WFEAT have highest priority, then the sense definition features, then the template
     :syntax (merge-in-defaults wfeat 
				  (append (sense-definition-syntax sense)
					  (list (list 'w::template (sense-definition-templ sense)))
					  (syntax-template-syntax templdef)))
  ;   :template 'template

     :pref (sense-definition-pref sense)
     :mappings maps
     :nonhierarchy-lf (null lfdef)
     :boost-word (sense-definition-boost-word sense)
     :prototypical-word (sense-definition-prototypical-word sense)
     ;; swift -- there are 2 issues here:
     ;; 1) sense-definition accessors and word-sense-definition accessors can't be applied to the same object because they're siblings, not children
     ;; 2) cmucl can figure out that sense is always a sense definition, so this code won't execute, coercions here will always be nil
     ;;:coercions (and (word-sense-definition-p sense) (word-sense-definition-coercions sense))
     )
    ))

(defun establish-lf (word sense)
  "Given the sense, established the LF. If it is specified directly, gets it from the table. If it is not specified, makes it from lf-parent and lf-form and adds it to the table if necessary"
    (cond
     ((sense-definition-nonhierarchy-lf sense)
      nil)
     (t
      (let* ((lf-parent (sense-definition-lf-parent sense))
	     (lf-form (or (sense-definition-lf-form sense) word))
	     (res (get-lf-description lf-parent))
	     )
	(cond
	 ;; get-lf-description always returns a structure, never null, so this won't execute
	 ;; need a new test to make it work
;         ((null res)
;          ;; complain if you get an lf parent that doesn't exist
;          (Error
;           (make-system-condition 'invalid-vocab-ref
;                                  :format-control "Invalid LF parent name -- doesn't exist ~S" 
;                                  :format-arguments (list lf-parent)
;                                  )))
         ((listp lf-form)
	  (error 
	   (make-system-condition 'invalid-vocab-entry
				  :format-control "Invalid lf form ~S"
				  :format-arguments (list lf-form)
				  )))
	 (t ;; previously seen form - return it
	  (setf (lf-description-name res) (list :* lf-parent lf-form))
	  res)	 
	 )
	))
     ))


; return the slot (e.g. ont::agent, ont::theme) for a specific grammatical name, e.g. subj or obj
(defun get-slot-for-name (maps grammatical-name)
  (remove-if #'null (mapcar (lambda (map)
			      (if (eql (basic-synsem-map-name (cdr map)) grammatical-name)
				  (basic-synsem-map-slot (cdr map)))
			      )
			    maps))
  )

(defun check-synsem-mappings (maps roles)
  "Checks the synsem mappings for consistency with roles. Currently only checks that every role
   defined in synsem map is also defined within roles"  
  (dolist (map maps)
    (let ((slotname (basic-synsem-map-slot map)))
      (when (and (not (eql slotname 'norole)) (null (assoc slotname roles)))
	(Error 
	 (make-system-condition 'invalid-vocab-ref
				:format-control "Syntax-semantic mapping~%  ~S~%  incompatible with allowed semantic role list~%  ~S~%  - no role ~S present" 
				:format-arguments (list  map roles (basic-synsem-map-slot map))))
	)
      ))
  )


;; Takes the LF defomotopm, retrieves the associated SEM features
;; Merges them with semspec, infers all implied features
;; returns the complete ($ sem feature-list) specification.

;;;(defun merge-sem-with-lf (lf semspec)
;;;  (let ((mainfeat nil)
;;;	)
;;;    ;; Now do the merging
;;;    (checksemfeatures (feature-list-features semspec) lfontology)
;;;    (get-all-sem-features 
;;;     (merge-typed-feature-lists semspec (lf-description-sem lf) lfontology)
;;;     lfontology
;;;     )
;;;    (setf mainfeat (merge-in-typed-defaults semspec (lf-description-sem lf)))  
;;;    ;; Now infer all implied features and add in defaults
;;;    (complete-features-with-defaults (infer-implied-features mainfeat feattable) 
;;;				     (ling-ontology-feature-type-table lfontology))
;;;    ))

;; a function that takes a list of LF arguments and makes a list of
;; role-restriction pairs out of it
(defun make-roles-from-lf-arguments (lfargs)
  (mapcar (lambda (arg)
	    (list (sem-argument-role arg) (make-nonempty-sem (sem-argument-restriction arg)))
	    )
	  lfargs)
  )

(defun make-nonempty-sem (sem)
  "Given a semantic structure, if type is a variable, make sure that it is nonempty"
  (if (null (feature-list-features sem))
      (let ((type (feature-list-type sem)))
	(cond
	 ((and (listp type) ;; we assume that the second value is a variable name
	       (null (cddr type)) ;; there is no restriction
	       )
	  (setq type (right-var (second type) '?! nil)))
	 ((is-variable-name type)
	  ;; strip off the ? and put a ! after it to force nonempty, but use gensym to make sure the var is unique
	  (setq type (read-from-string (concatenate 'string "?!" (symbol-name (gensym (subseq (symbol-name type) 1))))))
	  ))
	(make-feature-list :type type :defaults (feature-list-defaults sem))
	)
    sem))


  
       
(defun create-word-synsem-mappings (synsemmaps params)
  "Performs parameter replacement in synsemmaps based on params. Returns a list of word-synsem-map structures with parameters substituted properly"
  ;; First a check - all parameters defined have to be in templates
  ;; if not, raise an error
  (when (not (subsetp params synsemmaps :test #'parameter-map-helper))
    (Error 
     (make-system-condition 'invalid-vocab-ref
			    :format-control "Parameters ~S incompatible with synsemmaplist ~S - no mapping use them" 
			    :format-arguments (list 
					       (set-difference params synsemmaps 
							       :test #'parameter-map-helper)
					       synsemmaps)))
    )
  ;; now we know everything is OK, so we simply convert the mappings and return a list consed with mapping names
  (mapcar (lambda (x)
	    (list (first x) (create-word-synsem-map (cdr x) params))
	    )
	  synsemmaps))


(defun parameter-map-helper (param map)
  "True if a parameter matches a given synsem-map"
  (eql (first param) (synsem-map-paramname (cdr map)))
  )
  
(defun create-word-synsem-map (synsemmap params)
  "Determines if a parameter defined in synsemmap is re-defined in params and creates a syntax restriction accordingly"
  (let* ((paramname (synsem-map-paramname synsemmap))
	 (paramval (or (get-arg-value paramname params) (synsem-map-default synsemmap)))
	 ;; the following lines parse and make the distinction between (? varname (% ...)) and (% ...) which are the only 2 allowed forms
	 ;; if there's no specified varname, it defaults to the argument name (e.g. lsubj)
	 (var (if (eql (first paramval) '?) (second paramval) (synsem-map-name synsemmap)))
	 ;; paramrestr should end up as (% cat feats)
	 (paramrestr (if (eql (first paramval) '?) (third paramval) paramval))
	 )
    (make-word-synsem-map
     :name (synsem-map-name synsemmap)
     :slot (synsem-map-slot synsemmap)
     :optional (synsem-map-optional synsemmap)
     :maponly (synsem-map-maponly synsemmap)
     :varname var
     ;; paramrestr here is (% cat feats)
     :syntcat (second paramrestr)
     ;; for features we take the required features and treat anything specified in other restrictions as defaults only
     ;; !!! really need an error message if there's a merge error here
     :syntfeat (merge-feature-lists (cddr paramrestr) (synsem-map-required synsemmap) 
				    :merge-function #'(lambda (x y) (merge-features x y :detect-conflicts t)))
     ;;:syntfeat (concatenate 'list (cddr paramrestr) (synsem-map-required synsemmap))
     )
    ))
  

(defun add-lf-coercions (sense)
  " 
;; add-lf-coercions (sense)
;; Given a word-sense-definition structure, search the lf-ontology for applicable lf coercions and if any, add to sense 
;;
;; sense   : word-sense-definition structure
;; returns : the entry modified with applicable LF coercions
;;
"
  (let ((coercions
	 (mapcar #'(lambda (c)
		     (let ((news (copy-word-sense-definition sense))
			   )
		       ;;	      (setf (word-sense-definition-lf news) (list ':* (lf-coercion-operator-result c) (get-lf-form lfname ':*)))
		       (setf (word-sense-definition-lf news) (coercion-operator-description-lf c))
		       (setf (word-sense-definition-sem news) (coercion-operator-description-sem c))
		       (setf (word-sense-definition-operator news) (coercion-operator-description-name c))
		       news
		       ))
		 (find-lf-coercion-operators  (word-sense-definition-lf sense))
		 ))	 
	)
    (setf (word-sense-definition-coercions sense) (union (word-sense-definition-coercions sense) coercions :test #'equal))
    sense
    ))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Conversion from lexicon internal sense format to parser augment-lexicon :leaf format
;;

(defun build-synt-arguments (mappings roles)
  "Given a set of syntactic mappings, makes a list with the following information:
    LSUBJ, LOBJ, LCOMP, LIOBJ restrictions (syntactic),
    LSUBJ-MAP, LOBJ-MAP, LCOMP-MAP, LIOBJ-MAP (checked with roles) "
  (mmapcan (lambda (map) (build-synt-argument (second map) roles)) mappings)
  )

(defun build-synt-argument (arg roles)
  "Takes in argument mapping, generates a corresponding syntactic argument and mapping feature"
  (remove-if #'null
	     (cons
	      (generate-map arg)
	      (generate-subcat-features (set-optional-feature arg) roles)	      
	      ))
  )


;; changed call to Merge-feature-list-with-defaults to include om package
(defun get-sem-for-synsemmap (synsemmap roles)
  "Gets the restriction on synsemmap based on roles. Merges restrictions from all roles synsemmap maps to into a single sem and return it"
  (let ((srole (basic-synsem-map-slot synsemmap))
	)
    (cond
     ((listp srole) ;; maps to many roles
      (reduce (lambda (x y) (om::merge-feature-list-with-defaults x y))
	      (mapcar (lambda (x) (get-arg-value x roles)) srole))
      )
     (t ;;  a single role
      (get-arg-value srole roles)
     )
     )
    ))

(defun generate-subcat-features (synsemmap roles)
  "Given a synsemmap, generates a corresponding subcat argument for the lexical entry"
  (let* ((argname (word-synsem-map-name synsemmap))
	 ;; we need to take special care so that if we don't
	 ;;(semvar (right-var argname '? 'sem))
	 (semrestr (get-sem-for-synsemmap synsemmap roles))
	 (semfeature (when (not (word-synsem-map-maponly synsemmap))
		       (list 'w::sem
			     (if semrestr
				 (make-type-spec semrestr 
						 :semvar (right-var argname '? 'type) 
						 :feature-list-sign '$)
			       (right-var argname '? 'ssem))
			     )))
	 )
    
    
    `((,(right-constit argname)
       (? ,(word-synsem-map-varname synsemmap)
	  ,(make-constituent-spec (word-synsem-map-syntcat synsemmap) 
				  (merge-in-feature semfeature (word-synsem-map-syntfeat synsemmap) 
						    :merge-function #'(lambda (x y) (merge-features x y)))
				  ;; (cons semfeature (word-synsem-map-syntfeat synsemmap))
				  )
	  )
       ))
    ))



;; Given a restriction, adds (optional +|-) to the parameter list
;; The restriction is in synsem-map format
;; returns the list of synsem-map items with optionality features set
;; !!! should add error checking here
(defun  set-optional-feature (synsemmap) 
  (if (basic-synsem-map-optional synsemmap)
      (setf (word-synsem-map-syntfeat synsemmap) 
	(append (word-synsem-map-syntfeat synsemmap) '((w::optional +))))
    (setf (word-synsem-map-syntfeat synsemmap) 
      (append (word-synsem-map-syntfeat synsemmap) '((w::optional -))))
    )
  synsemmap
  )

(defun generate-map (synsemmap)
  "Generates a (CONSTIT-MAP ROLE) features from synsemmaps"
  (let ((slot (basic-synsem-map-slot synsemmap))
	(feat (right-var (right-constit (basic-synsem-map-name synsemmap)) nil '-map))
	)
    (cond
     ((eql slot 'NOROLE) nil)
     ((eql slot 'ARGUMENT) (list 'w::ARG-MAP slot))
     (t
      ;; Myrosia added intern in order to move everything over to W package
      (list (convert-to-syntax-package feat) slot)
      ))
    ))


;; 12/02/00 Myrosia
;; Takes a set of LF arguments in the LF format and converts them to the
;; (% roles (role rolesem)) format
(defun make-role-restrictions (semargs)
  (let ((roleargs 
	 (mapcar (lambda (x)
		   ;; !!!!!!!! FIXME - strip-lf-form is here because parser cannot handle
		   ;; (:* LF::Purpose purpose) as a feature name in ROLES constituent
		   ;; Either the parser needs to be fixed, or we need a new way of dealing with
		   ;; specializations for adverbials		   
		   (list (ontologymanager::strip-lf-form (first x) ':*) 
			 (make-type-spec (second x) :sempref (first x) :feature-list-sign '$)))
		 (remove 'w::ARGUMENT semargs :key #'car)))
	 )
    (when roleargs
      (list
       `(w::roles
	 (% w::roles ,@roleargs)
	 )
       ))
    ))


(defun right-constit (name)
  (ecase name
    (lsubj 'w::subj)
    (lobj  'w::dobj)
    (liobj 'w::iobj)
    (lcomp 'w::comp3)
    (argument 'w::argument)
    (subcat 'w::subcat)
    (post-subcat 'w::post-subcat)
    (premod 'w::premod)
    (subcat2 'w::subcat2)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for automatically adding KR information to parser entries
;;

;; Given a sense list in internal format, merge in KR info
(defun add-kr-info (senselist &key (allow-coercions t) (no-kr-preference 1)
							 (kr-preference 1))
  (mmapcan (lambda (x) 			  	     
	     (let ((answer (add-kr-to-sense x :allow-coercions allow-coercions :kr-preference kr-preference :no-kr-preference no-kr-preference)))	       
	       (when (lextraceon-p)
		
		 (format t "~%       Senses found are ~S" answer))	       
	       answer))	   
	   senselist))



(defun add-kr-to-sense (sense &key (no-kr-preference 1)
				   (kr-preference 1)
				   (allow-coercions t)
				   )
  "given a single sense in word-sense-definition format, add KR info to it. Senses with added kr wil be multiplied by kr-preference. Senses without added kr will be multiplied by no-kr-preference"

  (when (lextraceon-p)
    (format t "~%~%Building entries for sense ~S or word ~S" (word-sense-definition-lf sense) (word-sense-definition-word sense)))
  (cond
   ((word-sense-definition-nonhierarchy-lf sense) ;; the sense is nonhierarchical, so we don't specialize
    (when (lextraceon-p)
      (format t "~%     sense is flagged as non-hierarchical. No mapping applied"))
    (list (make-nonspecialized-sense sense kr-preference no-kr-preference nil))
    )
   ((word-sense-definition-specialized sense) ;; the sense is already specialized, so we do nothing -- but add kr coercions if they are allowed
    ;; and adjust the preference for boosting
    (setf (word-sense-definition-pref sense) (* kr-preference (word-sense-definition-pref sense)))
    (if allow-coercions
	(list (add-kr-coercions (copy-word-sense-definition sense)))
      (list (copy-word-sense-definition sense))
      ))
    
   (t ;; attempt to specialize
    (let* ((lfname (word-sense-definition-lf sense))
	   (sem (word-sense-definition-sem sense))	  
	   ;; find all the transforms applicable to this sense
	   (transforms (add-kr-to-lf lfname sem kr-preference (is-predicate-sense sense) t))
	   ;; try to apply these transforms to the sense
	   (res (mmapcan (lambda (x) 
			   (add-transform-to-sense sense x allow-coercions)) 
			 transforms))		
	   )
      ;; tracing
      (when (and (lextraceon-p) (null transforms))
	(format t "~%  ----> No KR Maps found for lf ~S" lfname)
	)*
      (when (or (null res) *keep-nonspecialized-entries*)
	;; if we have not found specializations, or if we want to keep the nonspecialized entries
	;; we add the nonspecialized entry to the results, boosted down with no kr preference	  
	(setq res (append res (list (make-nonspecialized-sense sense kr-preference no-kr-preference res))))
	)
      res) 
    )
   ))

(defun make-nonspecialized-sense (sense kr-preference no-kr-preference has-defined-specializations)
  "Returns a copy of a sense definition, boosted depending on whether it is a core word"
  (let ((csense (copy-word-sense-definition sense)))
    ;; we boost down anything that already has defined specializations for this sense (which were boosted up), or has no specializations and is not a core word
    (if (or has-defined-specializations (not (word-sense-definition-boost-word csense)))
	(setf (word-sense-definition-pref csense)
	  (* no-kr-preference (word-sense-definition-pref csense)))
      (setf (word-sense-definition-pref csense) ;; core words are upgraded together with kr words
	(* kr-preference (word-sense-definition-pref csense))))
    csense
    ))
  
  
(defun add-transform-to-sense (sense transform-info allow-coercions)
  "Adds a mapping to sense. Sense is in word-sense-definition structure, transform is lf-kr-transform structure, result is a list of new word-sense-definitions associated with the sense"
  (let* ((res (copy-word-sense-definition sense))
	 (transform-name (first transform-info))
	 (transform-class (second transform-info))
	 (transform-preference (third transform-info))
	 (transform-sem (fourth transform-info))
	 (transform-roles (fifth transform-info)))
      (when (lextraceon-p)
	(format t "~%    Applying mapping ~S" transform-name ))    
    (setf (word-sense-definition-kr-type res) transform-class)        
    (setf (word-sense-definition-pref res)      
      (* transform-preference (word-sense-definition-pref sense)))
    ;; add new sem and roles to the sense    
    (setf (word-sense-definition-sem res) transform-sem)    
    (setf (word-sense-definition-roles res)       
      (mapcar #'(lambda (x)		       
		       (list (sem-argument-role x) (sem-argument-restriction x)))
	      transform-roles))
    (setf (word-sense-definition-transform res) transform-name)		    
          ;; set the "specialized" value -- destructive modification within mapc!
    (setf (word-sense-definition-specialized res) t)
    ;; we add the coercion information, but coercions	
    ;; themselves are not boosted up. If boosting is necessary, it
    ;; will happen when the coercion entry is generated based on
    ;; the boost of the parent entry
    ;; if there were any domain independent coercions (these are specified in the lf ontology) 
    ;; then we also need to add kr info to them
    (setf (word-sense-definition-coercions res)
      (add-kr-info (word-sense-definition-coercions res) :allow-coercions nil
		   :kr-preference 1 :no-kr-preference 1))
    (when allow-coercions (add-kr-coercions res))      
    (list res)
  ))


;; add-kr-coercions (cname sense)
;; sense     : word-sense-definition structure that has already been specialized
;; results   : sense returned, with the coercion field set to a list of word-sense-definitions with
;;           : the sem from the coerced type
;; 
(defun add-kr-coercions (sense)
  "Assuming that the sense has the specified class name, adds possible coercions from the KR ontology"
  (let* ((cname (word-sense-definition-kr-type sense))
	 (coercions (find-kr-coercion-operators cname))
	 )
    (setf (word-sense-definition-coercions sense)
      (append  (word-sense-definition-coercions sense)
	       (mapcar (lambda (opdef)
			 (let* ((csense (copy-word-sense-definition sense))
				;;			(opdef (gethash x (kr-ontology-operator-table krontology)))
				(newtype (coercion-operator-description-result opdef))
				(newlf (coercion-operator-description-lf opdef))
				)
			   (setf (word-sense-definition-lf csense) newlf)
			   (setf (word-sense-definition-kr-type csense) newtype)
			   (setf (word-sense-definition-sem csense) 
			     (coercion-operator-description-sem opdef))
			   ;;		     (get-all-sem-features (add-kr-type-to-sem newtype (get-linguistic-type-sem newlf lfontology) lfontology) lfontology))
			   (setf (word-sense-definition-operator csense) (coercion-operator-description-name opdef))
			   csense
			   ))
		       coercions)
	       ))
    sense
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
     ((eql semspec '-) (setf type '-) (setf feat nil)
      )     
     ((null semspec)
      (setq type (or semvar (right-var sempref '? 'sem)))
      )
     (t
      (setf type (or (feature-list-type semspec) 'ont::any-sem))

#|
T -- semspec is #S(FEATURE-LIST :TYPE ?!!TYPE5685 :FEATURES NIL :DEFAULTS NIL) semvar is NIL and type is ?!!TYPE5685
T -- semspec is #S(FEATURE-LIST :TYPE ?!!TYPE5686 :FEATURES NIL :DEFAULTS NIL) semvar is NIL and type is ?!!TYPE5686
T -- semspec is #S(FEATURE-LIST :TYPE ?!!TYPE5686 :FEATURES NIL :DEFAULTS NIL) semvar is ?ARGUMENTTYPE and type is ?!!TYPE5686
T -- semspec is #S(FEATURE-LIST :TYPE ?!!TYPE5685 :FEATURES NIL :DEFAULTS NIL) semvar is ?SUBCATTYPE and type is ?!!TYPE5685

|#
      (setf feat (merge-in-defaults (feature-list-features semspec) (feature-list-defaults semspec)))      
      (when (null type) ;; have a spec, but not the order
	(error (make-system-condition 'invalid-vocab-entry
				      :format-control "Incorrect sem specification: type missing" 
				      :format-arguments nil)))      
      ))
    ;;  add restriction if there's a variable type with no features
    ;;(when (and (is-variable-name type) (null feat))
    ;;(setq type `(? ,(strip-variable-sign type) f::phys-obj f::abstr-obj f::time f::situation)))   
     ;; return $ type features list
    (if (and (is-variable-name type) (null feat))
	;; return only a non-null variable if the features are NIL
	;(cons feature-list-sign (list '?!sem))
	;'?!sem ;; return non-null variable without the feature-list-sign as per jfa request 20121119
	(cond 
	  ((equalp (symbol-name semvar) "?ARGUMENTTYPE")
	   '?!argumentsem)
	  ((equalp  (symbol-name semvar) "?SUBCATTYPE") 
	   '?!subcatsem)
	  ((equalp  (symbol-name semvar) "?LSUBJTYPE") 
	   '?!subjsem)
	  ((equalp  (symbol-name semvar) "?LOBJTYPE") 
	   '?!dobjsem)
	  (t
	   '?!sem)
	  )	
;	  (setq type `(? ,(strip-variable-sign type) f::phys-obj f::abstr-obj f::time f::situation)))
	(cons feature-list-sign (cons type feat)))
    ))




