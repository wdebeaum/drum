;; Lexicon access functions

;; 2001 rewritten by Myrosia to clean up and reorganize

(in-package "LEXICONMANAGER")

;; From make-new-lex.lisp
(declaim (special *pos-defaults*))

(defvar *pos-defaults*     
    '((w::V ((w::morph (:forms (-vb)))                           ;;  default values
	  ;; grammatical categories for verb subcategorizations
	  (w::subj (% w::np (w::sem ($ ?sem))))
	  (w::iobj (% -)) ;; indirect object -- only for ditransitive verbs, otherwise use comp3
	  (w::dobj (% -)) ;; direct object
	  (w::comp3 (% -)) ;; verb complement phrases -- why is this comp3 and not comp?
	  (w::part (% -)) ;; for verb particles, as in "pick up"
	  (w::sem ($ f::Situation)) ;; all verbs have this semantic type
	  (w::VC +) ;; verb crossing foot feature is positive for all verbs; used for compatibility with Collins parses
;;	  (w::dys nil) ;; vector for dysfluency detection -- every word has this
	 ))
      (w::N ((w::morph (:forms (-S-3P)))                           ;;  default values
	 (w::SEM ?sem)
	 (w::LF ?LF) ;; why is this needed for nouns but not for other words?
	 (w::AGR W::3S) ;; why is this not a var?
	 (w::case ?cas)
	 (w::sort ?srt)
	 (w::mass ?mss)
;;	 (w::dys nil)
	 ))
      ;; swift 06/30/06 -- the dys feature is not currently used
;;      (w::adj ((w::dys nil)))
;;      (w::adv ((w::dys nil)))
;;      (w::quan ((w::dys nil)))
;;      (w::art ((w::dys nil)))
;;      (w::conj ((w::dys nil)))
;;      (w::fp ((w::dys nil)))
;;      (w::name ((w::dys nil)))
;;      (w::number-unit ((w::dys nil)))
;;      (w::ordinal ((w::dys nil)))
;;      (w::prep ((w::dys nil)))
;;      (w::pro ((w::dys nil)))
;;      (w::uttword ((w::dys nil)))
;;      (w::value ((w::dys nil)))
      ))

(defvar *var-prefix* "V")

(defvar *number-lexicon*
    '((w::zero 0)
      (w::oh 0 0.93 ((w::NoBareSpec +)) )
      (w::one 1) 
      (w::two 2)
   ;;   (w::to 2 0.9 ((w::NoBareSpec +))) 
      (w::three 3) (w::four 4) (w::five 5) (w::six 6) (w::seven 7) (w::eight 8)
      (w::nine 9) (w::ten 10) (w::eleven 11) (w::twelve 12) (w::thirteen 13) 
      (w::fourteen 14) (w::fifteen 15) (w::sixteen 16) (w::seventeen 17) 
      (w::eighteen 18) (w::nineteen 19)
      (w::twenty 20) (w::thirty 30) (w::forty 40) (w::fifty 50) (w::sixty 60)
      (w::seventy 70) (w::eighty 80) (w::ninety 90))) ;; (hundred 100) (thousand 1000) (million 1000000) these are defined in lexicon now

(defun lexicalize-number (val)
  (car (find-if #'(lambda (x) (eq (second x) val))
	   *number-lexicon*)))

(defun change-kr-factors (boost-up boost-down)
  (setq *kr-sense-boost* boost-up)
  (setq *no-kr-probability* boost-down)
  )

(defun replace-or-with-var (sem)
  "Given a sem in 'print' format, replace <OR> with ? var"
  (mapcar #'(lambda (x)
	      (if (and (listp x) (listp (second x)) (eql (first (second x)) '<OR>))
		  ;;		  `(,(first x) (? ,(gensym) ,@(cdr (second x))))
		  `(,(first x) (? ,(gentemp) ,@(cdr (second x))))
		x))
	  sem))

(defun retrieve-from-lex (word)
  ;;(format t "~% RETRIEVE-FROM-LEX: ~S" word)
  ";;------------------------
;; retrieve-from-lex (w)
;;
;; w        : word
;; returns  : the lex-entry structure for a lexeme or number -- if a number is not in the lexicon, 
;;            it is created and returned
;;
"
  (or 
      (if (numberp word)
	  (list (make-number-entry word word 1.0 nil)))
      (if (member word *number-lexicon* :key #'car)
	  (let ((def (find word *number-lexicon* :key #'car)))
	    (list* (make-number-entry (first def) (second def) (third def) (fourth def))
		   (get-word-lex-entries-from-DB word *lexicon-data*)))
	    )
      (get-word-lex-entries-from-DB word *lexicon-data*)
      ;; this variable and function are defined in dynamic-lexicon.lisp
      ;; which is loaded after this file because of other dependencies
      (if *use-dynamic-lexicon* (get-dynamic-lex-entries word)))
  )
    

(defun identify-digit (n)
  "Finds the relevant digit in teens and tens for sequences"
  (cond ((< n 10) '-)
        ((< n 19) (mod n 10))
        ((member n '(20 30 40 50 60 70 80 90)) (/ n 10))
        (t '-)))

;; 05/14/00 Myrosia added the "twodigit" classification for all numbers smaller than 100
;;   4/02 Added zero classification - zero isn't a digit! (e.., *twenty zero) JFA
(defun classify-n (n)
  "Given a number return a set of features that govern its use in dates and times, e.g. restrict
   numerical ranges for days, times, hours.
   n      : number
   return : list of range names to which the number belongs
"
  ;;  classifies numbers in ranges for clock times, etc.
  (cond ((= n 0) '(w::zero W::minute))
        ((<= n 9) '(w::digit w::hour12 w::day w::month w::minute))
	((<= n 12) '(w::twodigit w::teen w::hour12 w::minute w::day w::month))
        ((<= n 19) '(w::twodigit w::teen w::minute w::day))
        ((= n 20) '(w::twodigit w::tens w::hour24 w::minute w::day w::end-zero))
	((<= n 23) '(w::twodigit w::hour24 w::minute w::day))
        ((= n 30) '(w::twodigit w::tens w::minute w::day w::end-zero))
	((<= n 31) '(w::twodigit w::minute w::day))
        ((= n 40) '(w::twodigit w::tens w::minute w::end-zero))
        ((= n 50) '(w::twodigit w::tens w::minute w::end-zero))
	((< n 60) '(w::twodigit w::minute w::end-zero))
        ((= n 60) '(w::twodigit w::tens w::end-zero))
        ((= n 70) '(w::twodigit w::tens w::end-zero))
        ((= n 80) '(w::twodigit w::tens w::end-zero))
        ((= n 90) '(w::twodigit w::tens w::end-zero))
	((< n 100) '(w::twodigit w::end-zero))
	((and (>= n 100) (< n 1000)) 
	 (if (= (mod n 10) 0) 
	     '(w::threedigit w::end-zero)
	     '(w::threedigit)))
	((and (> n 1600) (<= n 2100)) 
	 (if (= (mod n 10) 0)
	     '(w::year w::end-zero)
	     '(w::year)))
	((= (mod n 10) 0)
	 '(w::end-zero))
	(t '-)))
  

(defun make-number-entry (word val prob feats &key (with-kr t))
  (let* ((has-digits (if (numberp word) '+ '-))
	 (entry (make-word-sense-definition
		 :pos 'w::number
		 :lf 'ont::number
		 :sem `,(make-typed-sem '(F::ABSTR-OBJ (F::INTENTIONAL -)
                          (F::INFORMATION F::DATA) (F::CONTAINER -)
                          (F::GRADABILITY -) (F::SCALE ?!sc)
                          (F::MEASURE-FUNCTION f::value)))		 
;;		 :sem (make-untyped-sem nil)
		 :boost-word t
		 ;;		 :pref (- (or prob 1.0)) 
		 ;; Myrosia 2004/6/17 changed to just default preference mechanism
		 ;; because all core words will be boosted now
		 :pref (or prob 1.0)
		 ;; the entry is marked as specialized because kr-type has been added to it 
		 :specialized t
		 :syntax (append feats `((w::mass (? massv w::count w::bare))
					 (w::agr ,(if (eql val 1) 'w::3s 'w::3p))
					 (w::ntype (? ntype ,(classify-n val)))
					 (w::digit ,(identify-digit val))
					 (w::val ,val)
					 (w::var ?nvar)
					 (w::lex ,(lexicalize-number val))
					 ;; allow numbers as bare specifiers so that the lfs for 'more than five trucks' and 'more than five' have the same representation for numbers
;;					 (w::nobarespec +)
					 (w::has-digits ,has-digits)
					 (W::QOF (% W::PP (W::PTYPE W::OF) (w::agr ?nagr)  (w::mass w::count) ;; three of the trucks
						   ))
					 ))
		 ;;:kr-type 'ont::number
		 )))
    (when (and with-kr *kr-in-lexicon*)
      ;; boost the word, because it does not go through the regular mechanism
      (setf (word-sense-definition-pref entry) (* *kr-sense-boost* (word-sense-definition-pref entry)))
      (add-kr-coercions entry))
    (make-lexicon-entry word entry)
    ))


;;  Main access function to lexicon databas

(defun get-word-lex-entries-from-DB (w lexicon-data)
  "returns all the possible lexical entries for the specified word.
   This is a list, each element being a LEX-ENTRY structure or a list for composite word entries"
  ;;(format t "~% GET-WORD-LEX-ENTRIES-FROM-DB: ~S" w)
  (let ((entries (getwordDefs w lexicon-data)))
    (if entries
       (let ((res (mapcan #'(lambda (e)
			      (gen-lexicon-constits w e lexicon-data))
			  entries)))
	;; (format t "~%GET-WORD-LEX-ENTRIES-FROM-DB returns ~S" entries)
	 res))
    ))


(defun getwordDefs (w lexicon-data)
  "returns vocabulary-entry structures"
  (mapcar (lambda (x)
	    (if (listp x)
		x
	      (list :none (gen-id w) x)))
	  (gethash w (lexicon-db-word-table lexicon-data))))

;; add numbers here instead of in retrieve-from-lex so we don't need to call the OM just for the definition
(defun getwordDefsnum (w lexicon-data)
  (append
     (mapcar (lambda (x)
	      (if (listp x) x
		(list :none (gen-id w) x)))
	     (gethash w (lexicon-db-word-table lexicon-data)))    
     (if (numberp w)
	 (list (make-number-entry w w 1.0 nil))
       (if (member w *number-lexicon* :key #'car)
	   (let ((def (find w *number-lexicon* :key #'car)))
	     (list (make-number-entry (first def) (second def) (third def) (fourth def)))
	     )))))
  
;;=======================================
;;
;;   Defining the database from the lexicon files

(defvar *lexicon-filter-predicate* nil
  "The lexicon can be filtered at load-time by setting this variable.
If this variable is non-NIL, it should be set to a function that returns
true if the given (ENTRY POS TEMPL TAGS) should be loaded.")

(defun set-lexicon-filter-tags (tags)
  "Enable load-time lexicon filtering by tag, using the given set of TAGS.
Sets *LEXICON-FILTER-PREDICATE* to a function that returns true if the
intersection of an entry's tags and these tags is non-empty."
  (setf *lexicon-filter-predicate*
	#'(lambda (word pos templ entry-tags)
	      (declare (ignore word pos templ))
	    (intersection entry-tags tags))))

;; A system-definition file can set TRIPS::*lexicon-filter-tags* to a list
;; of tags prior to loading the LexiconManager (as part of loading the TRIPS
;; system). If that variable is set, then we pass it to SET-LEXICON-FILTER-TAGS
;; here at load-time. Other ways of accomplishing this are possible, some of
;; which are probably less of a kludge than this one.
(when (and (boundp 'TRIPS::*lexicon-filter-tags*) TRIPS::*lexicon-filter-tags*)
  (set-lexicon-filter-tags TRIPS::*lexicon-filter-tags*))

;;(defun lexicon-define-words-new (pos wlist)
(defun define-words-in-db (pos templ boost-word tags wlist lexicon-data)
  "Given a list of words with a given part of speech, defines all words in it with that POS"
  (if (and templ (not (retrieve-template templ)))
      (Format t  "~%DEFINE-WORDS: Warning unknown template: ~S!!!!!!" templ))
  (mapcar #'(lambda (e)
	      (when (or (not *lexicon-filter-predicate*)
			(funcall *lexicon-filter-predicate* e pos templ tags))
		(define-a-word e pos templ boost-word lexicon-data))
	  )
          wlist) 
  )

(defun define-a-word (e pos templ boost-word lexicon-data)
  "Defines a word in the new lexicon format"
  (let ((entry (parse-vocab-table-entry e :pos pos :default-template templ :boost-word boost-word)))
    (if (vocabulary-entry-p entry)
	(add-entries-for-word entry lexicon-data)
	)))

(defun add-entries-for-word (entry lexicon-data)
  ;;(format t "~%~% adding entry ~S" entry)
  "This adds the word and all its morphological variants to the *lexicon-data* table.

  ;;; entry: the structure of type vocabulary-entry, a parsed representation of the data in the lexicon
  ;; The function does the following:
  ;;; - generates all morphological forms of entry based on the morphological features
  ;;; - for every morphological form, adds to the list of its definitions in lexicon-data the entry of the form
  ;;;    (<morph-variant> <entry-id> <entry-def>)
  ;;;    <morph-variant> is the tag for the specific morphological variation which generated the entry
  ;;;    <entry-id> is the unique id to identify the entry in the parses
  ;;;    <entry-def> is the copy of the entry which will later be used to generate the lexicon definition
  ;;; - If the entry contains a particle, the particle definition is also added to the table
"
  (when (vocabulary-entry-p entry)
    ;; put the base form into the base forms table for indexing
    (pushnew (vocabulary-entry-word entry) (lexicon-db-base-forms lexicon-data))
    ;; the lf-form for a word sense is not always specified -- this fills it in if it's missing
    (add-lf-form-to-senses (vocabulary-entry-senses entry) (vocabulary-entry-word entry))
    ;; get morphological feature specifications for each sense of this word
    (let* ((morphlist (sort-by-morph entry (lexicon-db-synt-table lexicon-data)))
	   (variants (mmapcan (lambda (x)
				;; generate all morphological variants for this word -- use defaults if none specified
	   			(generate-morph-variants (vocabulary-entry-word entry)
	   						 (or (first x) (get-morph-default (vocabulary-entry-pos entry)))
	   						 (second x))
				)
	   		      morphlist))	   
	   (word-table (lexicon-db-word-table lexicon-data))
	   )
      (if variants
	  (mapcar #'(lambda (x)
		     		      ;;(format t "~%ADDING variant: ~S ** ~S ~%" (first x) (second x))
		      (push  (second x)
			     (gethash (first x) word-table)))
		  variants)
	;; no variants (MORPH nil), so add it as it is
        (progn  (print-debug "~%Adding defn ~S" entry)
	  (push (list :none 
                    (gen-id (vocabulary-entry-word entry))
                    entry)
              (gethash (vocabulary-entry-word entry) word-table))))
      ;; Entry may have a particle. If this is the case, we have to add it to the vocab table, too.
      (when (vocabulary-entry-particle entry)
	(let* ((part (vocabulary-entry-particle entry))
	       (partdef (gethash part word-table))
	       )
	  (when (or (null partdef)
		    ;; this is a test that this entry is already defined as part of speech particle ('w::part)
		    ;; if not, we need to add it
		    ;; we look for POS information, which is in the vocabulary-entry structure, a third element 
		    ;; of every definition in the word table
		    (not (member 'w::part partdef :key #'(lambda (x) (vocabulary-entry-pos (third x)))))
		    )
	    (add-entries-for-word (make-vocabulary-entry :word part :pos 'w::part :boost-word t
							 :senses (list (make-sense-definition :pos 'w::part
											      :lf nil :nonhierarchy-lf t
											      :pref *no-kr-probability*
											      :templ 'no-features-templ
											      :boost-word t
											      ))
							 )
				  lexicon-data)
	    )))
      ;; add abbreviated form if there is one
      (when (vocabulary-entry-abbrev entry)
	  (add-entries-for-word (make-vocabulary-entry :word (vocabulary-entry-abbrev entry)
						       :pos (vocabulary-entry-pos entry)
						       :boost-word nil
						       :abbrev nil
						       :wfeats nil
						       :senses (add-abbrev-feature (mapcar #'copy-sense-definition (vocabulary-entry-senses entry))))
				lexicon-data)
	))
    ))

(defun add-abbrev-feature (senses)
  (mapcar #'(lambda (x) (push '(w::abbrev +) (sense-definition-syntax x))) senses)
  senses)

(defun get-morph-default (pos)
  "Returns the default morphology values for a part of speech"
  (case pos
    (w::v '(:forms (-vb)))
    (w::n '(:forms (-s-3p)))
    (w::ordinal '(:forms (-s-3p)))
    (otherwise '(:forms (-none)))))


(defun add-lf-form-to-senses (senses form)
  "Makes sure LF-FORM is defined in each sense:NB Destructive modification!"
  (let ((lf-form (make-into-symbol form)))
    (mapcar #'(lambda (s)
	        ;; Myrosia: only add lf-form if LF is not defined
                (if (and (null (sense-definition-lf-form s))
		         (null (sense-definition-lf s)))
		  (setf (sense-definition-lf-form s) lf-form))
                )
            senses)))

;;========================================================== A
;; precaution by myrosia Since morph can be re-defined in templates,
;; we re-sort every list of senses by the features defined there if
;; there was no global morph defined

;; swift
;; Morphological features are defined globally for a word like this:
;;
;; (wordfeats (W::morph (:forms (-vb) :past W::took :pastpart W::taken :ing W::taking)))
;;
;; Sometimes morphological features are defined only for a specific sense of a word
;; and sense-specific definition will override the general definition for the given sense.
;; For example, there is a sense of "go" that appears in a constructions like "go find it" "go do it", and this
;; must always be the base form, so this is specified within the sense definition like this:
;;
;; (SYNTAX (W::morph (:forms NIL)) (W::vform W::base))
;;
;; When :forms NIL, no variants are generated.
;;
;; If no morphological features are specified at all in the lexical entry, the default for
;; the part of speech is assumed. The only default paradigms we generate are the verb inflection
;; paradigm and plural forms for nouns

(defun sort-by-morph (entry synt-table)
  "Takes an entry, returns a list of entries, each with unique morph features"
  ;; get the word-level morphological specification
  (let ((wmorph (get-arg-value 'w::morph (vocabulary-entry-wfeats entry))))
    (if wmorph
	(list (list wmorph entry))
      (let* ((slist (separate-senses-by-morph (vocabulary-entry-senses entry) synt-table)))
	(if (eql (length slist) 1)
	    (list (list (caar slist) entry))
	  (mapcar #'(lambda (x)
		      (let ((centry (copy-vocabulary-entry entry)))
			(setf (vocabulary-entry-name centry) (gen-id (vocabulary-entry-word entry)))
			(setf (vocabulary-entry-senses centry) (second x))
			(list (first x) centry)
			))
		  slist)
	  ))
      
      )))

(defun separate-senses-by-morph (senses synt-table)
  "Given a list of sense definitions, sorts them into sublists such that each has entries with identical morph values"
  (let ((slist nil) (smorph nil) (mlist nil))
    (dolist (sense senses)
      (setq smorph 
	(or (get-arg-value 'w::morph (basic-sense-definition-syntax sense))
	    ;; here we check if there are morphological features defined in a template 
	    ;; need to check that we have a sense-definition; monroe-style domain-specific words don't have them & will break this
	    (and (sense-definition-p sense) (find-morph-features-in-template (sense-definition-templ sense) synt-table))))
      (setq mlist (assoc smorph slist :test #'equal))
      (if mlist
	  (push sense (second mlist))
	(push (list smorph (list sense)) slist))
      )
    slist
    ;;    (mapcar #'second slist)
  ))


(defun find-morph-features-in-template (name synt-table)
  "Looks at the template namedd in thge arg and returns the value of the MORPH feature if it exists"
  (let ((templ (gethash name synt-table)))
    (if (syntax-template-p templ)
	(get-arg-value 'w::MORPH (syntax-template-syntax templ)))))



;;=======================
;;    Code to generate morphological variants of a base form in the lexicon table
;;    To save time and space, we don't generate the actual modified entry until needed by the parser.
;;   Entries in the table are of the form (<morphfeat> <base entry>)
;;   This information is later used to generate the real lexicon entry
;;   for the word when needed by the parser

(defun generate-morph-variants (word features entry)
  "This generates the morphological variants of the given word and sets up values for the lexicon table that are of the form (<form> <base entry>) No change imade to the entry until the word is actually needed"
  (let ((forms (find-arg features :forms))	
	(formlist nil)
	)
    (if (null forms) 
	(setq formlist '(:none))
      ;; Myrosia's note: mmapcan is important here! This is my non-destructive version of mapcan that does not use append. Otherwise we get an infinite loop
      (setq formlist (mmapcan (lambda (form)
				(case form 
				  ;; normal verb forms
				  (-vb '(:12s123pbase :3s :ing :past :pastpart :nom :agentnom))
				  ;; normal noun forms
				  (-s-3p '(:sing :plur))
;;				  (-load '(:load :load-p)) ;; no longer used
				  ;; adjective forms
				  (-er '(:none :er :est))
				  (-ly '(:none :ly))
;;				  (-erdouble '(:none :erd :estd)) no longer used
				 ;; words that have no forms
				  (-none '(:none))
				  (otherwise
				   (lexiconmanager-warn "Unknown Morph FORM ~S in defn ~S" form entry)))
				)
			     forms))
      )
   
    (when (and (member ':pastpart formlist) (member ':past formlist))
      (when (and (null (find-arg features ':pastpart)) ;; don't generate separate entry for :pastpart if it's the same as :past (i.e. not irreg)
		 (null (find-arg features ':conflate-past))) ;; only add this once
	(setq formlist (remove ':pastpart formlist))
	(let ((wmorph (get-arg-value 'w::morph (vocabulary-entry-wfeats entry))))
	  (if wmorph (setf (vocabulary-entry-wfeats entry) (replace-feat (vocabulary-entry-wfeats entry) 'w::morph (append wmorph '(:conflate-past))))
	    (setf (vocabulary-entry-wfeats entry) '((W::MORPH (:FORMS (-VB) :conflate-past)))))
	  )
	))
    (if (member :nom features)
	(setq formlist (cons :noms formlist)))
    (if (member :agentnom features)
	(setq formlist (cons :agentnoms formlist)))
    ;; add the tag to generate the plural nom form as well if a NOM value is present
    ;;(format t "~%Formlist is ~S forms are ~S" formlist forms)
    (mapcar (lambda (f)
	      (gen-variant word f features entry))
	    (remove-duplicates formlist))    
    ))

(defun get-nom-templ-for-slot (word templ slot)
  (let* ((this-syn-templ (retrieve-template templ))
	 (mappings (syntax-template-mappings this-syn-templ))
	 (role-map (car (get-slot-for-name mappings slot)))
	 nom-templ)
    (when role-map (setq nom-templ (read-from-string (concatenate 'string "other-reln-" (symbol-name role-map) "-templ"))))
    (when (and nom-templ (not (retrieve-template nom-templ)))
      (print-debug "invalid template ~S for ~S substituting other-reln-theme" nom-templ word)
      ;;(setq nom-templ 'other-reln-theme-templ)
      )
    nom-templ)
  )

(defun gen-nom-senses (word senses)
  "This is the new gen-nom-senses -- we simply use the Verb entry, changing the POS and morph features"
    (declare (ignore word)) ; unused? --wdebeaum
  (mapcar #'(lambda (x)
	      `((lf-parent ,(sense-definition-lf-parent x))
		(templ ,(sense-definition-templ x)
		       ,(car  (sense-definition-params x)))))
		      ;;(if (sense-definition-sem x) (list (cons 'sem  (sense-definition-sem x))))))
	      
	  senses))

(defun gen-nomentry (word form entry)
   "This is the new method for generating nominalization - we preserve the V entry but change the
   POS and the morph features"
    (declare (ignore form)) ; unused? --wdebeaum
  (let* ((nom-senses (if word (gen-nom-senses word (vocabulary-entry-senses entry))))
	 (objpreps (or (find-arg (cadr (assoc 'W::morph (vocabulary-entry-wfeats entry))) :nomobjpreps)
		       '(w::of)))
	 ;; if nomsubjpreps is defined, used it, otherwise use default "by". It seems that
	 ;; the cases with idiosyncratic subject preps are verbs that do not allow "by"
	 (subjpreps (or (find-arg (cadr (assoc 'W::morph (vocabulary-entry-wfeats entry))) :nomsubjpreps)
			'(w::by)))
	; unused? --wdebeaum
	; (nom-lex (if (eq form :nom)
	;	      word
	;	      (add-suffix word "S")))
	 (nom-def (if word (cons word (list 
				       (cons 'senses (if objpreps
							 (mapcar #'(lambda (x) (cons (list 'syntax '(w::sort w::pred)
											   (list 'w::nomobjpreps (when (not (member objpreps '(- w::-)))
														   (list '? 'objp objpreps)))
											   (list 'W::nomsubjpreps (when (not (member subjpreps '(- w::-)))
														    (list '? 'subjp subjpreps)))
																     
											   )
										     x))
								 nom-senses)
							 nom-senses))))))
	 ;;(xx (format t "~%nom-senses=~S~%objpreps=~S  nom-def= ~S" nom-senses objpreps nom-def))
         (nom-entry (parse-vocab-table-entry nom-def :pos 'w::n :default-template 'other-reln-theme-templ))
         )
    (print-debug "defining nominalization ~S~%" nom-entry)
    ;; add word to the base form index so it can be accessed independently of the verb form
    (if nom-entry (pushnew word (lexicon-db-base-forms *lexicon-data*)))
   nom-entry)
  )


(defun gen-agentnomentry (word form entry)
   "This generates reuses the code to build NOM entries, with the additional feature :AGENTNOM +"
   (let* ((nom-entry (gen-nomentry word (case form (:agentnom :nom) (:agentnoms :noms)) entry)))
     (mapcar #'(lambda (x)
		 (setf (sense-definition-syntax x)
		       (cons '(W::Agent-nom +) (sense-definition-syntax x))))
	     (vocabulary-entry-senses nom-entry))
     
     nom-entry
  ))
 
(defun gen-variant (base form features entry)
  "takes one form (e.g., :3s :ing) and generates the entry"
  (let ((irreg (find-arg features form))
	(morph-variant nil)
	)
    ;; if compound noun, add suffix to last word
    (if (and (vocabulary-entry-remaining-words entry)
	     (eq form ':plur))
	(setq morph-variant (or irreg
				(gen-standard-morph-variant form (cons base (vocabulary-entry-remaining-words entry)))))
      (setq morph-variant (or irreg (gen-standard-morph-variant form base))))
    (cond
     ((symbolp morph-variant)
      (case form 
	(:nom
	 (if irreg ; irreg must be defined for :nom generation, as  the lexical item that denotes must be specified
	     (list morph-variant (list form (gen-id morph-variant) (gen-nomentry morph-variant form entry)))))
	(:noms
	 (let ((morph-variant (or irreg (add-suffix (find-arg features :nom) "S"))))
	   (list morph-variant (list form (gen-id morph-variant) (gen-nomentry morph-variant form entry)))))
	(:agentnom
	 (if irreg ;; a lexical value must have been defined in order to proceed
	     (list morph-variant (list form (gen-id morph-variant) 
				       (gen-agentnomentry morph-variant form entry)))))
	(:agentnoms
	 (let ((morph-variant (or irreg (add-suffix (find-arg features :agentnom) "S"))))
	   (list morph-variant (list form (gen-id morph-variant) (gen-agentnomentry morph-variant form entry)))))
	(otherwise  (list morph-variant (list form (gen-id base) entry)))))
     ((listp morph-variant)
      ;; the variant is itself a compound word
       (let ((nentry (copy-vocabulary-entry entry)))
	(setf (vocabulary-entry-word nentry) (first morph-variant))
	(setf (vocabulary-entry-remaining-words nentry) (rest morph-variant))
	(if (and (eq form ':nom) irreg) ; irreg must be defined for :nom generation
	    (list (first morph-variant) (list form (gen-id (first morph-variant)) (gen-nomentry morph-variant form nentry)))
	  (if (not (eq form ':nom)) (list (first morph-variant) (list form (gen-id base) nentry))))
	))
     )
    ))


(defun gen-standard-morph-variant (form base)
  "Generates a standard morphological variant"
  (case form
    (:none base)
    ;; verb forms
    (:12s123pbase base)
    (:3s (add-suffix base "S"))
    (:ing (add-suffix base "ING"))
    (:past (add-suffix base "ED"))
    (:pastpart (add-suffix base "ED"))
    ;; noun forms
    (:sing base)
    (:plur (add-suffix base "S"))
    (:load (add-suffix base "LOAD"))
    (:load-p (add-suffix base "LOADS"))
    ;; adjective forms
    (:er (add-suffix base "ER"))
    (:est (add-suffix base "EST"))
    (:erd (add-suffix (double-last-consonant base) "ER"))
    (:estd (add-suffix (double-last-consonant base) "DEST"))
    ;; adverb forms
    (:ly (add-suffix base "LY"))
    (otherwise base)
    ))
  
;;========================================================
;;   Code to generate lexicon entries for the parser from the lexicon table
;;

(defun gen-lexicon-constits (word lex-info lexicon-data)
  "produces a list of LEX-ENTRY structure for parser based on the data in
     the lexicon tables. If a composite entry it returns a list of form (COMPOSITE <remaining-words> <lex-entry>)"
  (let* ((sense-definitions (get-word-sense-definitions word lex-info lexicon-data))
	 )
	 ;; convert the senses from word-sense-definition format to lex-entry format
    (mapcar #'(lambda (e)
		(make-lexicon-entry word e))
	    sense-definitions)
    ))

(defun get-word-sense-definitions (word lex-info lexicon-data)
  "Takes a word, a lexical info including morphological features, and a lexicon-data structure. Returns a list of corresponding word-sense-definition structures"
  ;;(format t "~%GET-WORD-SENSE-DEFINTIONS: ~S" lex-info)
  (let* ((morphfeat (first lex-info))
         (name (second lex-info))
         (entry (construct-entry word name morphfeat (third lex-info)))
	 ;; retrieve the senses from the lexicon without KR
         (no-kr-senses (make-word-sense-definitions entry (lexicon-db-synt-table lexicon-data)))
	 )    
	 ;; if KR-in-lexicon is true, add kr information to the senses
    (if *kr-in-lexicon* 
	(add-kr-info no-kr-senses :allow-coercions t :kr-preference *kr-sense-boost* :no-kr-preference *no-kr-probability*) 
      no-kr-senses)
    ))
  
(defun make-lexicon-entry (name entry)
  "Constructs a LEX-ENTRY form using information specified in a WORD-SENSE-DEFINITION entry"
  (let* ((id (word-sense-definition-name entry)) ;; rule-id is never used
	 ;; -- why is it here?
;;	 (rule-id (list nil id))  ;; a fake rule so read-fv-pair can be used
	 (result-type (make-type-spec (cadr (assoc 'ont::result (word-sense-definition-roles entry)))))
         (prob (or (word-sense-definition-pref entry) *no-kr-probability*))
         (cat (word-sense-definition-pos entry))
	 (coerce (or (mapcar (lambda (x)
			       `(% w::coerce
				   (w::operator ,(word-sense-definition-operator x))
				   (f::kr-type ,(word-sense-definition-kr-type x))
				   (w::sem ,(make-type-spec (word-sense-definition-sem x) :semvar '?csem :feature-list-sign '$))
				   (w::lf ,(word-sense-definition-lf x))
				   ))
			     (word-sense-definition-coercions entry))
		     '-))
	 (feats1 `((w::LF ,(word-sense-definition-lf entry))
		  (w::SEM ,(make-type-spec (word-sense-definition-sem entry) :semvar '?sem :feature-list-sign '$))
		  (w::transform ,(word-sense-definition-transform entry))
		  (w::kr-type ,(word-sense-definition-kr-type entry))
		  (w::coerce ,coerce) ;; (? cv ,coerce))
		  ,@(word-sense-definition-syntax entry)
		  ;;,@(make-role-restrictions (word-sense-definition-roles entry))
		  ,@(build-synt-arguments (word-sense-definition-mappings entry) (word-sense-definition-roles entry))
		  ))
	 ;; add RESULT if in the ontology as its not usually in thw synt arguments
	 (feats (if result-type
		    (cons `(W::result ,result-type)
			  feats1)
		    feats1))
         (remaining-words (word-sense-definition-remaining-words entry))  ;; non-null for multi-word lex entries
         (name1 (if (null remaining-words)
		    (if (eq name 'w::do) 'w::DO-
			(if (consp name)
			    (let ((x (word-sense-definition-lf entry)))
			      (if (consp x)
				  (third x)
				  x))
			    name))
		    ;; we need to not use DO as the name  because of a printing bug in MCL for lists starting with DO
		    (let ((x (word-sense-definition-lf entry)))
		      (if (consp x)
			  (third x)
			  x))));;(cons name remaining-words)))
         )
    ;;  add default feature values if needed: 
    ;;     INPUT - the actual word
    ;;     LEX - if not specified, set to the word
    ;;     VAR - a variable to be bound when entry is retrieved.
    ;;     SEM.ARGSEM - make into variables unless they already are
    
    (setq feats (cons (list 'w::input name1) feats))
    (if (not (get-fvalue feats 'w::LEX)) 
	(setq feats (cons (list 'w::LEX name1) feats)))
    (if (not (get-fvalue feats 'w::VAR))
	;(setq feats (cons (list 'w::VAR (gentemp "V" *parser-package*))
	(setq feats (cons (list 'w::VAR (gentemp *var-prefix* *parser-package*))
			    feats))

	)
    
    ;; add other defaults based on POS
    (setq feats (add-default-features-for-POS feats cat))
    (make-lex-entry :id id :words (cons name remaining-words) :pref prob :description (cons cat feats)
		    :boost-word (word-sense-definition-boost-word entry))
    ))
    
               

(defun add-default-features-for-POS (feats pos)
  (let ((defaults (get-arg-value pos *pos-defaults*))
	)
    (mapcar #'(lambda (fv)
                (if (not (assoc (car fv) feats))
                  (setq feats (cons fv feats))))
            defaults)
    feats))

(defun construct-entry (word name morphfeat entry)
  "Returns a new entry with the appropriate features for this morphological feature"
  (cond
   ((vocabulary-entry-p entry)
    (let ((new-entry (copy-vocabulary-entry entry)))
      (setf (vocabulary-entry-word new-entry) word) 
      (setf (vocabulary-entry-name new-entry) name)
      (if (member morphfeat '(:load :loads :ly :er :est))
	  (do-complex-modify word morphfeat new-entry)
	;; or do a simple modify which is just changes to the SYNTAX feature
	(progn
;	  (print-debug "constructing entry for ~S ~S ~%" word entry)
          (setf (vocabulary-entry-wfeats new-entry)
	    (generate-morph-variant-features word morphfeat (vocabulary-entry-wfeats new-entry)))
          new-entry))))
   ((word-sense-definition-p entry)
;;;    (let ((new-entry (copy-word-sense-definition entry)))
;;;      (when (< (word-sense-definition-pref entry) 0)
;;;	(setf (word-sense-definition-pref new-entry)
;;;	  (- (* (word-sense-definition-pref new-entry) 
;;;		(if *kr-in-lexicon* *kr-sense-boost* *no-kr-probability*)))
;;;	  ))
;;;      new-entry))
    entry)
   (t
    (lexiconmanager-warn "Illegal entry found for word ~S: ~S" word entry))))

(defun do-complex-modify (word morphfeat entry)
  "This generate new entries in which any aspect of the change may be changed"
  (let ((new-entry (copy-vocabulary-entry entry)))
    (setf (vocabulary-entry-senses new-entry)
          (mapcar #'copy-sense-definition (vocabulary-entry-senses new-entry)))
    (case morphfeat
      (:load
       (do-load-modify morphfeat new-entry 'w::3s))      
      (:load-p 
       (do-load-modify morphfeat new-entry 'w::3p))
      (:ly
       (make-adverbial-from-adj word new-entry)
       )
      ((:er :est)
       (make-comparative new-entry morphfeat))
      )
    new-entry
    )
  )
     
  
(defun do-load-modify (morphfeat new-entry agr)
  (declare (ignore morphfeat))
  (setf (sense-definition-syntax new-entry)
        (replace-feat 
         (replace-feat 
          (replace-feat (sense-definition-syntax new-entry) 'w::sem '($ f::abstr-obj))
          'w::sort 'w::unit)
         'w::agr agr))
   ;; working here - what do we update?
  )

(defun make-adverbial-from-adj (word entry)
  "This destructively creates an ADV entry from an ADJ entry
   This function is invoked by declaring the -ly in the morphological features vector of a lexical entry for an adjective.
   It creates an ADV entry with a default template for verb modification.
"
  (setf (vocabulary-entry-pos entry) 'w::ADV)
  (mapcar #'(lambda (s)
              (setf (sense-definition-pos s) 'w::ADV)
	      ;; need to update the lfform with the right form of the word
	      (setf (sense-definition-lf-form s) word)
	      (setf (sense-definition-templ s) 'pred-s-vp-templ) ;; requires an :of role
              (setf (sense-definition-params s) nil)
              )
          (vocabulary-entry-senses entry))
  entry
  )

(defun make-comparative (entry feat)
  (setf (vocabulary-entry-wfeats entry)
	(add-er-or-est-feats (vocabulary-entry-wfeats entry) feat))
  (let ((senses (vocabulary-entry-senses entry))
	)
    (setf (vocabulary-entry-senses entry)
	  (mapcar #'(lambda (x)
		      (let* ((lf-parent (sense-definition-lf-parent x))
			     (orig-templ (sense-definition-templ x))
			     (fscale (car (get-sem-feature-value lf-parent 'f::scale)))
			     (wfeats (vocabulary-entry-wfeats entry))
			     ;;(this-sem (sense-definition-sem x))			    
			     ;;(orientation (cadr (assoc 'f::orientation this-sem)))
			     (templ (retrieve-template (sense-definition-templ x)))
			     (templ-feats (remove-if #'(lambda (x) (eq (car x) 'w::SUBCAT)) ; e.g., to remove (subcat -) from adj-theme-templ
						     (if (syntax-template-p templ)
							 (syntax-template-syntax templ))))
			     (comp-op (or (cadr (assoc 'w::comp-op wfeats))  ; in lexical entry
					  (cadr (assoc 'w::comp-op templ-feats)))) ; in template
			     )
				
			(case comp-op 
			  ((ont::less w::less)
			   (setf (sense-definition-lf-parent x)
				 (case feat
				   (:er 'ONT::less-VAL)
				   (:est 'ONT::min-VAL))))			      
			  (otherwise
			   (setf (sense-definition-lf-parent x)
				 (case feat
				   (:er 'ONT::more-VAL)
				   (:est 'ONT::max-VAL)))))
			(setf (sense-definition-templ x)
			      (case feat
				(:er (if (assoc 'subcat (syntax-template-mappings templ))
					 'COMPAR-TWOSUBCATS-TEMPL 'COMPAR-TEMPL))
				(:est (if (assoc 'subcat (syntax-template-mappings templ))
					 'SUPERL-TWOSUBCATS-TEMPL 'SUPERL-TEMPL))
				))
			(setf (sense-definition-syntax x)
			      (list* (list 'W::FUNCTN `,fscale)
				     (list 'W::FIGURE-SEM (get-figure-sem-from-type lf-parent
										    orig-templ))
				     (append (sense-definition-syntax x) templ-feats))))
			x)
		senses)))
  entry)

(defun get-figure-sem-from-type (type name)
    (declare (ignore name)) ; unused? --wdebeaum
  (let* (;;(templdef (gethash templ-name (lexicon-db-synt-table *lexicon-data*)))
	 (descr (get-lf-description type))
	 (fig-arg (find-if #'(lambda (x)
			    (eq (sem-argument-role x) 'ont::figure))
			(if (lf-description-p descr)
			    (lf-description-arguments descr))))
	 (restriction (sem-argument-restriction fig-arg)))
    (list (feature-list-type restriction) (feature-list-features restriction))))
		  

(defun build-var-description (v feature-list)
  " build-var-description (v feature-list) constructs a variable description from the input
    v            : a variable, e.g. x, y
    feature-list : list of syntactic features, e.g. (base pres)
 
    (build-var-description 'x '(base pres) returns (? x (base pres))"
  ;; Adding gentemp to avoid variable naming conflicts with new lexiconmanager code
  ;; because variables built with build-var are now subjected to general
  ;; variable naming controls, which were bypassed in the old code
  ;; when the lexicon was part of the parser
    (list '? (gentemp (symbol-name v)) feature-list))

; (W::MORPH (:FORMS (LEXICONMANAGER::-VB) :CONFLATE-PAST))
(defun conflate-past (feats)
  (let ((wmorph (get-arg-value 'w::morph feats))
	(res nil))
    (setq res (find ':conflate-past wmorph))
    res)
  )

(defun generate-morph-variant-features (word morphfeat syntaxfeatures)
  "generates the appropriate syntactic features for the morphological variant indicated
     by the morphfeat. The feature uniquely identifies the part of speech and the part
     of speech doesn't change. i.e., we can't use this function for generate an adverb from
     an adjective"
 (let ((feats (remove 'w::morph
		       syntaxfeatures
		       :test #'(lambda (feat pair)
				 (eq feat (car pair))))))
   (case morphfeat
     ;;  any word with a full entry already
     (:none
      feats)
     ;; verbs
     (:12s123pbase
      (append feats
              (list (list 'w::vform
                          (build-var-description 'x '(w::base w::pres)))
                    (list 'w::agr
                          (build-var-description 'y '(w::1s w::2s w::1p w::2p w::3p)))
                    )))
     (:3s
      (append feats 
              (list (list 'w::vform
                          (build-var-description 'x '(w::pres)))
                    '(w::agr w::3s)
                    )))
     (:ing 
      (append feats
              (list  '(w::vform w::ing)
		     (list 'w::agr (build-var-description 'w::agr '(w::1s w::2s w::3s w::1p w::2p w::3p)))
                     )))
     (:past
      ;; check for combining :past and :pastpart entries
      (if (conflate-past syntaxfeatures)
	  (append feats
		  (list (list 'w::vform
			      (build-var-description 'x '(w::past w::pastpart)))
			(list 'w::agr (build-var-description 'w::agr '(w::1s w::2s w::3s w::1p w::2p w::3p)))
			))
	(append feats
		(list (list 'w::vform
			    (build-var-description 'x '(w::past)))
		      (list 'w::agr (build-var-description 'w::agr '(w::1s w::2s w::3s w::1p w::2p w::3p)))
		      ))))
     (:pastpart
      (append feats
              (list '(w::vform w::pastpart)
		     (list 'w::agr (build-var-description 'w::agr '(w::1s w::2s w::3s w::1p w::2p w::3p)))
		    )))
     
     ;;  noun derivatives
     (:sing
      (replace-feat feats 'w::agr 'w::3s))
     ((:plur :noms :agentnoms)
      (replace-feat feats 'w::agr 'w::3p))
     
     ;;adjective derivatives
     (:er
      (add-er-or-est-feats feats 'er))
     (:est 
      (add-er-or-est-feats feats 'est))
   
     (-ly nil)

     ; nominalizations - has a separate entry generation function as more than the syntactic features change
     ((:nom :agentnom) nil) 
     
     (otherwise 
      (lexiconmanager-warn "Unknown morphological feature ~S in definition of ~S" morphfeat word)
      nil)))
 )

(defun add-er-or-est-feats (oldfeats er-or-est)
  ;; 
  (let ((lf (get-fvalue oldfeats 'w::lf))
	;; op and scale are not currently used
;;	(op (get-fvalue oldfeats 'w::comp-op))
;;	(scale (get-fvalue oldfeats 'w::pred))
	)
    (if (null lf)
	(setq lf (get-fvalue oldfeats 'w::lex)))
    (append
     `((w::COMPARATIVE ,(case er-or-est (:er '+) (:est 'w::SUPERL)))
      ) oldfeats)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Myrosia's stuff to check/count all sense defs
;;

(defun make-word-sense-defs (word entry)
  "Makes word sense definition out of an entry, without generating morph variants"
  (when (vocabulary-entry-p entry)
    (gen-lexicon-constits word (list :none 'w::id entry) *lexicon-data*)
    ))


(defun list-all-words (lexicon-data)
  (let ((res nil))
    (maphash (lambda (key val)	       
	       (declare (ignore val)) ;;?? val is never used -- why is it here? -- because maphash requires 2 arguments, and we are using only keys
	       (pushnew key res))
	     (lexicon-db-word-table lexicon-data))
    res))

(defun getworddef (word)
  (let ((entries (getwordDefs word *lexicon-data*)))
    ;;(format t "~%Entries are ~S" entries)
    (mapcan #'(lambda (e)
		(get-word-sense-definitions word e *lexicon-data*))
	    entries)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexicon functions from Core-parser/lex-functions.lisp

;;  "S" and "E..." SUFFIXES
;;  if a compound word, it adds the suffix to the last word

(defun add-suffix (word suffix)
  (if (consp word)
      (add-suffix-to-last-word word suffix)
    (if (symbolp word)
	(let* ((wordstring ;;(symbol-name word))
		(format nil "~S" word)) ;; Myrosia changed to keep package names
	       (rev-letters (reverse (coerce wordstring 'list)))
	       (last-letter (car rev-letters))
	       (second-last-letter (second rev-letters))
	       (third-last-letter (third rev-letters))
	       (fourth-last-letter (fourth rev-letters))
	       (all-but-last (coerce (reverse (cdr rev-letters)) 'string))
	       (first-suffix-letter (car (coerce suffix 'list)))
	       (second-suffix-letter (second (coerce suffix 'list))))
	  ;;        (intern
	  (read-from-string ;; Myrosia changed to keep package names
	   (case last-letter
	     ;;  switch "Y" to "I" when needed
	     (#\Y
	      (if (vowel second-last-letter)
		  (concatenate 'string wordstring suffix)                 ;; e.g., joys
		;;  word ends consonant "y"
		(case first-suffix-letter 
		  (#\S (concatenate 'string all-but-last "IE" suffix))  ;; e.g.,   carries
		  (#\I (concatenate 'string wordstring suffix))        ;; e.g., carrying
		  (otherwise (concatenate 'string all-but-last "I" suffix)))))   ;; e.g.,   carried
           ;;  Drop final "E" when suffix begins with "E" or "I",	     
	     (#\E
	      (case first-suffix-letter 
		((#\I)
		  ; but not for words ending in "ee", e.g. agreeing
		 (if (eql second-last-letter #\E)
		     (concatenate 'string wordstring suffix)
		   (concatenate 'string all-but-last suffix)));; e.g., caring
		((#\E)
		 (concatenate 'string all-but-last suffix));; e.g., cared, agreed
		((#\L) ;; able + ly -> ly
		 (if (and (eql second-suffix-letter #\Y) (eql fourth-last-letter #\A)
			  (eql third-last-letter #\B) (eql second-last-letter #\L))
		     (concatenate 'string all-but-last "Y")           ;; e.g., reliably
		   (concatenate 'string wordstring suffix)))
		(otherwise
		 (concatenate 'string wordstring suffix))))           ;; e.g., cases
	     ;;  Change "S" to "ES" after "H", "S" or "X", but not "GH" or "TH" or "PH"
	     ((#\H #\S #\X)
	      (if (and (eql first-suffix-letter #\S) (not (or (eql second-last-letter #\T) (eql second-last-letter #\G)  (eql second-last-letter #\P))))
		  (concatenate 'string wordstring "ES")                 ;; e.g., kisses
		(concatenate 'string wordstring suffix)))          ;; e.g., kissed
	     ;;  add "k" to final "c"
	     ((#\C)
	      (if (or (eql first-suffix-letter #\E)(eql first-suffix-letter #\I))
		  (concatenate 'string wordstring "K" suffix)
		(concatenate 'string wordstring suffix))); e.g. panicked, frolicking		
	     ;; Double certain final consonants when suffix begins with "E" or "I"
	     ;; and the word ends in 1 vowel followed by 1 consonant -- doesn't apply to e.g. stream, lean, gleam
	     ;; verbs ending in double consonants are not affected	  
	     ((#\B #\D #\G #\L #\M #\P #\R #\T)
	      (if (and  (member first-suffix-letter '(#\I #\E))
			(vowel second-last-letter)
			(not (vowel third-last-letter))
			)
		  (concatenate 'string wordstring (coerce (list last-letter) 'string)
			       suffix)  ;; e.g., bagged
		(concatenate 'string wordstring suffix))) ;; e.g., bags
	      ((#\N)
	       (if (and   (member first-suffix-letter '(#\I #\E))
			  (vowel second-last-letter)
			  (not (vowel third-last-letter))
			  (not (eql second-last-letter #\E)))    ;; verbs ending in en (listening, happening) are not affected
		   (concatenate 'string wordstring (coerce (list last-letter) 'string)
				suffix)  ;; e.g., grinned, grinning
		(concatenate 'string wordstring suffix))) ;; e.g., grins
	     (otherwise
	      (concatenate 'string wordstring suffix))
	     ))
	  )
      ;; wasn't a symbol, so return unchanged
      word)
    ))

(defun add-suffix-to-last-word (words suffix)
  "adds the suffix to the last word in the sequence"
  (if (null (cdr words))
    (list (add-suffix (car words) suffix))
    (cons (car words)
          (add-suffix-to-last-word (cdr words) suffix))))
    

(Defun vowel (letter)
  (member letter  (list  #\A #\E #\I #\O #\U)))

(defun double-last-consonant (word)
  (let* ((wordstring (coerce (symbol-name word) 'list))
         (last-letter (car (reverse wordstring))))
    (intern (concatenate 'string wordstring (list last-letter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feature handling functions from Core-parser/FeatureHandling.lisp

(defun replace-feat (feats feat val)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond ((null feats) (list (list feat val)))
        ((eq (caar feats) feat) (cons (list feat val) (cdr feats)))
        (t (cons (car feats) (replace-feat (cdr feats) feat val)))))

; This gets the value from a feature-value list
(defun get-fvalue (featlist feature)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (symbolp feature)
    (cadr (assoc feature featlist))
    (cadr (assoc feature featlist :test #'equal))
  ))
