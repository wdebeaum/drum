;; lexicon-functions.lisp
;;
;; Mary Swift
;;
;; Utility functions used in lexicon-interface.lisp
;;

(in-package "LEXICONMANAGER")

;; used in get-word-def-string to filter out lxm package prefix in word definition passed to parser
(defvar *lxm-package-var* (find-package :lexiconmanager))
(defvar *use-dynamic-lexicon* nil) ;; t if dynamic lexicon is in use

;; --------------------------------
;; a sample lexical entry structure for the word 'and'
;; #S(LEX-ENTRY :WORDS (AND) :PREF 1.03
;;              :DESCRIPTION
;;              (CONJ (VAR V217630) (LEX AND) (INPUT AND) (LF AND) (SEM ($ -)) (TRANSFORM NIL) (COERCE -)
;;               (SUBCAT (? SUB S PP NP VP)) (CONJ +) (SEQ +))))


;;------------------------
;; lex-entry-cat (lexentry)
;;
;; lexentry  : lexical entry structure 
;; returns : the lexical category (e.g. N, V, ADJ, ADV, CONJ), which is first in the description
;; 
(defun lex-entry-cat (lexentry)
  (first (lex-entry-description lexentry))
  )

;;------------------------
;; lex-entry-feats (lexentry)
;;
;; lexentry  : lexical entry structure
;; returns   : the rest of the lexical entry description, which is a list of all features (e.g. morph, agr, sem) 
;;             and attributes (e.g. lf, roles) for this entry
;; 
(defun lex-entry-feats (lexentry)
  (rest (lex-entry-description lexentry))
  )

;;------------------------
;; get-feature-values (feat-list feat-val)
;;
;; feat-list  : list of all features in the lexical definition (i.e. what (lex-entry-feats lexdef) returns)
;; feat-val   : a specific feature category, e.g. AGR, LF...
;; returns    : the feature(s) for the requested category
;; 
(defun get-feature-values (feat-list feat-val)
  (second (assoc feat-val feat-list))
  )

;;------------------------
;; strip-out-lf (lf)
;;
;; lf      : (* LF::VEHICLE w::truck)
;; returns : LF::VEHICLE
;;
" extract just the lf::X from the lf description created with establish-lf
Historical note: the only reason we need to do this is because not ALL lfs get put in this (:* ..) format -- sometimes it's just LF::X (for reasons not entirely clear at this time), so we maintain a level of uniform representation in the interface
"
(defun strip-out-lf (lf)
  (if (listp lf) (second lf)
    lf)
  )

;;------------------------
;; listify-lex-entry (wdef)
;;
;; wdef    : lex-entry structure
;; returns : list of lex entry structure
;;
(defun listify-lex-entry (wdef)
  (mapcar (lambda (def)
	    (list (lex-entry-id def) (lex-entry-words def) (lex-entry-pref def) (cons '% (lex-entry-description def))))
	  wdef)
  )

;; not used?
;;------------------------
;(defun getnumberDef (w)
;  "
;;; getnumberDef (w)
;;;
;;; w       : a word
;;; returns : the lex-entry if it's a number (2) or number word (two); otherwise nil
;;; 
;"
;  (let ((return-list nil))
;    (if (numberp w)
;      (setq return-list (list (make-number-entry w w 1.0 nil :with-kr nil)))
;    (if (member w *number-lexicon* :key #'car)
;        (let ((def (find w *number-lexicon* :key #'car)))
;          (setq return-list (list (make-number-entry (first def) (second def) (third def) (fourth def) :with-kr nil)))
;          )
;      )
;    )
;    return-list)
;  )

;;------------------------
;; get-metadata (w &key lftype pos key)
;;
;; returns  : the metadata vector for the word sense and pos; if keyword is specified (e.g. :wn for wordnet) , returns just the value for that keyword
;; 
(defun get-metadata (w &key (lftype nil) (pos nil) (key nil))
    (let ((res nil))  
      (dolist (def  (getwordDefs w *lexicon-data*))
	(let* ((x (third def))
	       (this-pos (vocabulary-entry-pos x))
	       )
	  (cond ((and pos lftype)
		 (if (eq pos (vocabulary-entry-pos x))
		     (dolist (s (vocabulary-entry-senses x))
		       (when (eq (sense-definition-lf-parent s) lftype)
			 (if key (pushnew (list w lftype pos (get-keyword-arg (sense-definition-meta-data s) key)) res :test #'equal)
			   (pushnew (list w lftype pos (sense-definition-meta-data s)) res :test #'equal))
			 ))))
		(pos ;; all senses for this pos
		 (if (eq pos (vocabulary-entry-pos x))
		     (dolist (s (vocabulary-entry-senses x))
		       (if key (pushnew (list w (sense-definition-lf-parent s) pos (get-keyword-arg (sense-definition-meta-data s) key)) res :test #'equal)
			 (pushnew (list w (sense-definition-lf-parent s) pos (sense-definition-meta-data s)) res :test #'equal))
		       )))
		(lftype ;; all senses for this lftype
		 (dolist (s (vocabulary-entry-senses x))
		   (when (eq (sense-definition-lf-parent s) lftype)
		     (if key (pushnew (list w lftype this-pos (get-keyword-arg (sense-definition-meta-data s) key)) res :test #'equal)
		       (pushnew (list w lftype this-pos (sense-definition-meta-data s)) res :test #'equal))
		     )))
		(t ;; all senses for this word
		 (dolist (s (vocabulary-entry-senses x))
		   (if key (pushnew (list w (sense-definition-lf-parent s) this-pos (get-keyword-arg (sense-definition-meta-data s) key)) res :test #'equal)
		     (pushnew (list w (sense-definition-lf-parent s) this-pos (sense-definition-meta-data s)) res :test #'equal))
		   )))
	  ))
      res)
    )

;;------------------------
(defun retrieve-from-vocabulary (w)
"
;; retrieve-from-vocabulary (w)
;;
;; w       : word
;; returns : the vocabulary entry structure for this word
;;
;; The vocabulary-entry structure contains only information from the lexicon data files
"
  (getwordDefs w *lexicon-data*)
  )

;;------------------------
(defun get-vocabulary-entry (e)
  "
;; get-vocabulary-entry (e)
;;
;; e       : vocabulary entry structure
;; returns : the description -- the main part where all the info is
;;
"
  (third (car e))
  )

(defun get-agr (w form wfeats)
  "
;; get-agr (w form)
;;
;; w       : word (e.g. truck)
;; form    : form category, e.g. :sing, :plur, :past
;; returns : list of agreement, e.g. (AGR 3S) for truck, (AGR 3P) for trucks
;; or nil if no agr features exist
;;
;;  (generate-morph-variant-features 'leave :12s123pbase 'nil)
;;  ((VFORM (? X (BASE PRES))) (AGR (? Y (1S 2S 1P 2P 3P))))
"
  (if (eq form ':none) ;; fixed word form e.g. 'be' -- have to extract agr from wordfeats list
      (assoc 'w::agr wfeats)
    (assoc 'w::agr (generate-morph-variant-features w form 'nil))
    )
 )

(defun get-be-form (formcat agr)
      "
;; get-be-form (w formcat agr)
;;
;; formcat : a verb form category: :12S123PBASE (base and present) 
;;                                 :3S (3rd singular present) 
;;                                 :PAST, :PASTPART, (also passive) :ING
;; agr     : person and number agreement: 1S, 2S, 3S, 1P, 2P, 3P
;; returns : the appropriate form. If agr is nil for a past or present, return the list of
;;         : possible forms
;;
  "
      ;; we assume we have a form of 'be' already, since (is-be word) is called before
      (case formcat
	(:base '(w::be))  
	(:ing '(w::being))  
	(:pastpart '(w::been)) ;; passive or pastparticiple
	(:past ;; was were
	 (cond ((member agr '(w::1s w::3s)) '(w::was))
	       ((member agr '(w::2s w::1p w::2p w::3p)) '(w::were))
	       (t '(w::was w::were))
	       )
	 )
	(:pres ;; am, are, is
	 (cond ((eq agr 'w::1s) '(w::am))
	       ((eq agr 'w::3s) '(w::is))
	       ((member agr '(w::2s w::1p w::2p w::3p)) '(w::are))
	       (t '(w::am w::are w::is))
	       )
	 ))
      )

(defun is-be (w)
    "
;; is-be (w)
;;
;; w       : word, e.g. 'be
;; returns : T if w is a form of 'be'; otherwise NIL
;;
  "
  (member w '(w::be w::am w::^m w::is w::are w::^s w::^re w::was w::were w::being w::been))
  )

(defun get-agr-values (wagr)
  "
;; extract-agr-values (wagr)
;;
;; wagr    : word agreement features. This can be a variable list, e.g. (AGR (? Y (1S 2S 1P 2P 3P)))
;;           note that fixed forms may have the structure (AGR (? AGR 1S 3S)) 
;; returns : a list with only the agr values, e.g. (1S 2S 1P 2P 3P), or (3S)
;;
  "
  (let ((varlist (second wagr)))   
    (if (listp varlist)
	(let ((agrvals (third varlist)))
	  (if (listp agrvals) agrvals
	    (list agrvals)))
      (list varlist) ;; otherwise return the value in a list, e.g. (3S)
      )
    )
  )

(defun get-all-lemmas ()
    " 
;;
;; returns : list of all base forms defined in the lexicon
;; NB this doesn't include compound words or particles as separate so it's
;; only used for indexing into the lexicon, where compounds and
;; particle verbs are indexed by the first word
;; 
"
  (let ((res nil))
    (dolist (w (lexicon-db-base-forms *lexicon-data*))
      (pushnew w res)
      )
    res))


(defun get-lf-types-for-category (pos)
  " 
;;
;; pos       : part of speech e.g. w::v
;; returns : list of lf types for all words in selected pos category 
;; 
"
  (let ((return-list nil))
    ;; for every base form in the lexicon
    (dolist (word  (lexicon-db-base-forms *lexicon-data*))
      ;; get the definition
      (dolist (def  (getwordDefs word *lexicon-data*))
	(let* ((x (third def)))
	  ;; if it matches the pos, add any new lf types to the list
	  (when (eq pos (vocabulary-entry-pos x))
	    (dolist (s (vocabulary-entry-senses x))
	      (pushnew (sense-definition-lf-parent s) return-list :test #'equal)
	      )
	    )
	  )
	)
      )
    return-list)
  )

(defun get-trips-mapped-thematic-roles ()
  " 
;;
;; returns : list of all thematic roles for verbs in TRIPS lexicon
;; 
"
  (let ((return-list nil))
    ;; for every base form in the lexicon
    (dolist (w (lexicon-db-base-forms *lexicon-data*))
      ;; get the definition
      (dolist (wdef (getwordDef w))
	;; when we have a verb definition
	(when (eq (word-sense-definition-pos wdef) 'w::v)
	  ;; add every (new) mapped role to the list
	  (dolist (role-map (word-sense-definition-mappings wdef))
	    (let ((synsem-map (second role-map)))
	    (pushnew (word-synsem-map-slot synsem-map) return-list :test #'equal)
	    ))
	  )))
      (sort return-list #'string-lessp))
  )


(defun get-trips-role-mapping-pairs ()
  " 
;;
;; returns : list of all role-constituent pair mappings for verbs in TRIPS lexicon
;; 
"
  (let ((return-list nil))
    ;; for every base form in the lexicon
    (dolist (w (lexicon-db-base-forms *lexicon-data*))
      ;; get the definition
      (dolist (wdef (getwordDef w))
	;; when we have a verb definition
	(when (eq (word-sense-definition-pos wdef) 'w::v)
	  ;; add every (new) mapped role to the list
	  (dolist (role-map (word-sense-definition-mappings wdef))
	    (let ((synsem-map (second role-map)))
	    (pushnew (list (word-synsem-map-syntcat synsem-map)
			   (word-synsem-map-slot synsem-map)
			   )
			   return-list :test #'equal)
	    ))
	  )))
      (sort return-list #'string-lessp :key #'cadr))
  )

(defun get-trips-thematic-roles ()
  " 
;;
;; returns : list of all thematic roles for verbs in TRIPS lexicon
;; this function gets the roles from the ROLES list in the word definitions
;; so it includes all roles defined for the word's lf type, even if they're not
;; explicitly mapped via templates -- to see only mapped roles use
;; get-trips-mapped-thematic-roles
;; 
"
  (let ((return-list nil))
    ;; for every base form in the lexicon
    (dolist (w (lexicon-db-base-forms *lexicon-data*))
      ;; get the definition
      (dolist (wdef (getwordDef w))
	;; only do this for verbs
	(when (eq (word-sense-definition-pos wdef) 'w::v)
	  (dolist (role (word-sense-definition-roles wdef))
	    (pushnew (car role) return-list :test #'equal)
	    ))
	))
    (sort return-list #'string-lessp))
  )

(defun get-thematic-roles-and-restrictions ()
  " 
;;
;; returns : list of all thematic roles and their restrictions for verbs in TRIPS lexicon
;; 
"
  (let ((return-list nil))
    ;; for every base form in the lexicon
    (dolist (w (lexicon-db-base-forms *lexicon-data*))
      ;; get the definition
      (dolist (wdef (retrieve-from-lex w))
	;; only do this for verbs
	(when (eq (lex-entry-cat wdef) 'w::v)
	  ;; this is the structure we get for roles 
	  ;; (% w::roles (role1 ...) (role2 ...) etc)
	  (let ((roles (get-feature-values (lex-entry-feats wdef) 'w::roles)))
	    (if (listp roles)
		(dolist (r roles)
		  (when (listp r)
		    (let ((role (car r))
			  (restr (cadr r))
			  )
		      (when (listp restr)
			(let ((semtype (second restr)))
			  ;; if type is a variable then there are no restrictions specified
			  (cond ( (is-variable-name semtype)
				  (pushnew (list role) return-list :test #'equal)
				  ;;    (setq restr (substitute 'f::x (second restr) restr)))
				  )
				;; if type is a list, it's a variable that ranges over more than one type
				;; we standardize the representation for output purposes
				((listp semtype)
				 (setf semtype (substitute 'f::? (first semtype) semtype))
				 (setf semtype (remove (second semtype) semtype))
				 (setf restr (substitute semtype (second restr) restr))
				 (pushnew (list role restr) return-list :test #'equal)
				 )
				;; otherwise add the role and its restrictions to the list
				(t (pushnew (list role restr) return-list :test #'equal))
				)
			  ))
		      ))
		  ))))
	))
    ;; sort by thematic role name
    (sort return-list #'string-lessp :key #'car))
  )

(defun get-lf-hierarchy-types-for-category (pos)
  " 
;;
;; pos       : part of speech e.g. w::v
;; returns : list of all lfs used (includes the hierarchy) for all words in selected pos category 
;; 
"
  (let ((return-list nil)
	(lf-type-list (get-lf-types-for-category pos))
	)
    (dolist (lf  lf-type-list)
      (let ((rents-list (om::get-parents lf)))
	(pushnew lf return-list :test #'equal)
	(dolist (rent rents-list)
	  (pushnew rent return-list :test #'equal)
	  )
	)
      )
    return-list)
  )

(defun maximum-lf-depth-for-category (pos)
  " 
;;
;; pos       : part of speech e.g. w::v
;; returns : longest list of parents for a word in this category
;; 
"
  (let ((return-list nil)
	(lf-type-list (get-lf-types-for-category pos))
	)
    (dolist (lf  lf-type-list)
      (let ((rents-list (om::get-parents lf)))
	(if (> (length rents-list) (length return-list))
	    (setq return-list rents-list))
	)
      )
    return-list)
  )

(defun show-unknown-words (wordfile)
  "
;; show-unknown-words (wordlist)
;; wordlist : list of words -- they don't have to have the w package prefix
;; returns  : all words in wordlist that are not defined in the current lexicon
"
  (let ((res nil)
	(wordlist (util::read-in-list-from-stream wordfile))
	)
    (dolist (w wordlist)
      (let ((ww (util::convert-to-package w 'w))) ;; words must have the w package prefix
      (if (not (retrieve-from-lex ww))
	  (pushnew w res :test #'equal))
      )
      )
    (reverse res))
  )


(defun read-vocab-file (fname)
  (let ((instream (open fname :direction :input))
	(word nil)
	(wordlist nil)
	(result nil)
	)
    (loop
     do (pushnew (setq word (read-line instream nil nil)) wordlist)
     while word)
    (close instream)
    (dolist (word wordlist)
      (when word (pushnew (read-from-string word) result)))
     (reverse result)
  ))

(defun compare-LM-vocab (wordfile)
  "tests each word in the LM vocab file and if it's unknown adds it to the list of unknown words that is returned
   @param wordfile -- an LM vocab file, e.g. SpeechLM/cardiac/cardiac.vocab
  "
  (let ((res nil)
	(wordlist (read-vocab-file wordfile))
	)
    (dolist (w wordlist)
      (let* ((ww (util::convert-to-package w 'w))) ;; words must have the w package prefix
      (if (not (retrieve-from-lex ww))
	  (pushnew w res :test #'equal))))
    (reverse res))
  )
  

(defun remove-doubles (wordfile)
  "
;; show-unknown-words (wordlist)
;; wordlist : list of words -- they don't have to have the w package prefix
;; returns  : all words in wordlist that are not defined in the current lexicon
"
  (let ((res nil)
	(wordlist (util::read-in-list-from-stream wordfile))
	)
    (dolist (w wordlist)
      (pushnew w res :test #'equal)
      )
    (format t "total words: ~S~%" (length res))
    (reverse res))
  )


(defun remove-numbers (wordfile)
  (let ((res nil)
	(wordlist (util::read-in-list-from-stream wordfile))
	)
    (dolist (w wordlist)
      (if (not (numberp w))
	  (pushnew w res :test #'equal))
      )
    (format t "total words: ~S~%" (length res))
    (reverse res))
  )


(defun intersect-files (file1 file2)
  (let ((l1 (remove-numbers file1))
	(l2 (remove-numbers file2))
	common l1only l2only)
    (dolist (w l1)
      (if (find w l2)
	  (pushnew w common :test #'equal)
	  (pushnew w l1only))
      )
    (dolist (w l2)
      (if (not (find w l1))
	  (pushnew w l2only)))
    (values common l1only l2only))
  )


(defun show-unknown-pos (wordfile pos)
  "
;; show-unknown-pos (wordlist)
;; wordlist : list of words -- they don't have to have the w package prefix
;; pos      : part of speech e.g. w::v w::n -- don't need the package prefix
;; returns  : all words in wordlist that are not defined as this pos in the current lexicon
"
  (let ((res nil)
	(wordlist (util::read-in-list-from-stream wordfile))
	(wpos (util::convert-to-package pos 'w))
	)
    (dolist (w wordlist)
      (let* ((ww (util::convert-to-package w 'w)) ;; words must have the w package prefix
	    (wdef (retrieve-from-lex ww))
	    )
      (if (not wdef) (pushnew w res :test #'equal) ;; word is undefined, add to list
	(if (not (eq (lex-entry-cat (car wdef)) wpos)) (pushnew w res :test #'equal))) ;; word is defined, but not as a verb
      )
      )
    (format t "total unknown verbs is ~S~%" (length res))
    (reverse res))
  )

(defun stringify (lst)
  "Get rid of the lxm package prefix -- was used in verbnet definition processing"
  (let ((*package* *lxm-package-var*))
    (format nil "~S" lst)
    )
  )

(defun is-closed-class (part-of-speech-category)
  (is-trips-closed-class (list part-of-speech-category))
  )

(defun is-trips-closed-class (poslist)
  "words with these trips part of speech tags are excluded from external resource lookup"
  (or (member 'w::art poslist)
      (member 'w::pro poslist)
      (member 'w::quan poslist)
      (member 'w::conj poslist)
      (member 'w::prep poslist)
      (member 'w::infinitival-to poslist)
      (member 'w::part poslist)
      (member 'w::number poslist)
      (member 'w::number-unit poslist)
      (member 'w::punc poslist)
      )
  )


(defun find-categories-in-wdef (poslist wdef)
   (remove-if #'null (mapcar (lambda (d)
			       (let* ((feats (lex-entry-feats d))
				      (this-pos (lex-entry-cat d)))
				(find this-pos poslist)))
			     wdef))
   )

(defun is-trips-functional-category (word-category)
  (find word-category '(w::^ w::^o w::^s w::art w::conj w::cv w::infinitival-to w::neg w::number-unit w::ordinal w::pro w::punc w::quan w::value w::prep))
  )
  
(defun get-all-word-forms (w)
  "
;; get-all-word-forms (w)
;;
;; w       : word (e.g. take)
;; returns : list of vocabulary entry structures for all morphological variants of a word  
;; 
;; (get-word-forms 'w::person) returns
;;((PERSON
;;  (:SING PERSON5750
;;   #S(VOCABULARY-ENTRY :NAME PERSON2029 :PARTICLE NIL :REMAINING-WORDS NIL
;;                       :WORD PERSON :POS N :PRONUNCIATION NIL :WFEATS NIL
;;                       :SENSES
;;                       (#S(SENSE-DEFINITION :POS N :LF NIL :SEM NIL :PREF 1.0
;;                                            :NONHIERARCHY-LF NIL :SYNTAX NIL
;;                                            :BOOST-WORD NIL
;;                                            :LF-PARENT LF_PERSON
;;                                            :LF-FORM PERSON
;;                                            :TEMPL COUNT-PRED-TEMPL
;;                                            :PARAMS NIL))
;;                       :BOOST-WORD NIL)))
;; (PERSONS
;;  (:PLUR PERSON5751
;;   #S(VOCABULARY-ENTRY :NAME PERSON2029 :PARTICLE NIL :REMAINING-WORDS NIL
;;                       :WORD PERSON :POS N :PRONUNCIATION NIL :WFEATS NIL
;;                       :SENSES
;;                       (#S(SENSE-DEFINITION :POS N :LF NIL :SEM NIL :PREF 1.0
;;                                            :NONHIERARCHY-LF NIL :SYNTAX NIL
;;                                            :BOOST-WORD NIL
;;                                            :LF-PARENT LF_PERSON
;;                                            :LF-FORM PERSON
;;                                            :TEMPL COUNT-PRED-TEMPL
;;                                            :PARAMS NIL))
;;                       :BOOST-WORD NIL))))

"
  (let* ((entries (getwordDefs w *lexicon-data*))
	 (return-list nil)
	 )
    (dolist (lentry entries)
      (let* ((entry (third lentry))
	     (morphlist (sort-by-morph entry (lexicon-db-synt-table *lexicon-data*)))
	     (variants (mmapcan (lambda (x)
				  (generate-morph-variants (vocabulary-entry-word entry)
							   (or (first x) (get-morph-default (vocabulary-entry-pos entry)))
							   (second x))
				  )
				morphlist)))
	(setq return-list (append return-list variants))
	))
    return-list)
  )


(defun get-vocabulary-entries ()
  "
;; get-vocabulary-entries
;;
;; returns  : vocabulary entries for all words (including morphological variants) in lexicon
;;          : these entries do not include information from the lf-ontology, i.e. roles or complete semantic features
;;
;; here's what the vocab entry for 'select' base form looks like:
;;  (SELECT
;;  ((:12S123PBASE SELECT43
;;    #S(VOCABULARY-ENTRY :NAME SELECT42 :PARTICLE NIL :REMAINING-WORDS NIL :WORD SELECT :POS V :PRONUNCIATION NIL :WFEATS NIL
;;                        :SENSES
;;                        (#S(SENSE-DEFINITION :POS V :LF NIL :SEM ((ASPECT F_BOUNDED) (TIME-SPAN F_ATOMIC)) :PREF 0.95 :NONHIERARCHY-LF NIL
;;                                             :SYNTAX NIL :BOOST-WORD NIL :LF-PARENT LF_SELECT :LF-FORM SELECT :TEMPL AGENT-THEME-XP-TEMPL
;;                                             :PARAMS NIL))
;;                        :BOOST-WORD NIL))))
;;
;; possible extension: make this print out the composite and particle entries nicely "
  (let ((res nil))
    (maphash (lambda (word entry)
	       (push (list word entry) res))
	       (lexicon-DB-word-table *lexicon-data*)
	       )
    res)
  )

(defun remove-irregular-multiples (verb-lemma-list)
  "lexemes for irregular forms should not be counted as separate lemmas"
  (let ((contractions '(w::ca w::wo w::sha w::^s w::^d w::^ll w::^m w::^re w::^ve))
	(nonbase-be-forms '(w::am w::are w::is w::was w::were))
	)
    (dolist (lemma verb-lemma-list)
      (if (or (find lemma contractions)
	      (find lemma nonbase-be-forms))
	  (setq verb-lemma-list (remove lemma verb-lemma-list)))
      )
    verb-lemma-list)
  )

(defun get-verb-lemmas ()
    " 
;;
;; returns : list of all base forms defined in the lexicon for the given pos
;; 
"
    (let ((res nil)
	  (lemmas (get-all-lemmas))
	  )
      (dolist (lemma lemmas)
	(let* ((entries (retrieve-from-vocabulary lemma)))
	  ;; an entry can have multiple parts of speech or particle verbs under the same lemma
	  (dolist (e entries) 
	    (let ((entry (third e)))
	      (if (and (eq (vocabulary-entry-pos entry) 'w::v)
		       ;; collect only base forms
		       (or (irregular-base-p e)
			   (regular-base-p e)))
		  ;; check for particle verbs
		  (if (vocabulary-entry-particle entry)
		      (pushnew (sense-definition-lf-form (car (vocabulary-entry-senses entry))) res  :test #'equal)
		    
		    (pushnew lemma res  :test #'equal))
		)
	      )
	    )
	  )
	)      
      (sort (remove-irregular-multiples res) #'string-lessp)
      )
    )

(defun get-lemmas (pos)
    " 
;;
;; returns : list of all base forms defined in the lexicon for the given pos
;; note that this will return non-lemma forms stored in the lexicon as irregular
;; such as irregular plurals
;; 
"
    (let ((res nil)
	  (lemmas (get-all-lemmas)) ;; get lexicon hash table keys
	  )
      (dolist (lemma lemmas)
	(let* ((entries (retrieve-from-vocabulary lemma)))
	  (dolist (e entries)
	    (let ((entry (third e)))
	      (case (vocabulary-entry-pos entry)
		((or w::n w::adj w::adv w::name w::quan w::pro w::det w::quan w::uttword)
		 (if (eq (vocabulary-entry-pos entry) pos)
		     ;; collect all relevant lemmas in a list
		     (if (or (eq (first e) ':NONE) (eq (first e) ':SING))
			 ;; include multi-word entries
			 (if (vocabulary-entry-remaining-words entry)
			     (pushnew (cons lemma (vocabulary-entry-remaining-words entry)) res :test #'equal)
			   (pushnew lemma res  :test #'equal)))))
		(otherwise nil))
		)
	      )
	    )
	)
      res)
    )

(defun get-word-sense-pairs (pos)
  (let ((lemmas (get-lemmas pos))
	senses pairs res)
    (if (not lemmas) (setq lemmas (get-verb-lemmas)))
    (dolist (lemma lemmas)
      (if (listp lemma) (setq pairs (get-lf-with-word (first lemma)))
	(setq senses (get-lf-with-word lemma)))
      (dolist (sense senses)
	  (pushnew sense res :test #'equal)
       ))
    res)
  )

(defun regular-base-p (entry)
  (eq (first entry) ':12S123PBASE)
  )

(defun irregular-base-p (entry)
  (and (eq (first entry) ':NONE)
  ;;     (eq (get-vform entry) 'w::base)
       )
  )

(defun get-all-vocabulary-words ()
  "
;; get-all-vocabulary-words
;; Returns: the list of all words (including morphological variants) in the lexicon, without repetitions
;; This includes particles and parts of multi-word entries not defined as independent words and therefore not visible on the call to
;; get-vocabulary-entries, which only returns the list of words with vocabulary entries in the tables"
  (let ((res nil)
	(vocab-entries (get-vocabulary-entries))
	)
    (dolist (entry vocab-entries)
      ;; word is the first of the entry
      (pushnew (first entry) res)
      ;; the second element of the entry is the list of definitions with names, entry codes and vocab structures
      (dolist (spec (second entry))
	(let ((def (third spec))) ;; the definition structure is the third in the entry specification
	  (when (vocabulary-entry-particle def)
	    (pushnew (vocabulary-entry-particle def) res))
	  (dolist (word (vocabulary-entry-remaining-words def))
	    (pushnew word res)
	    )
	  ))
      )
    res
    ))

(defun lex-size ()
 (hash-table-count (lexicon-db-word-table *lexicon-data*))
  )

(defun test-lex ()
" accesses every word and variant in the lexicon, revealing any problems or warnings"
    (maphash (lambda (word entry)
	       (declare (ignore entry))
		 (when word
		 (mapcar (lambda (def)
			   (declare (ignore def))
;			   (format t " ~S" word)
			   )
			 (retrieve-from-lex word))
		 ))
             (lexicon-DB-word-table *lexicon-data*)
             )
    )

(defun count-lex ()
" counts all words and variants in the lexicon"
  (let ((res nil)
	(vcount 0)
	(ncount 0)
	(adjcount 0)
	(advcount 0)
	(qcount 0)
	(namecount 0)
	(othercount 0)
	)
    (maphash (lambda (word entry)
	       (declare (ignore entry))
	       (format t " ~S " word)
	       (when word
		 (mapcar (lambda (def)
			   (case (lex-entry-cat def)
			     (w::v (incf vcount))
			     (w::n (incf ncount))
			     (w::adj (incf adjcount))
			     (w::adv (incf advcount))
			     (w::name (incf namecount))
			     (w::quan (incf qcount))
			     (otherwise (incf othercount))
			     ))
			 (retrieve-from-lex word)) ;; all senses, but not morph variants
		 ))
             (lexicon-DB-word-table *lexicon-data*)
             )
    (format t "verbs ~A: ~% nouns ~A~% adj ~A~% adv ~A~% quan ~A~% names ~A~% other ~A~%" vcount ncount adjcount advcount qcount namecount othercount)
    res)
  )

(defun base-from-hyphenated-symbol (sym)
 "strip anything after a hyphen"
 (let ((p (position #\- (symbol-name sym))))
   (if p (intern (subseq (symbol-name sym) 0 p))
	sym)))

(defun split-on-what (line split)
  (let ((str "") (new nil) (y ""))
    (dotimes (x (length line))
      (setf y (string (aref line x)))
      (cond
       ((equalp y split) (push str new) (setf str ""))
       (t (setf str (concatenate 'string str y)))))
    (if str
	(push str new))
    (reverse new)))

(defun hyphenated-symbolp (lex)
  "multiword symbols are words separated by hyphens"
  (find #\- (symbol-name lex) :test #'equalp)
  )

(defun get-first-part-of-hyphenated-symbol (lex)
  "multiword entries are indexed in the lexicon under the first word"
  (let ((firstword (intern (first (split-on-what (symbol-name lex) "-")))))
    (if firstword (setq firstword (util::convert-to-package firstword 'w)))
   firstword)
    )

;; return the lftriple (:* lftype word) from a word definition structure
(defun get-lftriple (wdef)
  (get-feature-values (lex-entry-feats wdef) 'w::lf)
  )

(defun lex-entry-ont-type (wdef)
  (let ((this-lftriple (get-lftriple wdef)))
    (if (listp this-lftriple) (strip-out-lf this-lftriple)) 
  ))

(defun get-remaining-words (wdef)
  (let ((lex (get-feature-values (lex-entry-feats wdef) 'w::lex)))
    (if (listp lex) (cdr lex) nil)
  ))

(defun get-lfform (w lftype)
  (let* ((match-word (if (listp w) (first w) w))
	 (defs (retrieve-from-lex match-word))
	(res nil))
    (dolist (wdef defs) ;; walk through the defs to get an lfform for this lftype (sense)
      (if (listp w) (print-debug "cdr w is ~S remaining words are ~S ~%" (cdr w) (get-remaining-words wdef)))
      (when (or (not (listp w)) (equal (cdr w) (get-remaining-words wdef)))
	  (let* ((lftriple (get-lftriple wdef))
		 (this-lftype (if (listp lftriple) (strip-out-lf lftriple)
				nil)))
	    (when (and this-lftype (eq this-lftype lftype))
	      (pushnew (third lftriple) res :test #'equal))
	    ))
      )
    (first res))
  )

;; return all aliases for this word with this lftype (sense)
(defun get-aliases-for-sense (w lftype)
  (let ((result-list nil)
	(lfform (get-lfform w lftype))
	)
    (dolist (word-pair (get-words-from-lf lftype)) ;; search the words with this lf to find a matching lfform
      (let* ((this-word (first word-pair))
	    (match-word (if (listp this-word) (first this-word) this-word))) ;; multi-words are indexed by first word
	    (dolist (this-wdef  (retrieve-from-lex match-word)) ;; search the word definitions for a sense that matches the lftype and lfform 
	      (let ((this-cat (lex-entry-cat this-wdef)))
		(when (not (eq this-cat 'w::word))
		  (let ((this-lftriple (get-lftriple this-wdef)))		     
		    (when (listp this-lftriple)
		      (let* ((this-lftype (strip-out-lf this-lftriple))
			     (this-lfform (get-lfform this-word this-lftype)))
			(print-debug "checking lftype ~S lfform ~S against target lftype ~S lfform ~S~%" this-lftype this-lfform lftype lfform)
			(if (eq this-lftype lftype) ;; if the lftypes match, check the lfform
			    (when (eq this-lfform lfform)
			      (print-debug "********match found ~%" )
			      (pushnew this-word result-list :test #'equal))))))
		    )
		  )
		)
	      )
	  )
    ;; don't count singletons (ie florida doesn't count as an alias for florida)
    (when result-list 
      (if (= (length result-list) 1) (setq result-list nil)))
    result-list)
  )

(defun non-default-filtered-senses (wdef)
  "return only non-default sense definitions, i.e. not ont::referential-sem or ont::situation-root"
  (let ((non-default-wdef))
    (dolist (this-sense wdef)
      (when (not (find (lex-entry-ont-type this-sense) '(ont::referential-sem ont::situation-root)))
 	      (pushnew this-sense non-default-wdef :test #'equal)))
    non-default-wdef)
  )

(defun feature-values-from-lf (lf feature)
  "given an lf type, determine the feature values for a particular feature. e.g. for ont::vehicle and the feature w::mass, return w::count. This information is used to reduce ambiguity for unknown words in the absence of tagging information."
  (let ((word-cat-pairs (get-words-from-lf lf))
	res)
    (dolist (this-pair word-cat-pairs)
      (let* ((w (first this-pair))
	     (cat (second this-pair))
	     (this-wdef (retrieve-from-lex w))
	     this-feature-value)
	(dolist (this-sense this-wdef)
	  (when (and
		 (eq (lex-entry-cat this-sense) cat)
		 (eq (lex-entry-ont-type this-sense) lf)
		 (setq this-feature-value (get-feature-values (lex-entry-feats this-sense) feature)))
	    (when (not (is-variable-name this-feature-value))
	      (pushnew this-feature-value res :test #'equal)))
	)))
    res)
  )

(defun trips-pos-list-for-ont-type (ont-type)
  "return a list of all pos for which this ont-type has definitions"
    (when ont-type
     ;; the gethash implementation will fail if no words are defined as leaves on the type. There is no subtype searching. It also misses some cases of POS defined in the subtypes but not higher in the hierarchy, e.g. ont::predicate supports both w::adv and w::adj, but will get only ont::adv without subtype searching. Efficiency could be improved by implementing caching. (e.g., gloss -- this is called when TextTagger sends tagged lexical entries, and TT may pair incompatible words and types)
     #| (gethash ont-type (lexicon-db-lf-table *lexicon-data*))
      )
    )|#
     (remove-if #'null (remove-duplicates (mapcar (lambda (x)
						     (second x))
						   (get-words-from-lf ont-type :include-subtypes t))))))
					

(defun trips-pos-list-for-word (wdef &key include-multiwords)
  "return a list of all pos for which this word is defined in the trips lexicon"
  (let (pos-list)
    (dolist (entry wdef)
      (if include-multiwords
	  (pushnew (lex-entry-cat entry) pos-list :test #'equal)
	;; only count pos for single word entries
	(if (eq (length (lex-entry-words entry)) 1)
	    (pushnew (lex-entry-cat entry) pos-list :test #'equal)
	  )))
      pos-list))

(defun string-to-list (string)
  "Returns a list of the data items represented in the given list."
  (let ((the-list nil) ;; we'll build the list of data items here
        (end-marker (gensym))) ;; a unique value to designate "done"
    (loop (multiple-value-bind (returned-value end-position)
                               (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            ;; if not done, add the read thing to the list
            (setq the-list 
                  (append the-list (list returned-value)))
            ;; and chop the read characters off of the string
            (setq string (subseq string end-position))))))

(defun remove-hyphens (w)
  "convert a hyphenated word to a multiword expression with blanks, e.g. w::prime-time -> (w::prime w::time). This conversion needs to happen to find hyphenated words in the TRIPS lexicon and WordNet"
  (cond ((symbolp w)
	(when (find #\- (string w))
	  (setq w (string-to-list (substitute #\Space #\- (string w))))
	  ))
	(t w))
  (util::convert-to-package w :w)) ; make sure all the words are in the w package

(defun is-duplicate-sense (pos ont-type senses-retrieved)
  (remove-if #'null
    (mapcar (lambda (retrieved-sense-pair)
	      (if (member pos retrieved-sense-pair) (member ont-type retrieved-sense-pair)))
	      senses-retrieved))
  )

(defun is-subsumed-sense (pos ont-type senses-retrieved)
  (remove-if #'null
    (mapcar (lambda (retrieved-sense-pair)
	      (if (member pos retrieved-sense-pair) (om::is-sublf ont-type (car retrieved-sense-pair))))
	      senses-retrieved))
  )

(defun is-more-abstract-sense (pos ont-type senses-retrieved)
  (remove-if #'null
    (mapcar (lambda (retrieved-sense-pair)
	      (if (member pos retrieved-sense-pair) (om::is-sublf (car retrieved-sense-pair) ont-type)))
	      senses-retrieved))
  )


;(defun lexicon-entries ()
;  " Dump everything in the lexicon
;;; lexicon-entries
;;;
;;; returns  : lexical entries for all words (including morphological variants) in lexicon -- these include lf-ontolgy information
;;;          : it's what you would get if you call get-word-def for every word in the lexicon
;;; 
;@visibility private
;"
;  (let ((res nil))
;    (maphash (lambda (word entry)
;                 (mapcar (lambda (def)
;                           (push (list (lex-entry-words def) (lex-entry-pref def) (lex-entry-description def)) res))
;                         (retrieve-from-lex word))
;                 )
;             (lexicon-DB-word-table *lexicon-data*)
;             )
;    res)
;  )

