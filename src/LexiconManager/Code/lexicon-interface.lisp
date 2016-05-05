;; lexicon-interface.lisp
;;
;; Mary Swift
;;
;; Public lexicon access functions for use by other packages (Parser, Generation, Speech Recognition) 
;;

(in-package "LEXICONMANAGER")

;; TRIPS part of speech categories

;; W::ADJ     adjective
;; W::ADV     adverb
;; W::N       noun
;; W::V       verb
;;
;; W::ART            articles e.g. a, the
;; W::CONJ           conjunctions e.g. and, but
;; W::FP             filled pauses e.g. um, uh
;; W::INFINITIVAL-TO to
;; W::NAME           proper names
;; W::NEG            negation, e.g. not, n't
;; W::NUMBER-UNIT    e.g. dozen, hundred, thousand...
;; W::ORDINAL        first, second, ...
;; W::PREP           prepositions (no semantic content in TRIPS)
;; W::PRO            pronouns
;; W::PUNC           punctuation
;; W::QUAN           quantifiers, e.g. all, each, only
;; W::UTTWORD        interjections, acknowledgements & idiomatic constructions
;; W::VALUE          names of days, months, also letter-symbols

;; --------------------------------
;; a sample lexical entry structure for the word 'and'
;; #S(LEX-ENTRY :WORDS (AND) :PREF 1.03
;;              :DESCRIPTION
;;              (CONJ (VAR V217630) (LEX AND) (INPUT AND) (LF AND) (SEM ($ -)) (TRANSFORM NIL) (COERCE -)
;;               (SUBCAT (? SUB S PP NP VP)) (CONJ +) (SEQ +))))


(defun get-lf (w &key pos wdef)
  " 
;; uses retrieve-from-lex, which calls the lf-ontology to construct a complete lexical
;; representation (including sem features & role restrictions)
;; get-lf (w)
;;
;; @param w    : word (e.g. w::truck)
;; @param pos  : (optional) part of speech = e.g.  w::N,w::V
;;             : if pos is given, return only the valid lf types for the pos
;; @return    : list of LF::parent and part of speech 
;; 
;; (get-lf 'w::search) returns
;; ((LF::SCRUTINY W::V) (LF::EVENT W::N) (LF::COMPUTER-SOFTWARE W::N))
;;
;; (get-lf 'w::search :pos 'w::v) returns
;; ((LF::SCRUTINY W::V))
;;
@visibility public
"

  (let ((return-list nil))
    (when (symbolp w)
      (if (hyphenated-symbolp w) (setq w (get-first-part-of-hyphenated-symbol w)))
      (let ((defs (or wdef (retrieve-from-lex w))))
	(dolist (this-def defs)
	  (let ((cat (lex-entry-cat this-def))
		(feats (lex-entry-feats this-def)))
	    (when (not (eq cat 'w::word))
	      (let ((lf (strip-out-lf (get-feature-values feats 'w::lf))))
		(if lf ;; exclude nil lfs -- these appear in uttwords
		    (if pos
			;; only return lf types that match pos
			(if (eq pos cat)
			    (pushnew (list lf cat) return-list :test #'equal))
			(pushnew (list lf cat) return-list :test #'equal))
		))
	      )
	    )
	  )
	)
      )
    return-list)
  )
 
(defun get-lf-with-word (w &key pos)
  " 
;; get-lf-with-word (w)
;; like get-lf, but includes the word in the return -- also if it's a compound
;;
@visibility public
"

  (let ((return-list nil))
    (dolist (def  (retrieve-from-lex w))
      (let ((cat (lex-entry-cat def))
	    (feats (lex-entry-feats def)))
	(when (not (eq cat 'w::word))
	  (let* ((lf (get-feature-values feats 'w::lf))
		 (lf-type (strip-out-lf lf))
		 (word (if (listp lf) (third lf)
			 w))
		)
	    (if lf ;; exclude nil lfs -- these appear in uttwords
		(if pos
		    ;; only return lf types that match pos
		    (if (eq pos cat)
			(pushnew (list lf-type cat word) return-list :test #'equal))
		  (pushnew (list lf-type cat word) return-list :test #'equal))
	      )
	    )
	  )

      )
      )
    return-list)
  )
 
(defun get-word-form (w formcat &key agr)
  "
 get-word-form (w formcat &key agr)

 @param w       : word (e.g. take)
 @param formcat : form categorys are
           verbs:     :BASE, :PRES 
                      :PAST, :PASTPART, (also passive) :ING
           nouns:     :SING :PLUR
           adverbs    :LY (e.g., QUICKLY)
           adjectives :ER (e.g., QUICKER), :EST (e.g., QUICKEST); NIL formcat returns base form (e.g. quick)
 @param agr     : person and number agreement, used to identify correct 'be' form for present or past
         : values are 1S, 2S, 3S, 1P, 2P, 3P; default is nil.
 @return : word-form (e.g. took)
 
 @desc (get-word-form 'w::man ':plur) returns (w::men)
 
 (get-word-form 'w::be ':BASE) returns (be)
 (get-word-form 'w::be ':PRES :AGR '3s) returns (w::is) 
 (get-word-form 'w::be ':past :agr 1s) returns (was)
   if no agr is given for be past or present, return a list of possible values
 (get-word-form 'w::be 'v :past) returns (was were)

 (get-word-form '(w::pick w::up) :ing) returns ((w::picking w::up)) 
 (get-word-form 'w::quick ':er) returns (quicker)

 Current limitations (code to be added later):
 - doesn't handle domain-specific words produced with 'define-object' -- these return nil from get-all-word-forms and have only
a basic word definition and no sense definition
 @visibility public
"
  (let ((return-list nil))
    (cond ((listp w) ;; we have a complex entry, such as a compound word or particle verb
	   (let ((formslist (get-all-word-forms (car w)))  ;; complex entries are indexed by their first word
		 (rest-of-words (rest w))
		 )
	     (dolist (entry formslist)
	       (when entry
		 (let* ((form (car (second entry)))
			(x (third (second entry)))
			(wfeats (vocabulary-entry-wfeats  x))
			(particle (list (vocabulary-entry-particle x)))
			(remaining-words (vocabulary-entry-remaining-words x)))
		   ;; match both the form category and the remaining words
		   (cond ((and (eq form formcat) (or (equal rest-of-words remaining-words) (equal rest-of-words particle)))
			  (pushnew (cons (first entry) rest-of-words) return-list :test #'equal))
			 ;; is it :pastpart for a :conflate-past entry?
			 ((and (eq formcat ':pastpart) (or (equal rest-of-words remaining-words) (equal rest-of-words particle)))	
			  (if (and (eq form ':past) (conflate-past wfeats))
			      (pushnew (first entry) return-list :test #'equal))
			  )
			 ((and (eq formcat ':base) (or (equal rest-of-words remaining-words) (equal rest-of-words particle)))
			  (if (eq form ':12s123pbase)
			      (pushnew (cons (first entry) rest-of-words) return-list :test #'equal)))
			 ((and (eq formcat ':pres) (or (equal rest-of-words remaining-words) (equal rest-of-words particle)))
			  ;; if agr not specified return all present forms
			  (if (and (null agr) (or (eq form ':12s123pbase)(eq form ':3s)))
			      (pushnew (cons (first entry) rest-of-words) return-list :test #'equal)
			    ;; otherwise we have to match the agr since the 3s is different
			    (if (eq agr 'w::3s)
				(if (eq form ':3s)
				    (pushnew (cons (first entry) rest-of-words) return-list :test #'equal))
			      (if (eq form ':12s123pbase)
				  (pushnew (cons (first entry) rest-of-words) return-list :test #'equal))
			      )))
			 (t nil)
			 ))))))
	  (t ;; a simplex entry
	   (let ((formslist (get-all-word-forms w)))
	     (dolist (entry formslist)
	       (when entry
		 (let* ((form (car (second entry)))
			(x (third (second entry)))
			(wfeats (vocabulary-entry-wfeats x))
			)
		 (when (and (not (vocabulary-entry-particle x)) (not (vocabulary-entry-remaining-words x))) ; exclude multiwords
		   (if (eq form ':none)   ;; we have a fixed word form (e.g be) -- have to check for vform and agr
		       (if (is-be w) (setq return-list (get-be-form formcat agr)) ;; be is special, of course
			 ;; we've got a fixed form other than be
			 (let* ((wagr (assoc 'w::agr wfeats))
				(wform (assoc 'w::vform wfeats)))
			   (if (and (listp wform) (eq formcat (second wform)))
			       (if (listp wagr) 
				   (let ((agrvals (get-agr-values wagr)))
				     (or (member agr agrvals) (null agrvals)) ;; allow for no agr
				     (pushnew (first entry) return-list :test #'equal)
				     )))))
		     ;; a reqular simplex form
		     (cond ((eq form formcat) 		  ;; match the form category, e.g. :past, :ing, :pastpart
			    (pushnew (first entry) return-list :test #'equal))
			   ;; is it :pastpart for a :conflate-past entry?
			   ((eq formcat ':pastpart)
			    (if (and (eq form ':past) (conflate-past wfeats))
				(pushnew (first entry) return-list :test #'equal))
			    )
			   ;; :base and :pres don't match the form directly -- need to interpret
			   ((eq formcat ':base)        
			    (if (eq form ':12s123pbase)
				(pushnew (first entry) return-list :test #'equal)))
			   ((eq formcat ':pres)
			    ;; if agr not specified, return all present forms
			    (if (and (null agr) (or (eq form ':3s)(eq form ':12s123pbase)))
				(pushnew (first entry) return-list :test #'equal)
			      ;; otherwise we have to match the agr since the 3s is different
			      (if (eq agr 'w::3s)
				  (if (eq form ':3s)
				      (pushnew (first entry) return-list :test #'equal))
				(if (eq form ':12s123pbase)
				    (pushnew (first entry) return-list :test #'equal))
				)))
			   (t nil))
		   ))))
	     ))))
  return-list)
)

(defun generate-underspecified-prepositions-for-entry (w wdef penn-tags)
  "remove all adv entries and add an underspecified one"
  (let ((feats `((w::gap ?gap)
		 (w::ATYPE (? ATYPE w::PRE w::POST w::pre-vp))
		 (w::sort w::pred)
		 ))
	filtered-def new-vocab-entry)
    (when wdef
      (dolist (entry wdef)
	(when (not (eq (lex-entry-cat entry) 'w::adv))
	  (pushnew entry filtered-def)))
      (setq new-vocab-entry  (make-replica-entry w 'ont::predicate 'w::adv '(ont::predicate)))
      (setq filtered-def (append (make-lex-entry-from-replica-entry new-vocab-entry w 'w::adv 'ont::predicate feats nil) filtered-def))
      )
    (or filtered-def wdef))
  )


(defun deverbal-form (lex pos)
  (let ((res nil))
    (case pos
      (w::n (setq res (lxm::get-word-form lex ':ing))) ;; gerunds, e.g. swelling
      (w::adj (setq res (lxm::get-word-form lex ':pastpart)))   ;; deverbal adj, e.g. swollen
      (t nil))
    (if (listp res) (setq res (car res)))
    res)
  )

(defun is-lemma (worddeflist)
  "returns t if any definition in the list for the base form is a word lemma, otherwise nil
   this function excludes the irregular verbs forms that are included in the lexicon-db-base-forms
"
  (let ((res nil))
    (dolist (def worddeflist)
      (case (first def)
	((:SING :12S123PBASE) (setq res t))
	(:NONE   ;; exclude irregular verb forms
	 (if (not (eq (vocabulary-entry-pos (third def)) 'w::v))
	     (setq res t)
	   ;; check if it's an auxiliary verb with no variants, e.g. would
	   (if (not (vocabulary-entry-wfeats (third def)))
	       (setq res t)
	   ;; check if the wfeats are defined as vform base -- this happens with w::be (of course)
	     (let ((vform (assoc 'w::vform (vocabulary-entry-wfeats (third def)))))
	       (if (eq (second vform) 'w::base) (setq res t)))
		 ))
	   )
	(otherwise nil))	
      )
    res)
  )

(defun get-words-from-lf (lf &key include-subtypes)
  "
 get-words-from-lf (lf)

 @param lf : lf type (e.g. lf::move)
 @return   : list of pairs (part-of-speech word) for all words (base forms) that are leaves

 @desc (get-words-from-lf 'lf::conjunct) returns
 ((THEN ADV) (SO ADV) (HOWEVER ADV) (BUT ADV) (AND ADV) (OR ADV)
   (AT ADV) (ALSO ADV))
 @visibility public

if include-subtypes is t then words from all sublfs of the specified lf are also returned

TODO: domain-specific words (such as CALO) and certain irregular forms (such as people) aren't extracted by lexicon-db-base-forms
"
  (let ((return-list nil))  
    (dolist (word  (lexicon-db-base-forms *lexicon-data*))
      (dolist (def  (getwordDefs word *lexicon-data*))
	(if (is-lemma (list def)) ;; only check lemmas (i.e. exclude irregular forms in lexicon-db-base-forms)
	    (let* ((x (third def))
		   (remaining-words (vocabulary-entry-remaining-words x))
		   (particle (vocabulary-entry-particle x))
		   )	  
	      (dolist (s (vocabulary-entry-senses x))
		(when (or (eq (sense-definition-lf-parent s) lf)
			  (and include-subtypes
			       (om::is-sublf (sense-definition-lf-parent s) lf)))
		  ;; if this is a multi-word expression, get all the words
		  (cond (remaining-words 
			 (pushnew (list (cons word remaining-words) (vocabulary-entry-pos x))
				  return-list :test #'equal))
			;; particles
			(particle 
			 (pushnew (list (list word particle) (vocabulary-entry-pos x))
				  return-list :test #'equal))
			;; otherwise just return the single word
			(t
			 (pushnew (list word (vocabulary-entry-pos x))
				  return-list :test #'equal)))
		  ))
	      ))))
    return-list)
    )

(defun get-pos (lftriple &key search-children)
  "return a list of the valid part of speech categories for the given word and lf"
  (let ((res))
    (when (listp lftriple)
      (let* ((lftype (second lftriple))
	     (word (third lftriple))
	     (lflist (get-lf word))) ;; possible lftypes for word, e.g. ((ont::to-loc w::adv) (ont::spatial-loc w::adv))
	(dolist (pair lflist)
	  (if (equal (first pair) lftype) (pushnew (second pair) res :test #'equal)
	    ;; if there wasn't an exact lftype match, check the descendants
	    (if search-children
		(if (om::is-sublf (first pair) lftype) (pushnew (second pair) res :test #'equal)))))))
  res)
)

;; global vars to record these values for analysis
(defvar *unknown-words* nil) ;; words not in TRIPS lexicon
(defvar *unknown-word-tokens* 0) ;; unknown word token count
(defvar *known-words* nil) ;; words not in TRIPS lexicon
(defvar *known-word-tokens* 0) ;; unknown word token count
(defvar *wf-words* nil) ;; words retrieved form WF
(defvar *use-trips-and-wf-senses* nil) ;;  T: retrieve WF senses for w even for competing parts of speech in TRIPS (increase coverage); NIL: retrieve WF senses only for parts of speech not already defined for w in TRIPS (reduce ambiguity)
(defvar *use-tagged-senses-only* nil) ; if sense tags exist and this var is T, use only those senses even when others exist; otherwise return all senses if no match is found for the tagged senses
(defvar *unknown-words-only* nil) ;; if T, find WF senses only if word is not defined in TRIPS; behavior unaffected by pos.
(defvar *no-wf-senses-for-words-tagged-with-ont-types* nil) ;; if T, we don't use WordNet if we have ont types from tagger

;; Variables for Penn TreeBank corpus experiments
(defvar *use-tagged-pos-only* nil) ; option added for systems where the POS tagging is highly reliable -- if T, XM only returns senses matching the tagged POS. Currently only used for PTB corpus building since the annotated trees are gold standard.
(defvar *use-underspecified-prepositions* nil) ; for PTB corpus building experiment. If T, bypass all trips senses for any word tagged as IN and return an underspecified form
(defvar *use-underspecified-lexicon* nil) ; for PTB corpus building experiment. If T, bypass all trips senses for any content word tagged and return the "default" form
(defvar *write-dynamic-lexicon-file* t)

(defun initialize-unknown-words ()
  (setq *unknown-words* nil)
  (setq *unknown-word-tokens* 0)
  (setq *wf-words* nil)
  )

(defun uw ()
  (format t "~S unknown words: ~S ~%~S ~% unknown word tokens ~% ~S words from WordFinder: ~S ~%" (length *unknown-words*) *unknown-words* *unknown-word-tokens* (length *wf-words*) *wf-words*)
  )

(defun adjust-probability (entries adjust-factor)
  "adjust a word preference by the adjust-factor"
  (let ((res nil))
    (dolist (entry entries)
      (setf (lex-entry-pref entry) (+ (lex-entry-pref entry) adjust-factor))
      (pushnew entry res)
      )
   res)
  )

(defun combine-senses-on ()
  (setq *use-trips-and-wf-senses* t)
  )

(defun combine-senses-off ()
  (setq *use-trips-and-wf-senses* nil)
  )

(defun unknown-words-only ()
  (setq *unknown-words-only* t)
  )

(defun contains-number (mwe)
  (remove-if #'null (mapcar (lambda (w)
			      (numberp w))
			    mwe)
	     )
  )

(defun exclude-from-lookup (w trips-pos-list penn-tags)
  (cond ((not (listp w))
	 (or (numberp w) (is-frequent-word w)
	     (is-trips-closed-class trips-pos-list) (is-penn-closed-class penn-tags)))
	(t
	 (or (is-trips-closed-class trips-pos-list) (is-penn-closed-class penn-tags)
	       (contains-number w)
	       )))
  )

(defun convert-number-to-word (mwe)
  (let (res converted-mwe)
    (dolist (w mwe)
      (if (numberp w) (push (lexicalize-number w) converted-mwe)
	(push w converted-mwe)))
    (reverse converted-mwe))
    )

(defun remove-noninitial-hyphens (mwe)
  (if (and (find 'w::punc-minus mwe) (not (equal 'w::punc-minus (first mwe))))
      (remove 'w::punc-minus mwe) mwe))

			
(defun make-default-unknown-word-entry (w trips-pos-list penn-tags tagged-sense)
  "create a new word entry on-the-fly with default semantic types, and a low preference score"
  (let* ((tagged-pos (find-arg tagged-sense :penn-parts-of-speech))
	 (pos (or (car (merge-pos-info trips-pos-list (merge-pos-info penn-tags tagged-pos))) 'w::n))
	 (ont-type (find-arg tagged-sense :ont-types))
	 (domain-info (find-arg tagged-sense :domain-specific-info))
	 
;	 (feats (add-default-features-for-pos nil pos))
	 (w (if (listp w) (remove-noninitial-hyphens w) w))
;		(if (and (find 'w::punc-minus w)
;			 (not (equal 'w::punc-minus (first w))))
;		    (remove 'w::punc-minus w) w) w)) ;; remove non-initial hyphenation 
	 (default-ont-type (or (cdr (assoc pos '((w::v ont::situation-root) (w::n ont::referential-sem) (w::adv  ont::predicate) (w::adj ont::modifier)))) '(ont::referential-sem)))
	 res)
    ; turning this off in CERNL.
;    (if (and (listp w) (contains-number w)) (setq w (convert-number-to-word w)))
    (print-debug "id for ~S is ~S~%" w (gen-id w))
    (setq res (make-unknown-word-entry w pos .98 nil (gen-id w) (or ont-type default-ont-type) nil penn-tags nil domain-info))
    (print-debug "sense-filtering returns nil; generating ont::referential-sem entry for ~S~%" w)
    res))

(defun merge-pos-info (trips-pos-list penn-tags)
  "merge trips parts of speech and penn treebank part of speech tags into a single trips part of speech list"
  (let ((converted-penn-tags (remove-duplicates (remove-if #'null (mapcar (lambda (tag) (penn-tag-to-trips-pos tag))
									  penn-tags)))))
    (union trips-pos-list converted-penn-tags))
  )

(defun augment-trips-word-categories (word-categories)
  "augment the list of word categories with
      - if ADV is among the categories, include w::prep as well"
  (when (find 'w::adv word-categories) (setq word-categories (pushnew 'w::prep word-categories)))
  (when (find 'w::to word-categories) (setq word-categories (remove-duplicates (append '(w::adv w::prep) word-categories))))
  word-categories)

(defun filter-by-pos (wdef w pos-list penn-tags)
  "return only the senses matching POS tags. Some exceptions:
   - any word tagged as ADV will also get any associated prep interpretation, since the same word as adjunct is handled as ADV, but as prep it's a subcat
   - all simple word entries are returned, e.g. not, because that's required for correct processing in certain grammar rules such as negation of auxiliary verbs
"
  (let* ((filtered-def nil)
	(tagged-word-categories (augment-trips-word-categories (merge-pos-info pos-list penn-tags))))
    (print-debug "tagged word categories are ~S~%" tagged-word-categories) 
    (when (find-categories-in-wdef tagged-word-categories wdef)
      (dolist (def wdef)
	(when (or (member (lex-entry-cat def) tagged-word-categories) (is-trips-functional-category (lex-entry-cat def)))
	  (pushnew def filtered-def))))
    (print-debug "pos filtered-def is ~S~%" filtered-def)
    (when ;(and (null filtered-def) (null wdef)) ; if no results, return a default
	(null filtered-def)
      (pushnew (car (make-default-unknown-word-entry w pos-list penn-tags nil)) filtered-def))
    (or filtered-def wdef))
  )

(defun adjust-pos-preferences (wdef poslist penn-tags)
  "return the senses matching POS tags at a higher preference than others. If no senses match the tags, then don't make any adjustments. This function ignores (for now) sense tags since if present, sense filtering would have already taken place"
  (let ((filtered-def nil)
	(preferred-word-categories (merge-pos-info poslist penn-tags)))
    (when (find-categories-in-wdef preferred-word-categories wdef)
      (dolist (def wdef)
	(when (not (member (lex-entry-cat def) preferred-word-categories))
	  (setf (lex-entry-pref def) (- (lex-entry-pref def) .01)))
	(pushnew def filtered-def)))
    (or filtered-def wdef))
  )

(defun compatible-pos-and-ont-type (pos ont-type)
  "This checks if any word in the lexicon has the POS and this specified ont-type"
  (if (not (is-trips-pos pos)) (setq pos (penn-tag-to-trips-pos pos)))
  (let* ((poslist (or (trips-pos-list-for-ont-type ont-type) '(w::n)))   ;; if no words have this ont-type, default to N
	 (res (or (and (equal pos 'W::NAME) (member 'w::N poslist))   ;; names fine with N types
		  (find pos poslist))))
    (print-debug "~%compatibility of ~S and ~S is ~S~%" pos ont-type res)
    res)
  )

(defun compatible-tagging (part-of-speech-tags ont-types)
  (let (res)
    (dolist (part-of-speech part-of-speech-tags)
      (dolist (ont-type ont-types)
	(setq res (pushnew (compatible-pos-and-ont-type part-of-speech ont-type) res))))
      (remove-if #'null res))
  )
     
(defun compatible-ont-types (ont-type1 ont-type2)
  (let ((common-ancestor  (om::common-ancestor ont-type1 ont-type2)))
    (when common-ancestor
	   (not (find common-ancestor '(ont::referential-sem ont::any-sem ont::situation-root))))
    )
  )

(defun trips-pos-for-wn-sense-keys (wn-sense-keys)
  "return trips POS tags from wn senses. This check has been added because the statistical POS tagging does not always agree w/ the tagged sense"
  (remove-duplicates (remove-if #'null (mapcar (lambda (key)
						 (wf::trips-pos-for-wn-sense-key key))
					       wn-sense-keys)))
  )
 
(defun cernl-hack-is-umls-concept (domain-info)
  "added for CERNL to prefer concepts from MetaMap. LXM doesn't seem like the best place to be screening for UMLS concepts, especially since they are unique to the CERNL system, but for some reason TextTagger isn't able to do it. Go figure."
  (when (listp domain-info)
    (setq domain-info  (util::convert-to-package domain-info :parser))
    (remove-if #'null (mapcar (lambda (concept-info)
				(find 'parser::UMLS concept-info))
			      domain-info))
    )
  )

(defun is-preferred-concept (domain-info)
  (cernl-hack-is-umls-concept domain-info)
  )

(defun is-compatible-sense (sense sense-list)
  (cond
   ;;  exact match
   ((find sense sense-list)  sense)
   ;; sense is a subtype of some tagged sense
   ((is-subtype-of sense-list sense)
    sense)
   ;; sense is a supertype of some tagged senses, return the tagged subtypes
   ((is-parent-of sense sense-list))))
#||
	
  (let ((compat (or (find sense sense-list) (is-subtype-of sense-list sense)))
	child)
    (when (and (not compat) 
	       ;; don't boost high-level compatible parent types	
	       (not  (member sense '(ont::referential-sem ont::abstract-object ont::modifier ont::predicate ont::situation-root ont::phys-object ont::action))))
      ;; if we have a parent type, return both so the more specific tagged type will be used
      (when (setq child (is-parent-of sense sense-list))
	(if (listp child) (setq child (car child)))
	(setq compat (list child sense)))
      )
    (print-debug "~%compatibility of ~S and ~S is ~S~%" sense sense-list compat)
    compat)
  )
||#
(defun update-tagged-sense-list (sense-list sense)
  (let ((res))
    (cond ((find sense sense-list)
	   (setq res (remove sense sense-list)))
	  (t 
	   (dolist (this-sense sense-list)
	     (if (and (not (om::is-sublf sense this-sense)) (not (om::is-sublf this-sense sense)))
		 (push this-sense res)))))
    res)
  )

(defun update-domain-info (entry domain-info)
  (let* ((feats (lex-entry-feats entry))
	 (this-kr-type (get-feature-values feats 'w::kr-type))
	 (this-description (lex-entry-description entry))
	 )
    (if (not this-kr-type) ; add the domain info if kr-type is NIL
	(setf (lex-entry-description entry) (subst (list 'w::kr-type domain-info) '(w::kr-type nil) this-description :test #'equal)))
  entry)
  )

(defun found-sense (pos ont-type sense-info-triples)
  (let (res)
    (dolist (sense-info-triple sense-info-triples)
      (if (and (find ont-type sense-info-triple) (find pos sense-info-triple) (> (length sense-info-triple) 2))
	  (setq res sense-info-triple))
      )
    res)
  )

(defun update-domain-info-in-entries (wdef sense-info-triples)
  "add tagged domain info to all compatible entries"
  (if sense-info-triples
      (let (adjusted-def)
	(dolist (def wdef)
	  (let* ((feats (lex-entry-feats def))
		 (this-pos (lex-entry-cat def))
		 (this-lf (strip-out-lf (get-feature-values feats 'w::lf)))
		 matching-triple
		 )
	    (dolist (sense-info-triple sense-info-triples)
	      (if (setq matching-triple (found-sense this-pos this-lf sense-info-triples))
		  (setq def (update-domain-info def (third matching-triple))))
	      (pushnew def adjusted-def))))
	adjusted-def)
      ;; if sense-info-triples is NIL, we just return the entries passed in
      wdef))
      

(defun prefer-more-specific-tagged-senses (w wdef tagged-senses penn-tags domain-info)
  "adjust word sense preferences to prefer any sense compatible with TT tags over non-tagged (e.g. TRIPS or WF) senses"
  (let (adjusted-def)
    (dolist (def wdef)
      (let* ((feats (lex-entry-feats def))
	     (this-pref (lex-entry-pref def))
	     (this-lf (strip-out-lf (get-feature-values feats 'w::lf)))
	     (this-pos (lex-entry-cat def))
	     (compat  (is-compatible-sense this-lf tagged-senses))
	     add-more-specific-tagged-def
	     )
	;; if no score given give it a default 
	(if (not (numberp this-pref)) (setq this-pref .97))

	;; lower the preference up if it's not a compatible tagged ont-type (or subtype/parent of it)
	(if compat
	    (cond 
	      ;;  if a symbol, then the type was in the tagged list, keep it as tagged
	      ((symbolp compat)
	       (pushnew def adjusted-def))
	      ((consp compat)
	       ;; if a list, we have a set of subtypes of the entry in the tagged list, use them instead
	       (pushnew (car (make-unknown-word-entry w this-pos this-pref nil (gen-id w) (list (car compat)) nil penn-tags (list (car compat)) domain-info)) adjusted-def)))
	    ;;  not in tagged list, penalize
	    (progn
	      (if (> (lex-entry-pref def) .97)
		  (setf (lex-entry-pref def) (- (lex-entry-pref def) .01)))
	      (pushnew def adjusted-def)))
	))
    
    adjusted-def))
	    #||
	      
	   ;; if there's more than one compatible type, 
	   (if  (and (listp compat) (> (length compat) 1)) ;; if parent type, substitute a new entry with the more specific sense
		     (pushnew (car (make-unknown-word-entry w this-pos this-pref nil (gen-id w) (list (car compat)) nil penn-tags (list (car compat)) domain-info)) adjusted-def))
		(setf (lex-entry-pref def) this-pref)
		)
	       ; for an incompatible sense, demote the preference unless it's already low
	       (t  (if (> (lex-entry-pref def) .97)
		       (setf (lex-entry-pref def) (- (lex-entry-pref def) .01)))))
	(pushnew def adjusted-def)))
  adjusted-def)
  )||#

(defun prefer-more-specific-domain-tagged-senses (wdef)
  "prefer concepts tagged with external source information (e.g., UMLS in CERNL) over other concepts."
  (multiple-value-bind (kr nokr)
      (split-list #'(lambda (def)
		      (get-feature-values (lex-entry-feats def) 'w::kr-type))
		  wdef)
    (if kr
	;; some entries have domain specific tags, so demote the non tagged ones
	(append kr (mapcar #'(lambda (def)
			       (if (> (lex-entry-pref def) .97)
				   (setf (lex-entry-pref def) (- (lex-entry-pref def) .01)))
			       def)
			   nokr))
	wdef)))

(defun prefer-domain-senses (w wdef)
  "prefer sense-word pairs specified in om::*domain-sense-preferences*"
  (let ((wdef (remove-if #'null wdef)))
    (dolist (def wdef)
      (let* ((feats (lex-entry-feats def))
	     (this-pref (lex-entry-pref def))
	     (lf-pair (get-feature-values feats 'w::lf))
	     (this-lf (strip-out-lf lf-pair))
	     (lex (if (consp lf-pair) (third lf-pair) lf-pair))
	     (preferred-domain-sense (cadr (assoc lex om::*domain-sense-preferences*)))
	     )
	(when preferred-domain-sense
	  (if (eq this-lf preferred-domain-sense)
	      (setf (lex-entry-pref def) .99)
	      ;; demote competing word senses
	      (if (> (lex-entry-pref def) .97)
		  (setf (lex-entry-pref def) (- (lex-entry-pref def) .01))))
	      
	      (pushnew def wdef)))
	)
    wdef)
  )

(defun filter-by-senses (wdef w trips-pos-list penn-tags ont-types wn-sense-keys domain-info &key tagged-senses-only)
  "filter the word definitions to return only senses corresponding to the sense classes in the list. If no such sense exists, a default sense is created on the fly using the available sense and pos information, or defaults if missing"
  (let* ((pos-list (subst 'w::N 'w::name (merge-pos-info trips-pos-list penn-tags)))  ;; map NAME to N as they are treated equivalently
	(tagged-senses ont-types)
	(tagged-senses-remaining tagged-senses)
	(compatible-defs nil)
	(other-defs nil)
	)
    (when wn-sense-keys (dolist (key wn-sense-keys) ; convert wn senses to TRIPS ontology types
			  (let ((mapped-sense (car (wf::best-ont-type-for-sense-key key))))
			    (when mapped-sense (pushnew mapped-sense ont-types :test #'equal))))
	  (setq tagged-senses ont-types)
	  (setq tagged-senses-remaining ont-types))
    ;; search the retrieved word definitions for a matching sense
    (dolist (def wdef)
      (let* ((feats (lex-entry-feats def))
	     (this-pos (if (eq (lex-entry-cat def) 'w::NAME)
			   'w::N
			   (lex-entry-cat def)))
	  
	     (this-lf (strip-out-lf (get-feature-values feats 'w::lf))))
	(print-debug "poslist ~S this-pos ~S this-lf ~S  ont-types ~S~%" pos-list this-pos this-lf ont-types)
	(cond ((or (and pos-list (find this-pos pos-list)) (null pos-list)) ;; if part of speech is specified, they have to match
	       (cond ((is-compatible-sense this-lf ont-types)
		  
		      (pushnew def compatible-defs :test #'equal)
		      (setq tagged-senses-remaining (update-tagged-sense-list tagged-senses-remaining this-lf)))
		  ; for an incompatible sense, demote the preference unless it's already low
		     (t  
                      (if (> (lex-entry-pref def) .97)
		      (setf (lex-entry-pref def) (- (lex-entry-pref def) .01)))
		      (print-debug "lowering preference for incompatible sense ~S~%" this-lf)
		      (push def other-defs)
		      )))
	      (t  ; for an incompatible sense, demote the preference unless it's already low
	       (if (> (lex-entry-pref def) .97)
		   (setf (lex-entry-pref def) (- (lex-entry-pref def) .01)))
	       (print-debug "lowering preference for incompatible sense ~S~%" this-lf)
	       (push def other-defs))
	    )
	  ))
    (print-debug "wdef is ~S~% compatible-defs is ~S~% other-defs are ~S~%" wdef compatible-defs other-defs)
    (when tagged-senses-remaining
      (print-debug "No compatible sense found for ~S in sense list ~S~%" w tagged-senses-remaining)
      (dolist (ont-type tagged-senses-remaining)
	(dolist (pos pos-list)
	  (cond ((and pos (compatible-pos-and-ont-type pos ont-type))
		 ;; create a new entry with the given pos and sense as long as they are compatible
		 (let ((new-entry (car (make-unknown-word-entry w pos .99 nil (gen-id w) (list ont-type) nil penn-tags (list ont-type) domain-info))))
		   (when new-entry (push new-entry compatible-defs)))
		 (print-debug "making new sense for ~S as ~S ~S~%" w pos ont-type)
		 )
		;; generate a default entry if the domain senses are preferred
		((and (not wdef) (not compatible-defs) (is-preferred-concept domain-info) (not (is-closed-class pos)))
		 (pushnew (car (make-default-unknown-word-entry w pos-list penn-tags domain-info)) compatible-defs)
		 (print-debug "No match for ~S with pos ~S and sense ~S; making default entry ~%" w pos ont-types))
		; generate a default entry if there are no other entries 
		((and (not wdef) (not compatible-defs) (not (is-closed-class pos)))
		 (pushnew (car (make-default-unknown-word-entry w pos-list penn-tags domain-info)) compatible-defs)
		 (print-debug "No match for ~S with pos ~S and sense ~S; making default entry ~%" w pos ont-types)
		 )
		(t nil))
		)))
    (when (and (not tagged-senses-only) other-defs) ; return all senses with newly generated sense
      (when (non-default-filtered-senses compatible-defs) ; only append the generated senses if they're not default
	(print-debug "appending all senses to sense filtered def ~S~%" compatible-defs)
	(setq compatible-defs (append compatible-defs other-defs))))
    (setq compatible-defs (or compatible-defs wdef))

    compatible-defs)
  )

(defun retrieve-multiword-from-trips (words)
  (let ((wdef (retrieve-from-lex (car words)))
	res)
    (dolist (def wdef)
      (if (equal words (lex-entry-words def))
	  (pushnew def res)))
    res)
  )

(defun abstract-type (ont-type)
  (member ont-type '(ont::referential-sem ont::abstract-object ont::modifier ont::predicate ont::situation-root ont::phys-object ont::action ont::organization ont::geographic-region))
  )


(defun find-new-senses (wdef retrieved-sense-info domain-info tagged-ont-types replace-vbn-adj)
  (let (res  sense-introduced-this-cycle)
    (dolist (def (remove-if #'null wdef))
      (let* ((feats (lex-entry-feats def))
	     (this-pos (lex-entry-cat def))
	     (this-lf (strip-out-lf (get-feature-values feats 'w::lf))))
	(print-debug "FIND-NEW-SENSES: checking new sense ~S ~S~%  retrieved-sense-info is ~S~% is more abstract sense: ~S~% introduced this cycle: ~S~%" this-pos this-lf retrieved-sense-info (is-duplicate-sense this-pos this-lf retrieved-sense-info) sense-introduced-this-cycle)
	(cond  ((or
		; a non-null new sense
		(and (not (is-duplicate-sense this-pos this-lf retrieved-sense-info))
		     (not (null this-lf)))
	       ; a duplicate lf-type and pos pair, but introduced in the same processing cycle so it should have different argument structure
		(and  (equal sense-introduced-this-cycle (list this-lf this-pos)) (is-duplicate-sense this-pos this-lf retrieved-sense-info)))

	       ; add the new sense if it's acceptable (some CERNL hacks here)
	       (when (not (and tagged-ont-types
			       (abstract-type this-lf)))
		 (setq res (pushnew def res))
		 
		 ;; update the retrieved-sense-info
		 ;; CERNL/TT hack -- don't include domain-info for verbs if we are replacing with the domain-tagged adjectives, to make sure the adjective sense is preferred
		 (if (or (not (is-compatible-sense this-lf tagged-ont-types))
			 (and replace-vbn-adj (eq this-pos 'w::v)))
		     (setq retrieved-sense-info (pushnew (list this-lf this-pos) retrieved-sense-info))
		   (setq retrieved-sense-info (pushnew (list this-lf this-pos domain-info) retrieved-sense-info :test #'equal)))

		 (setq sense-introduced-this-cycle (list this-lf this-pos))
		 (print-debug "not duplicate -- adding new sense ~S~% updated sense list is ~S~%" (list this-lf this-pos) retrieved-sense-info)
	       ))
	      
	      ;; if we have a duplicate compatible sense, include the domain info for preference calibration
	       (t (if (and domain-info (is-compatible-sense this-lf tagged-ont-types))
		     ;; CERNL/TT hack -- don't include domain-info for verbs if we are replacing with the domain-tagged adjectives, to make sure the adjective sense is preferred
		      (if (or (not (equal this-pos 'w::v))
			      (and (equal this-pos 'w::v) (not replace-vbn-adj)))
			  (setq retrieved-sense-info
				(remove-duplicates (subst (list this-lf this-pos domain-info) (list this-lf this-pos) retrieved-sense-info :test #'equal) :test #'equal))))
		  (print-debug "duplicate sense ~S w/ domain info~% updated sense-triples are ~S~%" (list this-lf this-pos) retrieved-sense-info)
		 ))))
    (values res retrieved-sense-info))
  )

;; make sure strings are not embedded inside MWEs
(defun convert-to-symbol (w)
  (let (res)
    (cond ((listp w)
	   (dolist (el w)
	     (if (stringp el) (push (read-from-string el) res)
	       (push el res)))
	   (setq res (reverse res)))
    (t (if (stringp w) (push (read-from-string w) res))
	 (setq res w)))
    res)
  )

(defun ptb-filter (part-of-speech-tags w res tagged-sense)
  (let* ((penn-tags (util::convert-to-package (find-arg tagged-sense :penn-parts-of-speech) :w))
	 (semantics-from-penn-tag (penn-tag-to-trips-semantics penn-tags)))
    (cond ((and *use-underspecified-lexicon* (not (exclude-from-underspecification part-of-speech-tags))) ;; with underspecified lexicon
	   (if (eq 'w::in (base-penn-tag part-of-speech-tags))
	       (setq res (filter-by-pos (generate-underspecified-prepositions-for-entry w res part-of-speech-tags) w nil part-of-speech-tags))
	       (setq res (make-default-unknown-word-entry w nil part-of-speech-tags (find-arg tagged-sense :domain-specific-info))))
	   )    
	  (t  ;; treebank without underspecified lexicon
	   (setq res (filter-by-pos res w nil part-of-speech-tags))
	   (if semantics-from-penn-tag (setq res (prefer-senses-in-entry res semantics-from-penn-tag)))
	   (if (and *use-underspecified-prepositions* (eq 'w::in (base-penn-tag part-of-speech-tags)))
	       (setq res (generate-underspecified-prepositions-for-entry w res part-of-speech-tags))))
	  
	  (semantics-from-penn-tag ;; catch the extra semantic tag from penn treebank annotation if it exists but we don't have another treebank filter
	   (setq res (prefer-senses-in-entry res semantics-from-penn-tag)))
	  ((or part-of-speech-tags)
	   (setq res (adjust-pos-preferences res nil part-of-speech-tags)))	  
	  (t nil))
    res)
  )

; (lxm::get-word-def 'w::walk '( :wn-sense-keys ("walk%2:38:01")))
; (lxm::get-word-def 'w::walk '( :trips-parts-of-speech (w::n w::v) :penn-parts-of-speech (NN VBG) :ont-types (ont::self-locomote ont::walking) :wn-sense-keys ("walk%2:38:01")))
;  (lxm::get-word-def 'w::aardvark '(:trips-parts-of-speech (w::n) :penn-parts-of-speech (NN) :wn-sense-keys ("aardvark%1:05:00::" "ant_bear%1:05:01::)")))
 ; (lxm::get-word-def 'w::break '(:penn-parts-of-speech (VB) :tag (W::V) :wn-sense-keys ("break%2:29:04::"))) ;; evoke-injury sense, not in TRIPS
 ; (lxm::get-word-def 'w::show '(:penn-parts-of-speech (VB) :tag (W::V) :wn-sense-keys ("show%2:39:02"))) ; ont::show sense, in TRIPS
 ; (lxm::get-word-def 'w::Diltiazem '( :frame (3 12) :sense-info ((:penn-parts-of-speech (NNP NN) :trips-parts-of-speech (W::NAME W::N) :ont-types (ONT::PHARMACOLOGIC-SUBSTANCE)))))

(defun get-word-def (w keylist)
  (if (find-arg keylist :var-prefix)
      (setq *var-prefix* (string (find-arg keylist :var-prefix)))
      (setq *var-prefix* "V"))
  (let* ((w (util::convert-to-package w :w))
	 (w (convert-to-symbol w))
	 (wdef (if (listp w) (retrieve-multiword-from-trips w) (retrieve-from-lex w)))
	 (tagged-senses (clean-up-tagged-senses (find-arg keylist :sense-info)))
	 (first-tagged-sense (car tagged-senses)) ; only one sense for ptb
	 (trips-sense-info (get-lf w :wdef wdef))	 
	 (retrieved-sense-info trips-sense-info)
	 res new-wdef tagged-ont-types part-of-speech-tags domain-tagged-senses replace-vbn-adj
	 )
    (print-debug "GET-WORD-DEF: TRIPS senses for ~S are ~S~% tagged senses are ~S~%" w trips-sense-info tagged-senses)
    (cond ((not wdef)
	   (incf *unknown-word-tokens*)
	   (pushnew w *unknown-words* :test #'equal)
	   )
	  (t 
	   (when (not (equal w 'w::punc-period))
	     (incf *known-word-tokens*)
	     (pushnew w *known-words* :test #'equal)
	     )
	   ))
    (if (not (exclude-from-lookup w (if wdef (list (car (lex-entry-description (car wdef))))) nil)) 
      (cond (tagged-senses ;; text-tagger information, such as pos or ont-type, accompanies word reqest; loop through each tagged sense and search for a compatible sense in TRIPS or WF; if none found, generate default senses based on the tagged information and adjust sense preferences accordingly (e.g. in CERNL, senses with UMLS info are preferred above others)
	   (dolist (this-sense-keylist tagged-senses)
	     (let* ((domain-info (find-arg this-sense-keylist :domain-specific-info))
		    (penn-tags (util::convert-to-package (find-arg this-sense-keylist :penn-parts-of-speech) :w))
		    (these-ont-types (find-arg this-sense-keylist :ont-types))
		 
		    )
		 
		 ;; CERNL/TT hack: TT often assigns VBN tags for adjectives so make a substitution if incompatible POS and ont-type found
		 (when (and (not (compatible-tagging penn-tags these-ont-types)) (find 'w::VBN penn-tags) (find 'ont::has-medical-condition these-ont-types))
		   (setq penn-tags (subst 'w::JJ 'w::VBN penn-tags))
		   (setq this-sense-keylist (subst penn-tags (find-arg this-sense-keylist :penn-parts-of-speech) this-sense-keylist))
		   (setq replace-vbn-adj t)
		   )
	
		 (when (not (exclude-from-lookup w nil nil))
		  ;; obtain new senses from WordFinder, if any
		   (multiple-value-bind (new-wdef new-sense-info)		    
		       (find-new-senses (process-word-request w this-sense-keylist wdef) retrieved-sense-info domain-info these-ont-types replace-vbn-adj)
		     (print-debug "new wdef is ~S~%" new-wdef)
		     (print-debug "retrieved-senses are ~S~% new senses are ~S~%" retrieved-sense-info new-sense-info)
		     (setq res (append new-wdef res))
		     (setq part-of-speech-tags (remove-duplicates (append penn-tags part-of-speech-tags)))
		     (setq tagged-ont-types (remove-duplicates (append these-ont-types tagged-ont-types)))
		     (setq domain-tagged-senses (remove-duplicates (append domain-info domain-tagged-senses)))
		     (setq retrieved-sense-info new-sense-info)
		     ))

		 ;; CERNL/TT hack: adjust word sense preferences to prefer external tagging
		 (setq res (prefer-more-specific-tagged-senses w res tagged-ont-types penn-tags domain-info)) ; prefer any TT-tagged ont-type
		 ;; update the domain-specific-info in both the new entries from wordnet (res) and the trips def (wdef)
		 (setq res (update-domain-info-in-entries res retrieved-sense-info))
		 (setq wdef (update-domain-info-in-entries wdef retrieved-sense-info))
		 (if domain-tagged-senses (setq res (prefer-more-specific-domain-tagged-senses res))) ; prefer external domain (e.g., UMLS) tagged senses
		 )))

	    ;; no text-tagger information with word request
	    (t
	     (when (not (exclude-from-lookup w nil nil))
	       (when (or (not trips-sense-info)
			 (and trips-sense-info (not *unknown-words-only*)))
		 (print-debug "calling WordFinder ~%" w) ;; call WF if no TRIPS info, or if settings want WF wdefs also 
		 (setq res (find-new-senses (process-word-request w nil wdef) retrieved-sense-info nil nil nil))))))
      ;;  even though we don't look up the word, we might refine the sense from tagged information
      ;;   I AM DISABLING THIS FOR NOW AS IT CAUSES PROBLEMS (e.g., changing specifier sense of "this") 
      ;;(setq wdef (refine-existing-entry-with-sense-info wdef tagged-senses))
      )
    ; combine new or default senses from external sources w/ existing trips senses
    (print-debug "~%After processing RES=~S ~%     WDEF=~S" res wdef)
    (setq res (append res wdef))
    (cond  ;; moved this earlier in processing to use be able to pass in penn-tags and domain-info
	   ;(tagged-ont-types ;; there is sense information from Text Tagger; make adjustments/hacks
	   ;; insert TT-tagged domain specific information (i.e., UMLS info), if any, into the lexical entries
;	   (setq res (update-domain-info-in-entries res retrieved-sense-info))
	  ; )
      
	 
	   ;; CERNL/TT hack: adjust word sense preferences to prefer external tagging
;	   (setq res (prefer-more-specific-tagged-senses w res tagged-ont-types penn-tags domain-info)) ; prefer any TT-tagged ont-type
;	   (if domain-tagged-senses (setq res (prefer-tagged-domain-senses res))) ; prefer external domain (e.g., UMLS) tagged senses
;	   )

	  ;; lexical filtering for the penn treebank domain
	  ((and *use-tagged-pos-only* part-of-speech-tags)
	   (setq res (ptb-filter part-of-speech-tags w res first-tagged-sense)))
	  (t nil))

     ; lxm never returns nil -- create an entry for the parser based on available input
    (when (null res)
      (cond ((and (listp w) tagged-senses) ;; domain-tagged multiwords; loop through each tagged sense and use the info in the default entry if possible
	     (setq w (remove-noninitial-hyphens w))
	     ; turning this off in CERNL so we get 30-mg rather than THIRTY-MG
;	     (if (contains-number w) (setq w (convert-number-to-word w)))
	     (print-debug "warning:: creating underspecified entry for domain-tagged multiword ~S~%" w)
	     (dolist (this-sense-keylist tagged-senses)
	       (let* ((domain-info (find-arg this-sense-keylist :domain-specific-info))
		      (score (find-arg this-sense-keylist :score))
		      (penn-tags (util::convert-to-package (find-arg this-sense-keylist :penn-parts-of-speech) :w))
		      (these-word-categories (merge-pos-info nil penn-tags))
		      (these-ont-types (find-arg this-sense-keylist :ont-types)))
		 (dolist (ont-type these-ont-types)
		   (dolist (cat these-word-categories)
		     (when (compatible-pos-and-ont-type cat ont-type)
		       (setq res (append res (make-unknown-word-entry w cat score NIL (gen-id w) (list ont-type) nil penn-tags these-ont-types domain-info))))
		     ))
		 (if (null res) (setq res (append res (make-default-unknown-word-entry w nil penn-tags nil))))
	       )))
	    (t  (print-debug "warning:: LXM found no entries -- returning default entry ~%")
		(setq res 
		      (if tagged-senses
			  (mapcar #'(lambda (x) (car (make-default-unknown-word-entry w nil part-of-speech-tags x))) tagged-senses)
			  (make-default-unknown-word-entry w nil part-of-speech-tags nil))))
	    ))
    ;; TRIPS domain-specific sense preference adjustment
    (if om::*domain-sense-preferences*
	(setq res (prefer-domain-senses w res)))
    (setq res (remove-if #'null res))
    (if res
	(progn
	  (print-debug "returning entries to parser for ~S ~S~%" w res)
	  (listify-lex-entry res)); listify for the parser
	;; making unknown word entry failed, make generic entry
	(listify-lex-entry (make-default-unknown-word-entry w nil '(w::n) nil)))
    ))

(defun is-defined-word (w &key (use-wordfinder *use-wordfinder*))
  "Return T iff the word w has a real definition (not just referential-sem)."
  (let* ((*use-wordfinder* use-wordfinder)
	 (defs (get-word-def w nil)))
    ;; it's defined if we didn't just make up a single referential-sem sense
    (not (and (= 1 (length defs))
	      (eq 'ont::referential-sem
		  (second (second (assoc 'w::lf (cddr (nth 3 (car defs))))))
		  )))))

(defun refine-existing-entry-with-sense-info (wdef sense-info)
  (let ((ont-type (car (find-arg (car sense-info) :ont-types))))
    (if ont-type
	(mapcar #'(lambda (x) (refine-existing-sense x ont-type)) wdef)
	wdef)))

(defun refine-existing-sense (def ont-type)
  (let* ((desc (lex-entry-description def))
	 (words (lex-entry-words def))
	 (oldlf (parser::get-fvalue (cdr desc) 'w::lf)))
    (if (or (eq oldlf 'w::expletive) (> (list-length words) 1))
	def
	(progn
	  (setf (lex-entry-description def)
		(cons (car desc) 
		      (replace-feat (replace-feat (cdr desc) 'w::lf (replace-ont-type oldlf ont-type))
				    'w::sem (replace-sem-type (parser::get-fvalue (cdr desc) 'w::sem) ont-type))))
	  def))))

(defun replace-ont-type (oldlf ont-type)
  (if (consp oldlf)
      (list :* ont-type (third oldlf))
      ont-type))

(defun replace-sem-type (oldsem ont-type)
  (if (not (equal oldsem '($ -)))
    (cons (car oldsem) (cons (refine-abstract-class (cadr oldsem) ont-type)
			     (replace-feat (cddr oldsem) 'f::type ont-type)))
    oldsem
    ))

(defun refine-abstract-class (oldclass ont-type)
  (if (and (consp oldclass) (eq (car oldclass) 'om::?))
      'F::PHYS-OBJ
      oldclass))
  

(defun clean-up-tagged-senses (types)
  "various hacks to reduce the amount of texttagger senses, and remap when necessary"
  (let* ((newtypes (subst 'ont::EVENT-OF-CHANGE 'ont::procedure types))  ;; temporary 
	 ;; if certain senses have domain-specific-info, ignore the others
	 (newertypes (if (find-if #'(lambda (x) (find-arg x :domain-specific-info)) newtypes)
		  (remove-if-not #'(lambda (x) (find-arg x :domain-specific-info)) newtypes)
		  newtypes))
	 ;; if we have a tagged protein ignore the others
	 (res  newertypes))
    #||(res (if (find-if #'(lambda (x) (member 'ont::PROTEIN (find-arg x :ont-types))) newertypes)
		  (mapcar #'(lambda (x) (reduce-ont-type x 'ont::protein))
			  (remove-if-not #'(lambda (x) (member 'ont::PROTEIN (find-arg x :ont-types))) newertypes))
		  newertypes)))||#
    (print-debug "Cleaned up senses are ~S" res)
    res))
    
(defun reduce-ont-type (sense ont-type)
  (replace-arg sense :ont-types (list ont-type)))

(defun process-word-request (w keylist wdef)
  "
 get-word-def (w)
 @param w : word 
 @return  : the lexicon entry for the word (a lex-entry structure) in list format for message passing
            this lexicon entry retrieval incorporates ontologymanager info in the lex definition (sem features, roles)
 @visibility public
 "
 (let* ((trips-pos-list (find-arg keylist :trips-parts-of-speech))
	(wn-sense-keys (remove-if #'wf::stoplist-p
				  (find-arg keylist :wn-sense-keys)))
	(rawscore (find-arg keylist :score))
	(score (if (numberp rawscore) (max rawscore .95) .98))
	(wn-pos-list (when wn-sense-keys (trips-pos-for-wn-sense-keys wn-sense-keys)))
	(domain-info (find-arg keylist :domain-specific-info))
	(merged-trips-wn-pos-list (union trips-pos-list wn-pos-list))
	(penn-tags (util::convert-to-package (find-arg keylist :penn-parts-of-speech) :w))
	(semantics-from-penn-tag (penn-tag-to-trips-semantics penn-tags))
	(ont-sense-tags (find-arg keylist :ont-types))
	res
	(*package* *lxm-package-var*)	 
	(wf-poslist (or merged-trips-wn-pos-list *default-wf-poslist*)) ;; use default pos list if no pos passed in from parser
	(this-trips-pos-list (if (listp w) (trips-pos-list-for-word wdef :include-multiwords t) (trips-pos-list-for-word wdef :include-multiwords nil)))
	(this-trips-sense-list (get-lf w :wdef wdef))
	(wf-wdef (when (and *use-wordfinder*
			    (not (exclude-from-lookup w this-trips-pos-list penn-tags)))
		   (if wdef  ;;  there are TRIPS entries for the word
		      (if *use-trips-and-wf-senses*   ;; check if we should get the WN defs anyway
			  (get-unknown-word-def w :pos-list wf-poslist :penntag penn-tags  :ont-sense-tags ont-sense-tags :trips-sense-list this-trips-sense-list :wn-sense-keys wn-sense-keys :score score)
			  ;; word in trips; retrieve only WF senses from non-trips pos
			  (if (set-difference wf-poslist this-trips-pos-list) 
			      (get-unknown-word-def w :pos-list (set-difference wf-poslist this-trips-pos-list) :penntag penn-tags  :ont-sense-tags ont-sense-tags :trips-sense-list this-trips-sense-list :wn-sense-keys wn-sense-keys :score score)))
		      ;; there are no entries in TRIPS
		      (get-unknown-word-def w :pos-list wf-poslist :penntag penn-tags :ont-sense-tags ont-sense-tags :trips-sense-list this-trips-sense-list :wn-sense-keys wn-sense-keys :score score))))
	
	(backup-wf-wdef (if (and *use-wordfinder* merged-trips-wn-pos-list (null wf-wdef)) ;; null lookup using pos from TT

			    ;; so try again using default pos list and no sense tags, in case of bad tagging
			    (if (not wdef) (get-unknown-word-def w :pos-list *default-wf-poslist* :penntag penn-tags :score score)
			      (if (and *use-trips-and-wf-senses* (not (exclude-from-lookup w this-trips-pos-list penn-tags)))
				  (get-unknown-word-def w :pos-list (or (set-difference *default-wf-poslist* this-trips-pos-list) *default-wf-poslist*) :penntag penn-tags :score score)))))
	(final-wf-wdef (or wf-wdef backup-wf-wdef))
	(adjusted-wdef  (if *use-domain-senses* (boost-domain-senses wdef) wdef))
	;; if using combined senses and there is a WF word definition, lower the WF definition score and append it to the TRIPS word definition
	(combined-wdef (if (and final-wf-wdef adjusted-wdef) (append adjusted-wdef final-wf-wdef)))
	;; probabilities are adjusted based on sense and pos tags -- so we no longer automatically prefer trips senses
;			   (if *use-trips-and-wf-senses* (append (adjust-probability final-wf-wdef -.04))
;			     (append adjusted-wdef final-wf-wdef))))
	)
   ;; now get TRIPS senses from the calling function only
   (setq res (or combined-wdef adjusted-wdef final-wf-wdef))
  
   ;; filter the results according to tagging information: senses or pos
   (cond (;(and (not (find 'w::name (merge-pos-info trips-pos-list penn-tags))) ; no sense filtering on names
	  (or (not (null ont-sense-tags)) (not (null wn-sense-keys)));)
          (setq res (or (filter-by-senses res w merged-trips-wn-pos-list penn-tags ont-sense-tags wn-sense-keys domain-info :tagged-senses-only *use-tagged-senses-only*) res)))
	 
         ;; lexical filtering for the penn treebank domain
         ((and *use-tagged-pos-only* (or trips-pos-list penn-tags))
;;	   ;; lexical filtering for the penn treebank domain
;;	  ((and *use-tagged-pos-only* part-of-speech-tags)
;;	   (setq res (ptb-filter part-of-speech-tags w res first-tagged-sense)))
           (cond ((and *use-underspecified-lexicon* (not (exclude-from-underspecification penn-tags))) ;; with underspecified lexicon
                 (if (eq 'w::in (base-penn-tag penn-tags))
                     (setq res (filter-by-pos (generate-underspecified-prepositions-for-entry w res penn-tags) w merged-trips-wn-pos-list penn-tags))
                   (setq res (make-default-unknown-word-entry w merged-trips-wn-pos-list penn-tags domain-info)))
                 )		
                (t  ;; treebank without underspecified lexicon
                 (setq res (filter-by-pos res w merged-trips-wn-pos-list penn-tags))
                 (if semantics-from-penn-tag (setq res (prefer-senses-in-entry res semantics-from-penn-tag)))
                 (if (and *use-underspecified-prepositions* (eq 'w::in (base-penn-tag penn-tags)))
                     (setq res (generate-underspecified-prepositions-for-entry w res penn-tags))))))
         (semantics-from-penn-tag ;; catch the extra semantic tag from penn treebank annotation if it exists but we don't have another treebank filter
          (setq res (prefer-senses-in-entry res semantics-from-penn-tag)))
         ((or merged-trips-wn-pos-list penn-tags)
          (setq res (adjust-pos-preferences res trips-pos-list penn-tags)))
         (t nil))
   (print-debug "process-word-request returns ~S~%" res)
   res)
 )

(defun add-word-def-if-necessary (w pos)
  "The parser calls this when it sees an entry for a composite word -- if one of the subsequent 
   words in the composite is undefined in its own right (the first is always defined), then we
   need to add a dummy entry to the lexicon which causes the lexiconmanager to generate a word entry
   if later seen. 

 @param  w   : the word to add
 @param  pos : part of speech of the dummy word
 @return     : dummy lex-entry for non-initial lexemes in multi-word entry
 @visibility public
"
  
  (let ((lw (util::convert-to-package w :w)))   
    (if (symbolp lw)
	(if (not (retrieve-from-lex lw))	    
	    (let ((id (gen-id lw)))
	      (push (list :none 
			  id
			  (make-vocabulary-entry :word lw :pos pos
						 :senses (list 							
							  (make-word-sense-definition
							   :name id
							   :pos pos
							   :word lw
							   ;; Myrosia 2004/6/17 changed to default preference mechanism
							   ;; because we don't need to boost regular words
							   :pref 1
							   ))
						 ))
		    (gethash lw (lexicon-db-word-table *lexicon-data*))
		    )
	      ))
      )
    ))


(defun add-names (&key type content)
  " 
;; called from e.g. webanalyzer to dynamically update lxm list of web links or other names
;; add-names (:type 'link :content '(books (your account) cart))
;;
;; @param w        : type e.g. link (for web link) -- currently no other types
;; @param content  : list of names to be added -- multi-word expressions are in a list
;;             
;; @return    : (success) if any words added, otherwise (failure)
;; 
;;
@visibility public
"
  (let ((res '(failure)))
    (if (and content (listp content))
	(case type
	  ;; add web links as names of type icon (the same type as w::link)	
  (link (setq res (dynamic-add-lexeme content 'w::name 'ont::text-representation 'name-templ)))
	  (label (setq res (dynamic-add-lexeme content 'w::name 'ont::text-representation 'name-templ)))
	  (otherwise (lexiconmanager-warn "invalid type specification ~S! no names added~%" type)
		     )
	  )
      (lexiconmanager-warn "invalid content specification -- no names added!~%")
      )
    res)
  )


(defun add-lex-entry (&key word cat type)
  " 
;; called to dynamically update lxm with a word
;; add-lex-entry (:word w::block :cat w::N :type ont::phys-obj)
;;
;; @param word  : word to be added
;; @param caat  : part of speech category   
;; @param type  : ontology type -- must be an existing one
;;             
;; @return    : (success) if word added, otherwise (failure)
;; 
;;
@visibility public
"
  (let ((res '(failure)) 
	templ)
    (print-debug "received request to add word ~S as ~S ~S ~%" word cat type)
    (if (and (and word cat type) (compatible-pos-and-ont-type cat type))
	(case cat
	  (w::n 
	   (setq templ 'count-pred-templ)
	   ;(setq type 'ont::phys-object)
	   (setq res (dynamic-add-lexeme (list word) 'w::n type templ)))
	  (w::v
	   (setq templ 'arg0-arg1-xp-templ)
	   ;(setq type 'ont::situation-root)
	   (setq res (dynamic-add-lexeme (list word) 'w::v type templ)))
	  (w::adj
	   (setq templ 'central-adj-templ)
	   ;(setq type 'ont::predicate)
	   (setq res (dynamic-add-lexeme (list word) 'w::adj type templ)))
	  (otherwise (lexiconmanager-warn "invalid specification for ~S ~S ~S! no word added~%" word cat type)
		     )
	  )
	(lexiconmanager-warn "invalid lexical entry addiiton request for ~S ~S ~S -- no word added!~%" word cat type)
	)
    res)
  )
 
(defun clear-dynamic-lexicon ()
    " 
;; called whenever LxM sees a '(start-conversation) -- on system load or restart
;;
@visibility public
"
  (reinit-dynamic-lexicon)
  )


(defun get-aliases (&key content lftype)
  " 
;; get-aliases (w)
;; w could be either a symbol fl or a list if multi-word
;; return list of aliases for the given word, otherwise nil
;; aliases for a w will have the same lfform -- e.g. w::florida has lfform w::florida, and so does w::fl
;;
@visibility public
"
  (let* ((w (util::convert-to-package content :w))
	 (results nil)
	 (this-result nil)
	 (aliases nil)
	 (w1 (if (listp content) (first w) w))) ;; get the first word if it's a list
    (if (not lftype) ;; we don't know which sense of the word to match
	(let ((lflist (get-lf w1)))
	  (dolist (lf-pair lflist) ;; search all senses
	    (let* ((this-lf (first lf-pair)))
	      (setq aliases (get-aliases-for-sense w this-lf))
	      (when aliases (setq this-result (list this-lf aliases))		
		    (pushnew this-result results :test #'equal)))))
      ;; we know the sense of the word
      (progn
	(setq aliases (get-aliases-for-sense w lftype))
	;; embed the result in a list for consistency w/ case where multiple lftypes might match
	(when aliases (setq results (list (list lftype aliases))))))
	(setq results (list 'aliases ':content results))
     results)
  )

(defun get-sem-for-lf (lftype)
  "given an lftype, return the sem feature list with the $ -- for the parser"
  (make-type-spec (get-lf-sem lftype :no-defaults nil) :semvar nil :feature-list-sign '$))


(defun get-sem-feature-value (lftype feature)
  "given an lftype and feature, return the sem feature value"
  (cdr (assoc feature (cddr (make-type-spec (get-lf-sem lftype :no-defaults nil) :semvar nil :feature-list-sign '$))))
  )

(defun is-reln (word lftype)
  "return the subcat role if word with this lftype is a relational noun, otherwise nil. This is used by IM to resolve possessive :assoc-with relations"
  (let ((w (util::convert-to-package word :w)))
    (remove-if #'null
	       (mapcar (lambda (def)
			 (let* ((feats (lex-entry-feats def))
				(this-pos (lex-entry-cat def))
				(this-lf (strip-out-lf (get-feature-values feats 'w::lf))))
			   (if (and (eq this-pos 'w::N) this-lf (eq this-lf lftype))
			       (get-feature-values feats 'w::subcat-map))))
			 (retrieve-from-lex w)))))

(defun get-particle (part-structure)
  "extract the actual word (e.g. w::up) from the lexical entry particle structure"
  (let ((particle (member 'w::part (cdr part-structure))))
    (if particle ;; e.g. (W::PART (W::LEX W::UP))
	(second (member 'w::lex (second particle))))
    )
  )

(defun is-particle-verb (lexeme lftype)
  "
 lexeme should be a list (multi-word), e.g. (w::pick w::up)
 lftype e.g., ont::pickup
 return (T) if word with this lftype is a particle verb, otherwise nil. This is used by SG to determine if a multi-word verb is eligible for particle verb surface variation: pick up the box / pick the box up."
  (let* ((this-lex (util::convert-to-package lexeme :w))
	(first-word (if (listp this-lex)(first this-lex)))
	(second-word (if (listp this-lex)(second this-lex)))
	)
      (remove-if #'null
	       (mapcar (lambda (def)
			 (let* ((feats (lex-entry-feats def))
				(this-pos (lex-entry-cat def))
				(this-lf (strip-out-lf (get-feature-values feats 'w::lf))))
			   (when (and (eq this-pos 'w::v) this-lf (eq this-lf lftype))
			     (equal (get-particle (get-feature-values feats 'w::part)) second-word))
			   ))
			 (retrieve-from-lex first-word)))
      ))
   
(defun get-prototypical-words-from-type (lftype)
  "
 get-prototypical-words-from-type (lftype)

 @param lf : lftype (e.g. ont::move)
 @return   : list of prototypical words

 @desc (get-words-from-type 'ont::hole) returns (w::hole)
 @visibility public

"
  (let (res firstword secondword)
    (mapcar (lambda (wordpair)
	      (cond ((listp (first wordpair))
		     (setq firstword (first (first wordpair)))
		     (setq secondword (second (first wordpair))))
		    (t (setq firstword (first wordpair))
		       (setq secondword nil)))
	      (dolist (sense (getworddef firstword))
		(let ((this-lftype (if (listp (word-sense-definition-lf sense)) (second (word-sense-definition-lf sense))
				     (word-sense-definition-lf sense))))
		  (if secondword
		      (if (and (equal this-lftype lftype) (word-sense-definition-prototypical-word sense)
			       (or (equal (first (word-sense-definition-remaining-words sense)) secondword)
				   (equal (get-particle (get-feature-values (word-sense-definition-syntax sense) 'w::part)) secondword)))
			  (pushnew (list firstword secondword) res :test #'equal))
		    (if (and (equal this-lftype lftype) (word-sense-definition-prototypical-word sense))
			(pushnew firstword res :test #'equal))))))		
	    (get-words-from-lf lftype))
    res)
  )

 
 
