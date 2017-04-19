;; wordfinder-interface.lisp
;;
;; Mary Swift
;;
;; Functions used to convert results returned by the WordFinder package into entries in the 
;; lexicon
;;

(in-package "LEXICONMANAGER")

;; a sample list of entries returned by WordFinder

(defun found-wordp (wf-info)
  (equal (car wf-info) 'SUCCESS)
)


(defvar *wf-sense-limit* 2) ;; by default return a small number of senses for each matched part of speech to control ambiguity (note that if a WordNet synset is included in the unknown word call, that overrides the sense limit and WF returns entries for just that synset.

(defvar *default-wf-poslist* '(w::n w::v w::adj w::adv)) ;; the part of speech list used by WordFinder to filter its unknown word search 

(defun limit-wf-senses-on (sensenum)
  (setq *wf-sense-limit* sensenum)
  )

(defun limit-wf-senses-off ()
  (setq *wf-sense-limit* 50) ; return (quasi) unlimited senses for each part of speech matched in WordNet
  )

;; by default names are excluded from the WF search as we get them from TextTagger -- this function includes them to the part of speech list that is used to filter the WordNet search
(defun include-wn-names ()
  (setq *default-wf-poslist* '(w::n w::v w::adj w::adv w::name))
  )

(defun is-name-tag (poslist penn-tag)
  (let ((penn-tag  (util::convert-to-package penn-tag :w)))
    (or (find 'w::name poslist) (find 'W::NNP penn-tag ) (find 'W::NNPS penn-tag))
  ))

(defun create-fake-WF-result-from-ont-type (w senses pos-list score)
  (print-debug "Creating an entry from the ONT type")
  (if senses
    (concatenate 'string "(success :entries
	      ((ENTRY :word \"" (make-into-string w) "\" :score " (format nil "~S" score) " :LFS " (format nil "~S" senses) " :constit (% " (format nil "~S" (car pos-list)) "))))")
    (concatenate 'string "(failure :word \"" (make-into-string w) "\"")))

(defun make-into-string (obj)
  (when obj
        (cond ((symbolp obj) (symbol-name obj))
	  ((consp obj)
	   (if (cadr obj)
	       (concatenate 'string (make-into-string (car obj)) "_"
			    (make-into-string (cdr obj)))
	       (make-into-string (car obj)))))))
  

(defun get-unknown-word-def (w &key (senselimit *wf-sense-limit*) pos-list penntag ont-sense-tags trips-sense-list wn-sense-keys score)
 ;; (when (and (not *no-wf-senses-for-words-tagged-with-ont-types*) (null ont-sense-tags))
  (let* ((wf-string 
	  ;;  In the case where we have an ont-type (and we don't want other senses), we "fake" WF 
	  (if (or (not *no-wf-senses-for-words-tagged-with-ont-types*) (null ont-sense-tags))
	      (wf::lookup-unknown-word-string w :senselimit senselimit :poslist pos-list :penntag penntag :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys)
	      (create-fake-WF-result-from-ont-type w ont-sense-tags pos-list score)))
	 (wf-entry (and wf-string (stringp wf-string) (read-from-string wf-string)))
	 senselist res)
    (print-debug "WF returns ~S~%" wf-entry )
    
    ;; filter these if we are filtering by ont::type
    (when (and (found-wordp wf-entry) (not (null wf-entry)))
      ;; make a new TRIPS lex-entry for each lftype matched
      (dolist (this-wf-entry (remove-if #'null (third wf-entry)))
	(let ((lflist (get-wf-lfs this-wf-entry))
	      (pos (get-wf-constit this-wf-entry))
	      )
	  ;; make lexical entries for all senses in the lflist
	  
	  (dolist (this-lf lflist) 
	    (when (not (member (list pos this-lf) senselist :test #'equal)) ;; skip duplicate senses
	    #||  ;; check for hidden names: tagged as N, not penn taged as NNS and starts with a capital  (not sure what problem this solved - isit obsolete?)
	      (if (and (eq pos 'w::n) (not (member 'w::nns penntag))
		       (is-instance this-wf-entry)) (setq pos 'w::name))||#
	      (let* ((score (get-wf-score this-wf-entry))
		     (feats (get-wf-feats this-wf-entry))
		     (wf-word (get-wf-word this-wf-entry))
		     (domain-info (get-wf-domain-info this-wf-entry))
		     (wid (gen-id wf-word))
		     (w (if (and (listp w) (find 'w::punc-minus w)) (remove 'w::punc-minus w) w)) ;; handle hyphenation 
		     (new-entry (make-unknown-word-entry w pos score feats wid (list this-lf) trips-sense-list penntag ont-sense-tags domain-info)))
		(when new-entry
		  (pushnew (list pos this-lf) senselist :test #'equal)
		  ;; return a list of lex-entry structures
		  (if (listp new-entry)
		      (dolist (entry-sense new-entry)
			(pushnew entry-sense res))
		      (pushnew new-entry res))
		  ))))))
      
      (print-debug "trips senses for word ~S are ~S~%" w trips-sense-list)
      (print-debug "new senses for word ~S are ~S~%" w senselist));)))
    (setq res (prefer-more-specific-senses res))
    res)
  )

(defun getwordDefs+multi (word)
  "this is getworddefs extended to handling multiwords"
  (if (consp word)
      (remove-if-not #'(lambda (x)
			 (or (equal (cdr word) (vocabulary-entry-remaining-words (third x)))
			     (eq (cadr word) (vocabulary-entry-particle (third x)))))
			 (getworddefs (car word) *lexicon-data*))
      (getworddefs word *lexicon-data*)))
      
(defun get-senses-for-words (w words-to-replicate lf pos)
  (let (filtered-words templates-to-replicate senses-to-replicate)
    (dolist (word-pair words-to-replicate)
      ;; we eliminate senses for incompatible parts of speech - but note that noun senses can take verb ones (as nominalizations)
      (if (or (eq (second word-pair) pos) 
	      (and (member pos '(w::N w::NAME)) (eq (second word-pair) 'w::V)))  ;;nominalization
	  (pushnew (first word-pair) filtered-words  :test #'equal)))
    (dolist (word filtered-words)
      (dolist (this-def (getwordDefs+multi word))  
	(let* ((x (third this-def))
	       (these-senses (vocabulary-entry-senses x)))
	  (when (or (eq (vocabulary-entry-pos x) pos)
		    (and (eq (vocabulary-entry-pos x) 'W::V) (member pos '(w::n w::name))))  ;; nominalization
	    (dolist (sense these-senses)
	      (if (eq (sense-definition-lf-parent sense) lf)
		  (pushnew (list (sense-definition-templ sense) (sense-definition-params sense)) templates-to-replicate  :test #'equal))
	      )))))
    ;; create the replicated senses
    (print-debug "~% templates to replicate = ~S" templates-to-replicate)
    (dolist (template templates-to-replicate)
      (pushnew (make-sense-definition :pos pos :lf (list :* lf w) :nonhierarchy-lf nil
								  :pref *no-kr-probability*
								  :lf-form (if (listp w) (make-into-symbol w) w)
								  :lf-parent lf
								  :templ (first template)
								  :params (second template)
								  :boost-word nil) senses-to-replicate :test #'equal))
    senses-to-replicate)
  )


(defun make-default-sense (w lf pos)
  (let ((lfform (if (listp w) (make-into-symbol w) w))
;;	(default-templs '(cause-affected-xp-templ cause-templ affected-templ))
	(default-templs '(arg0-arg1-xp-templ arg0-templ))
	senses-to-replicate)
    (print-debug "creating default entry for ~S with ~S" w lf)
    (dolist (default-templ default-templs)
    (pushnew (make-sense-definition :pos pos :lf (list :* lf lfform) :nonhierarchy-lf nil
								     :pref *no-kr-probability*
								     :lf-form lfform
								     :lf-parent lf
								     :templ default-templ
								     :params nil
								     :boost-word nil) senses-to-replicate))
    senses-to-replicate))

(defun verb-senses-to-replicate (w lftype pos trips-sense-list)
  "retrieve trips verb senses to use as models for this new verb sense -- this gets us verb roles, selectional restrictions and syntactic linking templates that are specific to the lf type"
  (let (senses-to-replicate model-words)
    (cond ((or (eq lftype 'ont::situation-root) (null (get-words-from-lf lftype)))
	   (if (null trips-sense-list) ; only generate a default sense if there are no trips senses
	       (setq senses-to-replicate (make-default-sense w lftype pos))))
	  ((setq model-words (get-words-from-lf lftype))
	   (setq senses-to-replicate (get-senses-for-words w model-words lftype pos)))
	  ((and (not (eql (om::get-parent lftype) 'ont::situation-root))
		(setq model-words (get-words-from-lf (om::get-parent lftype))))
	   (setq senses-to-replicate (get-senses-for-words w model-words lftype pos)))
	  (t (setq senses-to-replicate (make-default-sense w lftype pos))))
    senses-to-replicate)
  )

(defun replicate-senses (w lftype pos trips-sense-list)
  "retrieve trips senses to use as models for this new word. this gets us roles, selectional restrictions and syntactic linking templates that are specific to the lf type"
  (let (senses-to-replicate model-words)
    (case pos
      ((w::v w::n w::name) (setq senses-to-replicate 
			 (if trips-sense-list (verb-senses-to-replicate w lftype pos trips-sense-list)
			     (get-senses-for-words w (get-words-from-lf lftype) lftype pos))))
      (otherwise  
       (setq model-words (get-words-from-lf lftype))
       (setq senses-to-replicate (get-senses-for-words w model-words lftype pos))))
    senses-to-replicate)
  )

(defun make-replica-entry (w lf pos trips-sense-list)
  "create a vocabulary-entry-structure with the senses to replicate"
  (let ((senses-to-replicate (replicate-senses w lf pos trips-sense-list))
	new-entry)
    (print-debug "replicating senses in trips-sense-list ~S~%" trips-sense-list)
    (when senses-to-replicate
      (setq new-entry (make-vocabulary-entry
		       :word w
		       :pos pos
		       :boost-word t
		       :wfeats  `((W::morph (:forms NIL)))
		       :senses senses-to-replicate)))
     new-entry))

(defun base-penn-tag (penn-tags)
  "Return the base tag of a word-level penn-tag with a functional tag, e.g. IN-PRP -> W::IN"
  (if (listp penn-tags) (setq penn-tags (car penn-tags)))
  (util::convert-to-package (base-from-hyphenated-symbol penn-tags) :w)
  )

(defun is-trips-pos (pos)
  (find pos '(w::n w::adj w::conj w::adv w::v w::art w::name w::number w::number-unit))
  )

(defun penn-tag-to-trips-pos (penn-tags)
  "conversion between penn treebank word-level tags and trips part of speech tags for content words"
  (case (base-penn-tag penn-tags)
    (w::CC   'w::conj)   ; conjunction
    (w::IN   'w::adv)    ; could also be prep...
    (w::JJ   'w::adj)    ; adjective
    (w::JJR  'w::adj)    ; comparative adj
    (w::JJS   'w::adj)   ; superlative adj
    (w::NN   'w::n)      ; noun, singular or mass
    (w::NNS  'w::n)      ; noun plural
    (w::NNP  'w::name)   ; proper noun sing
    (w::NNPS 'w::name)   ; proper noun pl
    (w::RB   'w::adv)    ; adverb
    (w::RBR  'w::adv)    ; comparative adv
    (w::RBS  'w::adv)    ; superlative adv
    (w::MD   'w::v)      ; modal verb
    (w::VB   'w::v)      ; base verb
    (w::VBD  'w::v)      ; past tense verb
    (w::VBG  'w::v)      ; gerund
    (w::VBN  'w::v)      ; past participle
    (w::VBP  'w::v)      ; non-3rd person singular present
    (w::VBZ  'w::v)      ; 3rd person singular present
    (w::WRB  'w::adv)    ; wh-adverb
    (otherwise nil))
  )

(defun penn-tag-to-trips-syntax (penn-tags)
  "convert penn treebank tags (from TextTagger) into TRIPS morphological, syntactic and sometimes semantic (for functional tags) information used for building unknown word entries. This function is only called for content words: N, V, ADJ and ADV."
  ;;  multiple noun tagging by TT : singular, plural, mass, count
  (cond ((and (member 'w::NN penn-tags) (member 'w::NNS penn-tags))
	 '((w::morph (:forms (-S-3P)))
	   (w::mass (? mss w::count w::mass))
	   (w::agr (? agn w::3s w::3p))
	   (w::CASE (? cas w::SUB w::OBJ))
	   (w::sort w::pred)
	   (w::subcat ?subcat)
	   ))
	;; multiple verb tagging by TT  (W::VB W::VBD W::VBG W::VBN W::VBP W::VBZ) -- generate all forms	 
	((and (member 'w::VB penn-tags) (member 'w::VBD penn-tags)(member 'w::VBG penn-tags)(member 'w::VBN penn-tags)
	      (member 'w::VBP penn-tags) (member 'w::VBZ penn-tags))
	 '((w::VFORM (? vf (w::BASE w::PRES w::PAST w::pastpart w::ing)))
	   (w::AGR (? agr (w::1S w::2S w::3S w::1P w::2P w::3P)))))
	((and (member 'w::VB penn-tags) (member 'w::VBP penn-tags))
	 '((w::VFORM (? vf (w::BASE w::PRES)))
	   (w::AGR (? agr (w::1S w::2S w::1P w::2P w::3P)))))
	((and (member 'w::VBD penn-tags) (member 'w::VBN penn-tags))
	 '((w::VFORM (? vf (w::PAST w::PASTPART)))
	   (w::AGR (? agr (w::1S w::2S w::1P w::2P w::3P)))))
	;; individual tags
	((case (base-penn-tag penn-tags)
    ;  EX   (PRO)	; Existential there
    ;  IN   (PREP ADV)	; Preposition or subordinating conjunction
    ;  JJ   (ADJ)	; Adjective
	   (w::JJR  '((w::comparative +)
		      (W::allow-post-n1-subcat +)))	; Adjective comparative
	   (w::JJS  '((w::comparative +)))	; Adjective superlative
	   (w::NN   '((w::morph (:forms (-S-3P))) ; Noun, singular or mass
		      (w::mass (? mss w::count w::mass))
		      (w::agr (? ags w::3s))
		      (w::CASE (? cas w::SUB w::OBJ))
		      (w::sort w::pred)
		      (w::subcat ?subcat)
		      ))
	   (w::NNS   '((w::morph (:forms (-S-3P))) ; Noun plural
		       (w::mass (? msc w::count))
		       (w::agr (? agp w::3p))
		       (w::CASE (? cas w::SUB w::OBJ))
		       (w::sort w::pred)
		       (w::subcat ?subcat)
		       ))
	   (w::NNP  '((w::name +) (w::AGR w::3s)))   ; Proper noun singular
	   (w::NNPS '((w::name +) (w::AGR w::3p)))   ; Proper noun plural
					;  RB   (ADV)	                      ; Adverb
	   (w::RBR   '((w::comparative +)))	      ; Adverb comparative
	   (w::RBS   '((w::comparative +)))	      ; Adverb superlative
					;  RP   (PART)	; Particle
	   (w::VB   '((w::vform (? vf w::base))      ; Verb, base form
		      (w::AGR (? agr w::1S w::2S w::3s w::1P w::2P w::3P))))
	   (w::VBD  '((w::vform (? vf w::past))	; Verb, past tense
		      (w::AGR (? agr w::1S w::2S w::3s w::1P w::2P w::3P))))
	   (w::VBG  '((w::vform (? vf w::ing))	; Verb, gerund or present participle
		      (w::AGR (? agr w::1S w::2S w::3s w::1P w::2P w::3P))))
	   (w::VBN  '((w::vform (? vf w::pastpart w::past))	; Verb, past participle
		      (w::AGR (? agr w::1S w::2S w::3S w::1P w::2P w::3P))))
	   (w::VBP  '((w::vform (? vf w::pres))	; Verb, non-3rd person singular present
		      (w::AGR (? agr w::1S w::2S w::1P w::2P w::3P))))
	   (w::VBZ  '((w::vform (? vf w::pres))	; Verb, 3rd person singular present
		      (w::AGR (? agr w::3S))))
	   (otherwise nil))
	 ))
  )

(defun make-lex-entry-from-replica-entry (replica-entry w pos lf feats score)
  (print-debug "~%MAKE-LEX-ENTRY-FROM-REPLICA-ENTRY: generating senses for entry ~S~%" replica-entry)
  (when replica-entry
    (let* ((sem (get-lf-sem lf :no-defaults nil))
	   (lfform (if (listp w) (make-into-symbol w) w))
           (lf (list :* lf lfform))
           (sense-defs (make-word-sense-definitions replica-entry (lexicon-db-synt-table *lexicon-data*)))
           res)
      (when sense-defs
        (dolist (this-sense sense-defs)
          (let* ((maps (word-sense-definition-mappings this-sense))
                 (roles (word-sense-definition-roles this-sense))
                 (new-entry (make-word-sense-definition
                             :name (gen-id w)
                             :pos pos
                             :lf lf
                             :sem sem
                             :boost-word nil
                             :pref (or score .99)
                             :syntax feats
                             :kr-type nil
                             :mappings maps
                             :roles roles
                             )))
            (push (make-lexicon-entry w new-entry) res)
          )))
       res))
      )

(defun penn-tag-to-trips-semantics (penn-tag)
  "convert penn treebank functional tags into TRIPS semantic information used for building unknown word entries"
  ; check for semantic function tags on prepositions and adverbs
  (case (util::convert-to-package (car penn-tag) :w)
    (w::IN-BNF  '(ont::beneficiary)) ; benefactive
    (w::IN-DIR  '(ont::trajectory ont::direction)) ; direction
    (w::IN-EXT  '(ont::trajectory ont::direction)) ; extent ??
    (w::IN-LOC  '(ont::spatial-loc)) ; locative
    (w::IN-MNR  '(ont::manner)) ; manner
    (w::IN-PRP  '(ont::reason ont::purpose)) ; purpose or reason
    (w::IN-TMP  '(ont::temporal-predicate)) ; temporal
    (w::RB-BNF  '(ont::beneficiary)) ; benefactive
    (w::RB-DIR  '(ont::trajectory ont::direction)) ; direction
    (w::RB-EXT  '(ont::trajectory ont::direction)) ; extent ??
    (w::RB-LOC  '(ont::spatial-loc)) ; locative
    (w::RB-MNR  '(ont::manner)) ; manner
    (w::RB-PRP  '(ont::reason ont::purpose)) ; purpose or reason
    (w::RB-TMP  '(ont::temporal-predicate)) ; temporal	   
    (otherwise nil))
  )

(defun reconcile-penntags-and-pos (penntags pos)
  "find the corresponding penntag for the pos"
  (case pos
    (w::n (list (or (find 'w::NN penntags) (find 'w::NNS penntags))))
    (w::name (list (or (find 'w::NNP penntags) (find 'w::NNPS penntags))))
    (w::adj (list (or (find 'w::jjr penntags) (find 'w::jjs penntags) (find 'w::jj penntags) )))
    (w::adv (list (or (find 'w::rb penntags) (find 'w::rbr penntags) (find 'w::rbs penntags))))
    (w::v (intersection penntags '(w::VB w::VBD w::vbg w::vbp w::vbz w::md)))
    (otherwise penntags))
  )

(defun make-unknown-name-entry (word score penn-tag ont-types domain-info)
  (print-debug "making NAME sense for word ~S with type ~S~%" word ont-types)
  (let* ((penn-tag (reconcile-penntags-and-pos (list (base-penn-tag penn-tag)) 'w::name))
	 (syntax (or (penn-tag-to-trips-syntax penn-tag) '((w::name +) (w::AGR w::3s))))
	 )
  (when word
    (when (null ont-types)
      (setq ont-types '(ont::referential-sem))
      (setq score (if domain-info score .95 )))
     (remove-if #'null
	       (mapcar (lambda (ont-type)
			 (if (get-lf-sem ont-type)
			     (make-lexicon-entry word (make-word-sense-definition
						       :name (gen-id word)
						       :pos 'w::name
						       :lf (if (listp word) (list :* ont-type (make-into-symbol word))
							       (list :* ont-type word))
						       :sem (get-lf-sem ont-type :no-defaults nil)
						       :pref (or score .99)
						       :syntax syntax
						       :kr-type domain-info
						       :specialized (if domain-info t nil)
						       ))))
			 ont-types))
     ))
  )


(defun adjust-score-if-participle-adj (word score trips-sense-list)
"reduce score if this adjective has the same form as a verb participle, e.g. placed, made
 to reduce competition with derivational grammar rules for e.g. reduced relative clauses"
   (declare (ignore word))
 (when  (find 'w::v trips-sense-list :key #'second)
   (setq score .96)) ; only use adj if verb senses fail
  score)

(defun make-feature-value (lf feature)
  "try to extratc feature values from words we know with same LF"
  (if (eq lf 'ont::REFERENTIAL-SEM)
      '(? ms)
      (let ((values 
	     (feature-values-from-lf lf feature)))
    	(if (> (length values) 1) 
	    (append '(? ms) values)
	    (car values)
	    ))))

(defun lookup-type-in-lexicon (w)
  "this looks up a word in the lexicon and retruns the ONT type of the first entry"
  (let* ((entries (remove-if #'(lambda (x)
				(eq (second (fourth x)) 'w::word))
			     (get-word-def w nil)))
	 (c (cddr (fourth (first entries))))
	 (lf (parser::get-fvalue c 'w::lf)))
    (second lf)))
    
(defun make-unknown-word-entry (word pos score feats wid lflist trips-sense-list penn-tag tagged-ont-types domain-info)
  "make an underspecified lexical entry for the unknown word"
    (declare (ignore tagged-ont-types))
  (print-debug  "~%MAKE-UNKNOWN-WORD-ENTRY: generating word entry for ~S with ~S and ~S ~%" word pos lflist)
  (let* ((lfform (if (listp word) (make-into-symbol word) word))
	(lf (car lflist))
	(pos (if (listp pos) (car pos) pos))
	(penn-tag (reconcile-penntags-and-pos penn-tag pos))
	(syntax-from-penn-tag (penn-tag-to-trips-syntax penn-tag))
	 syntax sem maps roles wagr wmass res replica-entry)
     
    ;; assign underspecified syntactic and semantic features depending on part of speech
     (case pos
      (w::n
       (setq wagr (or (get-feature-values feats 'w::agr) 
		      (if (member 'w::nns penn-tag)
			  '(? ag w::3p))
		      (if (s-form word)
			  '(? ag w::3s w::3p)
			  '(? ag w::3s))))
       
       (setq wmass (or (get-feature-values feats 'w::mass) 
		       (make-feature-value lf 'w::mass)
		       '(? ms w::count w::mass)
		       ))
       
       (setq syntax (or syntax-from-penn-tag
			(append feats `((w::morph (:forms (-S-3P))) 
					(w::mass ,wmass)
					(w::CASE (? cas w::SUB w::OBJ))
					(w::agr ,wagr)
					(w::sort w::pred)
					(w::subcat ?subcat)
					))))
       
       (setq sem (get-lf-sem lf :no-defaults nil))

       (when (om::is-sublf lf 'ont::situation-root)
	 (setq syntax (append syntax '((w::nomobjpreps (? nomobjprep w::of)) (w::nomsubjpreps (? nomsubjprep w::by))))))
       ;;(setq lf (list :* lf lfform))     
       ;; make untyped sem so it will match any features on the role restrictions
;       (setq sem `,(make-untyped-sem nil) )
       (setq res (create-entry-based-on-entries-of-the-same-type word wid lf lfform sem syntax pos trips-sense-list domain-info score))
        )
      (w::name
       (setq res (make-unknown-name-entry word score penn-tag lflist domain-info))
       )
      (w::adj
       (let ((pertainyms (find-arg-in-act (car domain-info) :pertainyms))
	     (compar (if (member 'w::jjr penn-tag) 'compar)))
	 ;;(format t "~%Pertainymm is ~S from domain info ~S" pertainyms domain-info)
	 (if pertainyms
	     (let* ((pert (car pertainyms))
		    (sense-info (find-arg pert :sense-info))
		    (pert-lf-form (parser::tokenize (second pert)))
		    (pert-lf (or (car (find-arg (cadr sense-info) :ont-types))
				 (lookup-type-in-lexicon pert-lf-form)
				 'ont::referential-sem))
		    (pert-domain-info  (find-arg (cadr sense-info) :domain-specific-info)))
	       (format t "pertainym sense info is: SENSE-INFO ~S LF: ~S LF-FORM: ~S domain info: ~S" sense-info pert-lf pert-lf-form pert-domain-info)
	     ;;(setq feats (append feats (list (list 'w::pertainym (list (list :* pert-lf pert-lf-form) pert-sem)))))
	     (setq feats (append feats (list (list 'w::pertainym (list :* pert-lf (car pert-lf-form)))
					     (list 'w::pert-domain-info pert-domain-info))))
	     (if (member lf '(ONT::MODIFIER ONT::REFERENTIAL-SEM))
		 (setq lf '(:* ONT::ASSOC-WITH lf-form)))
	     (setq sem (get-lf-sem 'ont::ASSOC-WITH :no-defaults nil))
	     (setq domain-info nil))
	     ;;  no pertainym, so do normal computation off the LF
	     (progn
	       (setq sem (get-lf-sem lf :no-defaults nil))
	       (setq lf (change-to-compar-if-needed lf penn-tag))  ;; we keep the old SEM even if we cahnge theLF
	       ))
	 ;;(setq lf (list :* lf lfform))
	 (setq syntax (append feats `((w::gap ?gap)
				      (w::roles ?roles)
				      (w::arg ?arg)
				      (w::comp-op ?op)
					; (w::subcat ?subcat)
				      (W::ARGUMENT (% W::NP
						      (W::GAP ?argap)
						      (W::LF ?arglf)
						      (W::SORT ?argsort)
						      (W::LEX ?arglex)
						      (W::VAR ?argvar)
						      (W::AGR ?argagr)
						      (W::CASE ?argcase)
						      (W::SEM ?argsem)))
					;(w::argument ?argument)
				      (w::argument-map ont::figure)
				      (w::atype (? atype w::central w::postpositive))
				      (w::sort w::pred)
				      (w::allow-deleted-comp +)
				      (w::post-subcat ?postsub)
				      (w::set-modifier ?setmod)
				      (w::filled ?filled)
				      )
			      (if compar 
				  '((W::comparative +)
				    (W::template lxm::compar-templ)
				    (W::subcat-map ont::ground)
				    (W::subcat (% W::PP
						(W::GAP ?subcatgap)
						(W::LF ?subcatlf)
						(W::SORT ?subcatsort)
						(W::LEX ?subcatlex)
						(W::VAR ?subcatvar)
						(W::AGR ?subcatagr)
						(W::CASE ?subcatcase)
						(W::PTYPE W::THAN)
						(W::SEM ?subcatsem))))
				  '((w::subcat-map ?submap)))))
       (when syntax-from-penn-tag (setq syntax (append syntax syntax-from-penn-tag)))
       (setq score (adjust-score-if-participle-adj lfform score trips-sense-list))
       ))
      (w::adv
       (setq replica-entry (make-replica-entry word lf pos '(ont::modifier)))
;       (setq sem (get-lf-sem lf :no-defaults nil ))
;       (setq lf (list :* lf lfform))
       (setq syntax (append feats `((w::gap ?gap)
				    (w::ATYPE (? ATYPE w::PRE w::POST w::PRE-VP))
				    (w::allow-deleted-comp +)
				    (w::sort w::pred)
				    )))
       (when syntax-from-penn-tag (setq syntax (append syntax syntax-from-penn-tag)))
       (setq res (make-lex-entry-from-replica-entry replica-entry word pos lf syntax nil))
       )
      (w::v
       (setq sem (get-lf-sem lf :no-defaults nil))
       (setq syntax (or syntax-from-penn-tag
			    `( (w::VFORM ,(cond ((ing-form word)
						 '(? vf (W::base W::pres w::ing)))
						((ed-en-form word)
						 '(? vf (w::BASE w::PRES w::PAST w::pastpart)))
						(t '(? vf (w::BASE w::PRES)))))
			       (w::AGR ,(if (or (s-form word) (ed-en-form word))
		       		   '(? agr (w::1S w::2S w::3S w::1P w::2P w::3P))
					   '(? agr (w::1S w::2S w::1P w::2P w::3P))))
			       (w::nomobjpreps (? nomobjprep w::of))
			       (w::nomsubjpreps (? nomsubjprep w::by)))))
       ;; now we look at existing lexicon entries to generate entries for the new word
       (setq res (create-entry-based-on-entries-of-the-same-type word wid lf lfform sem syntax pos trips-sense-list domain-info score))
       )
      (otherwise nil))
     ;; if we failed to make an entry above, we do a generic one here
     
     (when (not res) ;; (find pos '(w::v w::adv w::name w::n))) 
       (print-debug "making word sense for word ~S with pos ~S lf ~S sem ~S domain-info ~S~%" word pos lf sem domain-info)
       (let* ((entry (make-word-sense-definition
		      :name wid
		      :pos pos
		      :lf (list ':* lf lfform)
		      :sem sem ;;(update-sem-with-domain-info sem domain-info)
		      :boost-word nil
		      :pref (or score (if domain-info .99 .97))
		      :syntax syntax
		      :kr-type domain-info
		      :specialized (if domain-info t nil)
		      :mappings maps
		   :roles roles
                   )))
;	(if (and syntax-from-penn-tag (or (eq pos 'w::adj) (eq pos 'w::adv)))
;	    (setq res (make-comparative entry syntax))
	  (push (make-lexicon-entry word entry) res);)
	  (print-debug "entry is ~S~%" entry)
	))
    res)
  )

(defun change-to-compar-if-needed (lf penn-tags)
  "checks for comparative of superative adjectives"
  (if (consp lf)
      (if (member 'w::jjr penn-tags)
	  (list ':* 'ONT::MORE-VAL (third lf))
	  lf)
      (if (member 'w::jjr penn-tags)
	  'ONT::MORE-VAL
	  lf)))

(defun create-entry-based-on-entries-of-the-same-type (word wid lf lfform sem syntax pos trips-sense-list domain-info score)
  (let ((this-entry (make-replica-entry word lf pos trips-sense-list))
	sense-defs maps res roles)
	 (when this-entry
	   (print-debug "generating senses for entry ~S~%" this-entry)
	   (setq sense-defs (make-word-sense-definitions this-entry (lexicon-db-synt-table *lexicon-data*)))
	   (when sense-defs
	     (dolist (this-sense sense-defs)
	       (setq maps (word-sense-definition-mappings this-sense))
	       (setq roles (word-sense-definition-roles this-sense))
	       (if (consistent-features (word-sense-definition-syntax this-sense) syntax)
		   (let* ((new-entry (make-word-sense-definition
				      :name wid
				      :pos pos
				      :lf `(:* ,lf ,lfform)
				      :sem sem
				      :boost-word nil
				      :pref (or score .99)
				      :syntax (combine-syntax-features (word-sense-definition-syntax this-sense) syntax)
				      :kr-type domain-info
				      :specialized (if domain-info t nil)
				      :mappings maps
				      :roles roles
				      )))
		     (push (make-lexicon-entry word new-entry) res)
		     )))
	     res
	     ))))

(defun combine-syntax-features (newfeats defaultfeats)
  "remove any default feats defines in newfeats"
  (let ((defined-feats (mapcar #'car newfeats)))
    (append newfeats (remove-if #'(lambda (x) (member (car x) defined-feats))
				defaultfeats))))

(defun consistent-features (feats1 feats2)
  (if feats1
      (let* ((feat (caar feats1))
	     (val (cadar feats1))
	     (feats2val (cadr (assoc feat feats2))))
	(if (or (member feat '(W::morph))  ;; some features are exempted
		(null feats2val)
		(equal feats2val val))
	    (consistent-features (cdr feats1) feats2)
	    ))
      T))
	

(defun ing-form (word)
  "T if word ends in ING"
  (let* ((chars (reverse (coerce (if (listp word)
				     (symbol-name (car (last word)))
				     (symbol-name word)) 'list))))
    (and (eq #\G (car chars)) (eq #\N (cadr chars)) (eq #\I (caddr chars)))))

(defun ed-en-form (word)
  "T if word ends in EN or EN"
  (let* ((chars (reverse (coerce (if (listp word)
				     (symbol-name (car (last word)))
				     (symbol-name word)) 'list))))
    (and (or (eq #\N (car chars)) (eq #\D (car chars))) (eq #\E (cadr chars)))))

(defun s-form (word)
  "T if word ends in EN or EN"
  (let* ((chars (reverse (coerce (if (listp word)
				     (if (symbolp (car (last word)))
					 (symbol-name (car (last word))))
				     (symbol-name word)) 'list))))
    (eq #\S (car chars))))
    
    

(defun update-sem-with-domain-info (sem info)
  (if (null info)
      sem
      (make-feature-list :type (feature-list-type sem)
			 :features (replace-arg (feature-list-features sem) 'F::KR-type info))))

(defun get-wf-entries (wf-result)
  (if (found-wordp wf-result)
      (get-keyword-arg wf-result :entries)))

(defun get-first-wf-entry (wf-result)
    (if (found-wordp wf-result)
	(car (third wf-result))
      nil)
  )

;; returns the wf-word as a symbol
(defun get-wf-word (wf-entry)
  (read-from-string (second (member :word wf-entry)))
  )

;; returns the wf word in string form
(defun get-wf-wordstring (wf-entry)
  (second (member :word wf-entry))
  )

(defun get-wf-score (wf-entry)
  (second (member :score wf-entry))
  )

(defun get-wf-feats (wf-entry)
  (second (member :features wf-entry))
  )

(defun get-wf-constit (wf-entry)
  (second (second (member :constit wf-entry)))
  )

;; only return ONT::ANY-SEM if there is no other option
(defun get-wf-lfs (wf-entry)
  (or (remove-if #'(lambda (x) (eq x 'ont::ANY-SEM))  (second (member :lfs wf-entry)))
      '(ont::ANY-SEM))
  )

(defun get-wf-like (wf-entry)
  (second (member :like wf-entry))
  )

(defun get-wf-domain-info (wf-entry)
  (second (member :domain-info wf-entry))
  )

(defun is-instance (wf-entry)
  "checking for proper names in the synset list to exclude them -- this just checks if the first letter of the word is capitalized; a better method would be to detect the new INSTANCE feature in the WN entry, but right now the wordnet code is not picking up on it" 
  (let* ((wn-word (get-wf-wordstring wf-entry))
	 (res nil))
    (print-debug "wn-word is ~S wf-entry is ~S ~%" wn-word wf-entry)
    (when wn-word
      (let* ((wordstring (coerce wn-word 'list))
	     (first-letter (car wordstring)))
	(setq res
	      (member first-letter '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\0 #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)))
	))
    res)
  )

(defun get-trips-synset-for-wordnet-word (w &key (pos nil))
  "returns a list of lex-entries for words in trips and in the wordnet synset of the (string) word w"
  (mapcan
    (lambda (pos-word)
      (let ((pos (first pos-word))
            (word (second pos-word)))
	(remove-if-not 
	  (lambda (entry)
	    (eql pos (first (lex-entry-description entry)))
	    )
	  (retrieve-from-lex word)
	  )))
    (wf::get-synset-for-word w :pos pos)
    ))

(defun is-penn-closed-class (penn-tags)
  "words with these trips part of speech tags are excluded from external resource lookup"
  (if (not (listp penn-tags)) (setq penn-tags (list penn-tags)))
  (or (member 'w::CC penn-tags)     ; conjunction
      (member 'w::CD penn-tags)     ; cardinal number
      (member 'w::DT penn-tags)     ; determiner
      (member 'w::EX penn-tags)     ; existential there
      (member 'w::POS penn-tags)    ; possessive ending
      (member 'w::PP penn-tags)     ; personal pronoun
      (member 'w::PP$ penn-tags)    ; possessive pronoun
      (member 'w::TO penn-tags)     ; infinitival to
      (member 'w::PRP$ penn-tags)   ; ??
      (member 'w::PRP penn-tags)    ; 
      (member 'w::RP penn-tags)     ; particle
      (member 'w::WDT penn-tags)    ; wh-determiner
      (member 'w::WP penn-tags)     ; wh-pronoun
      (member 'w::WP$ penn-tags)    ; possessive wh-pronoun
      (member 'w::wrb penn-tags)    ; wh-adverb
       )
  )

(defun is-penn-verb-tag (penn-tags)
  (if (not (listp penn-tags)) (setq penn-tags (list penn-tags)))
  (or (member 'w::VB penn-tags)
      (member 'w::VBD penn-tags)
      (member 'w::VBG penn-tags)
      (member 'w::VBN penn-tags)
      (member 'w::VBP penn-tags)
      (member 'w::VBZ penn-tags)
      (member 'w::MD penn-tags)
      )
  )

(defun is-penn-proper-noun (penn-tags)
  (if (not (listp penn-tags)) (setq penn-tags (list penn-tags)))
  (or (member 'w::NNP penn-tags)
      (member 'w::NNPS penn-tags))
  )

(defun exclude-from-underspecification (penn-tags)
  (or 
   (is-penn-verb-tag (base-penn-tag penn-tags))
   (is-penn-closed-class (base-penn-tag penn-tags))
   (is-penn-proper-noun (base-penn-tag penn-tags))
   )
  )

;; common words / stop words that shouldn't be looked up in WF
(defun is-frequent-word (w)  
  (or (find w '(w::THE w::A w::AN)) ;; articles
      ;; prepositions and conjunctions
      (find w '(w::of w::to w::and w::in w::out w::for w::on w::with w::as w::at w::from w::or w::by w::but w::so w::not w::such w::up w::down w::over w::under w::around w::far w::near w::also w::too w::if w::about w::through w::just w::only))
      ;; pronouns
      (find w '(w::THIS w::THAT w::I w::YOU w::HE w::SHE w::IT w::WE w::THEY w::ME w::HER w::HIM w::US w::THEM w::YOURS w::HIS w::HERS w::ITS w::OURS w::THEIRS w::WHICH w::THESE w::THOSE w::own))
      ;; verbs
      (find w '(w::AM w::ARE w::IS w::WAS w::WERE w::BE w::BEEN w::BEING   ;; be
			w::HAVE w::HAS w::HAD w::HAVING ;; have			    
			w::SHALL w::SHOULD w::WOULD w::SHALL w::COULD w::MAY w::OUGHT w::CANNOT ;; aux
			w::DO w::DID w::DOING ;;do
;			w::make w::makes w::making w::made ;; light verbs
;			w::let w::like w::liked
			w::^ve w::^s w::^d w::ca w::sha w::^ll w::wo w::^m w::^re
;			w::get w::gets w::getting w::got w::takes w::taking w::took
;			w::appear w::appeared w::appearing w::seem w::seemed w::seeming			   			        w::went w::gone w::said w::says w::tell w::told w::think w::thought
			))
      (find w '(w::first w::second w::third w::fourth w::fifth w::sixth w::seventh w::eighth w::ninth w::tenth w::eleventh w::twelfth w::thirteenth w::fourteenth w::fifteenth w::sixteenth w::seventeenth w::eighteenth w::nineteenth w::twentieth))
      (find w '(w::thing w::things))

      (find w '(w::new w::old w::really w::very w::real w::again w::easy w::hard w::early w::late w::great w::good w::bad))
      (find w '(w::here w::there w::everywhere w::anywhere w::somewhere w::someplace w::elsewhere w::anyplace))
      (find w '(w::one w::someone w::somebody w::noone w::nobody w::anyone w::anybody w::everyone w::everybody w::other))
      (find w '(w::when w::where w::how w::what w::why w::who w::while))
      (find w '(w::all w::each w::some w::many w::any w::few w::never w::always w::alot w::usually w::more w::much))
      (find w '(w::myself w::yourself w::himself w::herself w::itself w::ourself w::ourselves w::yourselves w::themself w::themselves w::self w::whoever w::whomever w::whatever w::whenever w::however w::whose w::whom))
      (find w '(w::today w::yesterday w::tomorrow w::tonight w::tonite w::now w::then w::sometime w::sometimes w::anytime w::yet w::until))
      (find w '(w::yes w::no w::maybe w::probably w::certainly))
  ))


