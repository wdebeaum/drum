; This package provides the code to map WordNet synsets into TRIPS entries
; The entries consist of the following:
; - part of speech (:constit-type pos)
; - synonyms (:like list-of-synonyms)
; - best guess of a logical form match (:lf logical-form)
; - verb frame
; - features (coming later)

; TODO:
; - features
;   WordNet entries are case-sensitive -- perhaps we should convert them
;   all to lowercase?  This can be a problem if there is a conflict,
;   as that may clobber entries.

(in-package :wordfinder)

; This is the master Wordnet Manager which helps us perform all operations
; in this task (except for Wordnet->TRIPS hierarchy conversion -- that's not
; Wordnet's job and we do that later with convert-hierarchy).
;; gf: Moved initialization to setup-wordnet so we can pass in the path
(defvar wm nil)
(defvar *prefer-core-wordnet-senses* nil)
(defvar *tag-pertainyms* t)

(defun setup-wordnet (basepath)
  "Must be called before the Wordnet interface can be used.  Calling this will load the Wordnet indices and exception files."
;;  (setq wm (make-instance 'wordnet::wordnet-manager :basepath basepath))
  (setq wm (make-instance 'wordnet-manager :basepath basepath))
;;  (wordnet::load-indices wm)
;;  (wordnet::load-exception-files wm))
   (load-indices wm)
   (load-exception-files wm)
   (load-core-senses wm)
   ;; TODO: move this to OM to avoid the lf-ontology check
   (if om::*lf-ontology* (make-synset-to-ont-type-table)) ; load the hash table for synset to ont-type mappings 
   )

(defun is-instance (synset)
  "Does synset have an instance link? I.e. is it a name that we want to discard unless we're looking for the W::name part of speech?"
  (consp (get-pointers-by-relationship wm synset "@i")))

(defun names-excluded (poslist)
  (not (find 'w::name poslist))
  )

(defun get-wf-lf (wf-entry)
  (car (second (member :lfs wf-entry)))
  )

(defun trips-pos-for-wn-sense-key (wn-sense-key)
  (let ((synset (get-synset-from-sense-key wm wn-sense-key)))
    (when synset
      (convert-wordnet-pos-to-trips (get-pos-string synset)))				
  ))

(defvar *has-trips-duplicate* nil) ; set to T during processing if a sense extracted from WN has a duplicate sense (i.e., same ont-type and pos) defined in TRIPS

(defun process-synsets (word synsets &key senselimit poslist penntags trips-sense-list wn-sense-keys)
  "Each sense returned is associated with a score. Scores begin at .99 and are decremented as we step through the list of senses as a rough means of ranking senses by frequency. WordNet lists senses in order of frequency based on their corpora statistics. The actual sense frequency score is not currently part of the WN download, so we rely on the synset ordering for now to create a relative ranking, which determines which senses are preferred by the parser."
    (declare (ignore wn-sense-keys))
  (let ((ncount 0) (vcount 0) (adjcount 0) (advcount 0)
	 (nscore 1) (vscore 1) (adjscore 1) (advscore 1) ; we decf before the first sense, so these will be 0.99
	 senselist n-lftypes v-lftypes adj-lftypes adv-lftypes)
        ;; get as many as senselimit trips senses for each POS given synsets
	;; with special handling for names, and verbs mapping to ONT::situation-root
	(dolist (synset synsets)
	  (let ((this-pos (convert-wordnet-pos-to-trips (get-pos-string synset))))
	    (when (find this-pos poslist)
	      (case this-pos
		(w::n (when (< ncount senselimit)
		;; Wordnet includes proper names w/ nouns as capitalized & marked as instance. So there is a condition here that depends on whether the system is set to include WordNet names or not
			(cond
			 ((names-excluded poslist)
			  (when (not (is-instance synset))
			    (push (cons this-pos (convert-synset synset word nscore penntags trips-sense-list)) senselist)
			    (incf ncount))
			  )
			 (t ;; otherwise we include any noun in the synset
			  (let* ((this-sense (convert-synset synset word nscore penntags trips-sense-list))
			       (this-lftype (get-wf-lf this-sense))
			       )
			  (when (and this-sense (not (member this-lftype n-lftypes)))
			    (push (cons this-pos this-sense) senselist)
			    (incf ncount)
			    (pushnew this-lftype n-lftypes))
			  )))))
		(w::v (when (< vcount senselimit) ;; stop inflating v senses for now to help with ambiguity
					;(< vcount (+ senselimit 2)) ;; inflate sense limit because abstract senses (ONT::situation-root) are filtered out downstream
			(let* ((this-sense (convert-synset synset word vscore penntags trips-sense-list))
			       (this-lftype (get-wf-lf this-sense)))
			  (when (and this-sense (not (member this-lftype v-lftypes)))
		
			    ;; record ont::situation-root if it's the only mapping 
			    (when (not (and (> vcount 0) (equal this-lftype 'ont::situation-root)))
			      (push (cons this-pos this-sense) senselist)
			      (incf vcount)
			      (pushnew this-lftype v-lftypes)))
			  ;; if there are other senses we can discard ont::situation-root
			  ;; if this verb has at least one sense that is not ont::situation-root, it should have no ont::situation-root senses
			  ;; but we need it as a default if there are no other verb senses
			  (when (and (> vcount 1) (member 'ont::situation-root v-lftypes))
			    (setq senselist
				  (delete 'ont::situation-root senselist
				      :key (lambda (sense) (get-wf-lf (cdr sense)))))
			    (decf vcount)
			    (setq v-lftypes (delete 'ont::situation-root v-lftypes))
			    )
			  )))
		(w::adj  (when (< adjcount senselimit)
			   (let* ((this-sense (convert-synset synset word adjscore penntags trips-sense-list))
				  (this-lftype (get-wf-lf this-sense))
				  )
			     (when (and this-sense (not (member this-lftype adj-lftypes)))
			       (push (cons this-pos this-sense) senselist)
			       (incf adjcount)
			       (pushnew this-lftype adj-lftypes)))))
		(w::adv (when (< advcount senselimit)
			   (let* ((this-sense (convert-synset synset word advscore penntags trips-sense-list))
				  (this-lftype (get-wf-lf this-sense))
				  )
			     (when (and this-sense (not (member this-lftype adv-lftypes)))
			       (push (cons this-pos this-sense) senselist)
			       (incf advcount)
			       (pushnew this-lftype adv-lftypes)))))
		(otherwise (print-debug "invalid part of speech encountered ~S~%" this-pos)))
	      )))
	;; undo the reversing effect of using push to add to the list
	(setf senselist (nreverse senselist))
	;; push fallback TRIPS senses to end of senselist, regardless of
	;; WordNet sense number
	(multiple-value-bind (fallback mapped)
	    ;; first split the list into two
	    (split-list
	        (lambda (sense)
		  (member (get-wf-lf (cdr sense)) '(
		      ;; the senses that convert-hierarchy (called by
		      ;; convert-synset) uses when there is no explicit mapping
		      ;; (we assume there will never be an explicit mapping to
		      ;; these types)
		      ont::any-sem		; noun
		      ont::situation-root	; verb
		      ont::modifier		; adjective
		      ont::predicate		; adverb
		      )))
		senselist)
	  ;; then re-concatenate them with fallback senses last
	  (setf senselist (nconc mapped fallback)))
	;; assign scores based on reordered senselist, and uncons to get only
	;; senses and not POSs
	(setf senselist
	      (loop for (pos . sense) in senselist
	      	    for new-score =
		      (ecase pos
		        (w::n   (decf   nscore 0.01))
			(w::v   (decf   nscore 0.01))
			(w::adj (decf adjscore 0.01))
			(w::adv (decf advscore 0.01))
			)
		    when sense ; get rid of nil senses for duplicate or subsumed
		    collect (replace-arg-in-act sense :score new-score)))
	senselist)
	)

(defun core-wordnet-senses (synsets)
  "Given a list of synsets, return only those that are core"
  (let ((core-senses))
    (dolist (synset synsets)
      (dolist (sense-key (sense-keys-for-synset synset))
	(let ((core-sense (is-core-wordnet-sense sense-key)))
	  (when core-sense (pushnew sense-key core-senses :test #'equalp)))))
    core-senses)
  )

(defun is-base-form (penntags)
  (remove-if #'null (mapcar (lambda (tag)
			      (find tag '(NN VB IN JJ RB CONJ NNP)) 
			      )
			    penntags))
  )

(defun convert-wordnet-entries (word &key senselimit poslist penntags trips-sense-list wn-sense-keys)
  "Given a word (string), returns wn senses for this word to send back to the parser.
  word: the word to lookup
  senselimit: the number of senses to retrieve for each part of speech -- set in the LXM.
  poslist: parts of speech for which to retrieve senses -- determined in LXM through a combination of TT info and TRIPS senses
  trips-sense-list: optional ont-types to filter choices -- passed in from TextTagger
  wn-sense-keys: optional wordnet sense keys -- passed in from TextTagger. When we have the sense key, this becomes a specific lookup by sense key
  "
  (let* ((word (substitute #\_ #\Space (string-downcase (string word)))) ; wordnet uses _ to separate collocations
	 lemma-synsets stemmed-synsets synsets results)
    (initialize-word-inflections)
    (cond (;; if we have wordnet sense keys, use those to get the word senses
	   (not (null wn-sense-keys))
	   (dolist (sense-key wn-sense-keys)
	     (unless (stoplist-p sense-key)
	       (let ((new-synset (get-synset-from-sense-key wm sense-key)))
	         (if new-synset (pushnew new-synset synsets :test #'equalp)))))
	   (setq synsets (reverse synsets))) ;; preserve wn sense ordering
	  (t  ;; otherwise look up the word in WordNet
	   (if (or (is-base-form penntags) (null penntags))
	       ;; try without stemming
	       (setq lemma-synsets (get-all-synsets wm word :use-stoplist t))
	     (setq stemmed-synsets
		   (get-all-synsets wm word :use-morphy t :use-stoplist t)))
	     ;; if initial lookup returns no senses, try stemming iff a duplicate trips sense was not surpressed
	     (if (and (not lemma-synsets) (not *has-trips-duplicate*) (not stemmed-synsets))
		 (setq stemmed-synsets
		       (get-all-synsets wm word :use-morphy t :use-stoplist t)))
	     (setq synsets (or stemmed-synsets lemma-synsets))
	     (print-debug "lemma-synsets are ~S~% stemmed-synsets are ~S~%" lemma-synsets stemmed-synsets)
	     (setq *has-trips-duplicate* nil)
	     ))
         (if *prefer-core-wordnet-senses* (setq synsets (or (core-wordnet-senses synsets) synsets)))
	 (print-debug "WordNet synsets retrieved are ~S~% poslist ~S penntags ~S trips-senses ~S wn-senses ~S~%" synsets poslist penntags trips-sense-list wn-sense-keys)
	 (setq results (process-synsets word synsets :senselimit senselimit :poslist poslist :penntags penntags :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys))
	 ;; if there are no senses after filtering by pos and we haven't tried stemming yet, try it
	 (when (and (not results) (not stemmed-synsets))
	   (print-debug "retrieving synsets for stemmed version of word ~S ~S~%" word poslist)
	   (setq synsets
		 (get-all-synsets wm word :use-morphy t :use-stoplist t))
	   (print-debug "WordNet synsets retrieved are ~S~% poslist is ~S~%" synsets poslist)
	   (if *prefer-core-wordnet-senses* (setq synsets (or (core-wordnet-senses synsets) synsets)))
	   (setq results (process-synsets word synsets :senselimit senselimit :poslist poslist :penntags penntags :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys))
	   (print-debug "returning senses for stemmed word  ~S~%" results)
	   )
	 results)
  )

; no longer used -- calling lxm functions	      
;(defun is-duplicate-or-subsumed-sense (pos mapped-ont-type trips-sense-list)
;  (remove-if #'null
;    (mapcar (lambda (trips-sense-pair)
;	      (or (if (member pos trips-sense-pair) (member mapped-ont-type trips-sense-pair))
;		  (if (member pos trips-sense-pair) (om::is-sublf mapped-ont-type (car trips-sense-pair))))
;	      )
;	    trips-sense-list))
;  )

(defun is-substance (synset)
" check if this synset is in the nouns.substance file, #27; another test would be to check for substance%1:03:00:: in the hierarchy"
   (print-debug "~%synset is ~S lex-filenum is ~S~%" synset (get-lex-filenum synset))
   (eql (get-lex-filenum synset) 27)
  )

(defun get-syntactic-features (constit-type penntags synset lex-filenum)
  "use information from PTB tags to fill in syntactic information such as agreement"
    (declare (ignore lex-filenum)) ; for now?
  (let ((feats))
;    (when (equalp constit-type 'w::N)
    (case constit-type
      (w::N
       ;; mass/count
       (if (is-substance synset) (setq feats '((w::mass w::mass))) (setq feats '((w::mass w::count))))
       ;; agreement 
       ;; we typically rely on PTB tags for agreement; but if they are not available, we check the N suffixes
       (cond ((and (member 'w::NN penntags) (member 'w::NNS penntags))
	      (setq feats (append feats '((w::agr (? lxm::ag w::3s w::3p))))))
	     ((member 'w::NNS penntags) (setq feats (append feats '((w::agr w::3p)))))
	     ((member 'w::NN penntags)  (setq feats (append feats '((w::agr w::3s)))))
	     (t ; last resort -- check if the noun had any common plural suffixes
	      (if *this-noun-inflection* (setq feats (append feats '((w::agr w::3p))))  
		  (setq feats (append feats '((w::agr w::3s))))))
	     )
       (print-debug "noun feats are ~S~%" feats)
      )
      (otherwise nil))
    feats))

(defun get-adj-synonyms (ss)
  (mapcar #'remove-syntactic-marker-from-adj (get-synonyms ss)))

(defun get-domain-info (synset word)
  "Get the domain info to add to the entry for a given WordNet sense, if any.
   Right now this just looks up the noun pertainyms of adjectives, and sends
   them to TextTagger for further tagging."
  (when (and *tag-pertainyms*
  	     dfc::*component* ; we can call TextTagger if we're a component
             user::*use-texttagger* ; ... and this system is using it otherwise
	     ;; ... but it wasn't TextTagger who called us (via LXM)
	     (not (eq lxm::*request-is-from* 'lxm::texttagger))
	     ; (FIXME paranoia)
	     (let ((pp (find-package :parser)))
	       (and pp (eq (eval (intern "*IN-SYSTEM*" pp)) :drum)))
  	     ;; only adjectives have pertainym links
             (member (get-ss-type synset) '("a" "s") :test #'string=))
    (let* (;; word might've been morphed to match synset, so get all the words
	   ;; that it could match...
	   (words (cons (substitute #\Space #\_ word)
			(second (assoc "adj" (run-morphy wm word)
				       :test #'string=))))
	   ;; ... and use them to find the number of the word in the synset
	   (synonyms (get-adj-synonyms synset))
	   (word-number (1+ (or
			      (position-if
				  (lambda (x)
				    (member x words :test #'string-equal))
				  synonyms
				  )
			      ;; if we still failed to find the word number,
			      ;; pick one that won't match any pointers, and
			      ;; print a warning
			      (progn
				(warn "can't find word ~s in synset ~s" word synset)
				-2))))
	   (unmorphed-word
	     (when (> word-number 0)
	       (nth (- word-number 1) synonyms)))
           (pertainym-pairs (get-pointers-by-relationship wm synset "\\"))
	   (pertainym-strings
             (loop for (ptr target-ss) in pertainym-pairs
		   for source-target = (slot-value ptr 'source-target)
		   for (source-word-number target-word-number) =
		     (multiple-value-list (truncate source-target 256))
		   when (or (= 0 source-word-number)
		            (= word-number source-word-number))
		   append (if (= 0 target-word-number)
		            ;; no word number, get all the synonyms
		            (get-adj-synonyms target-ss)
			    ;; get just the one pointed to by the word number
			    (let* ((words (slot-value target-ss 'words))
			           (target-word
				       (nth (- target-word-number 1) words))
				   (target-word-str
				     (slot-value target-word 'word)))
			      (list (substitute #\Space #\_ target-word-str)))
			    )
		   ))
	   )
      ;; don't use pertainyms that are the same word, to avoid infinite
      ;; recursion
      (setf pertainym-strings
	    (remove unmorphed-word pertainym-strings :test #'string-equal))
      (when pertainym-strings
        (let* ((pertainym-string (format nil "~{~a~^ ~}" pertainym-strings))
	       (tt-reply
	         (let ((*package* (find-package :parser))) ; HACK
		   (dfc::send-and-wait
		     (util::convert-to-package
		       `(request
			  :receiver TextTagger
			  :content (tag :text ,pertainym-string
					;; don't reenter WF
					:use-wordfinder nil
					))))))
	       (pertainym-infos
		 ;; extract just the tags that are for pertainyms, and add
		 ;; :wn-sense-keys if necessary
		 (loop for msg in tt-reply
		       for msg-type = (intern (symbol-name (first msg)) :wf)
		       for lex = (second msg)
		       for lex_ = (substitute #\_ #\Space lex)
		       for sense-info = (util::find-arg (cddr msg) :sense-info)
		       for tag-is-for-pertainym =
			   (and (eq 'word msg-type)
				(member lex pertainym-strings :test #'string=))
		       when (and tag-is-for-pertainym
		       		 ;; don't add :wn-sense-keys if already there
		       		 (notany (lambda (si)
				 	   (member :wn-sense-keys si))
					 sense-info))
		       do (loop for (ptr target-ss) in pertainym-pairs
				when (member lex (get-adj-synonyms target-ss)
					     :test #'string=)
				do (pushnew
				     `(:wn-sense-keys
				         (,(get-sense-key target-ss lex_)))
				     sense-info
				     :test #'equalp)
				)
		       when (and tag-is-for-pertainym sense-info)
		       collect `(,(intern "WORD" :parser) ,lex
				   :sense-info ,sense-info)
		       ))
	       )
	  (when pertainym-infos
	    `((,(intern "WORDNET" :parser)
		  :sense-key ,(get-sense-key synset word)
	    	  :pertainyms ,pertainym-infos))))))))
	  
(defun convert-synset (synset word score penntags trips-sense-list)
  "Given a Wordnet synset and a string of the original word being looked up, returns an entry to be sent back to the parser."
  (setq *parent-offset-list* nil) ;; reset parent offset list used in hierarchy search
  (let* ((hiers (get-hierarchy wm synset))
        (synonyms (get-synonyms synset))
	(lex-filenum (get-lex-filenum synset))
	(constit-type (convert-wordnet-pos-to-trips (get-pos-string synset)))
        (verb-frames (get-verb-frame-numbers synset))
	(mapped-ont-type (remove-duplicates (mapcar #'convert-hierarchy hiers)))
	)
    (cond ((and trips-sense-list
		(or (lxm::is-duplicate-sense constit-type (car mapped-ont-type) trips-sense-list)
		    (lxm::is-subsumed-sense constit-type (car mapped-ont-type) trips-sense-list)))
;		(is-duplicate-or-subsumed-sense constit-type (car mapped-ont-type) trips-sense-list))
	   (setq *has-trips-duplicate* t)
	   (print-debug "discarding duplicate or subsumed sense: mapped-sense ~S trips-senses ~S~%" mapped-ont-type trips-sense-list)
	   )
	  (t
	   (if (and mapped-ont-type (eq (first mapped-ont-type) 'ont::situation-root))
	       (print-debug "At least one WordNet ~S sense of ~S maps to ont::situation-root~%" constit-type word))
	   (build-entry word 
			; part of speech
			:constit-type constit-type 
			; currently this is just agr and count/mass for nouns
			:features (get-syntactic-features constit-type penntags synset lex-filenum)		    
			; synonyms
			:like (remove word synonyms :test #'equal)
			:score score
		        ; logical forms
			:lfs mapped-ont-type
			; verb frames
			:wordnet-verb-frame 
			(convert-wordnet-verb-frames verb-frames word synonyms)
			:domain-info (get-domain-info synset word)
			:source :wordnet)))))

(defun convert-wordnet-pos-to-trips (pos)
  (second (assoc pos '(("noun" w::N) ("verb" w::V)
                       ("adj" w::ADJ) ("adv" w::ADV)) :test #'equal)))

(defun convert-wordnet-verb-frames (frames word synonyms)
  "Given the frames of a verb-synset, the specific word that we're looking up, and the synonym list for the synset, returns a list of verb frame numbers that apply."
  (let ((pos (position word synonyms :test #'equal)))
       (if (and frames pos)
           (convert-wordnet-verb-frames-helper frames 
                                               (1+ pos)))))

(defun convert-wordnet-verb-frames-helper (frames wordpos)
  "Recursive helper for convert-wordnet-verb-frames."
  (if (and frames wordpos)
      (let* ((first-frame (first frames)) ; we work on the first frame
             (frame-num (first first-frame))
             (word-num (second first-frame)))
            (if (or (= word-num 0) ; if the first frame applies to all words...
                    (= word-num wordpos)) ; or this word
                ; then we keep it and continue down the list of frames
                (cons frame-num (convert-wordnet-verb-frames-helper (cdr frames)
                                                                    wordpos))
                ; otherwise, we just continue down the list of frames
                (convert-wordnet-verb-frames-helper (cdr frames) wordpos)))))


;;;
;;; WordNet Hierarchy to TRIPS logical form
;;;
;;;   The strategy here is to try the most specific match first.  We walk
;;;   upwards in the hierarchy until we find a match.  8 out of 9 WordNet
;;;   noun beginners have suitable TRIPS equivalents.  Verbs are trickier.
;;;   Some verbs are handled, but many will default to ONT::SITUATION-ROOT.  
;;;   Adjectives and adverbs will go to ONT::MODIFIER and ONT::PREDICATE for the time being.
;;;

#| 12/2010 hierarchy-mapping table is no longer used 
(defvar hierarchy-mapping (make-hash-table :test #'equal))
" Hierarchy equivalents
 Implementation note: the keys for this hash table are lists.  The first
 element is a string of their part of speech character (see 
 wordnet::pos-sym-to-string) and the second element is the offset in the file.
 If you print a synset, it will give you both pieces of information.
 TODO: use load-into-hash"
; We start at the top: with the WordNet noun unique beginners
;; offsets for WN 3.0
(dolist (pairs '((("n" 00002684) ONT::PHYS-OBJECT) ; "physical" "object"
		 (("n" 00029378) ONT::EVENT) ; "event"
                 (("n" 00030358) ONT::ACTING) ; "act" "human_action" "human_activity"
                 (("n" 00034213) ONT::SITUATION) ; "phenomenon" "not" "a" "perfect" "match"
                 (("n" 00002137) ONT::ABSTRACT-OBJECT) ; "abstraction"
                 (("n" 00023100) ONT::ABSTRACT-OBJECT) ; "psychological" "feature"
                 (("n" 00031264) ONT::GROUP-OBJECT) ; group.1 "group"
		 (("n" 14621446) ONT::GROUP-OBJECT) ; group.2
		 (("n" 06016853) ONT::GROUP-OBJECT) ; group.3
                 (("n" 13809207) ONT::PART) ; "part"
		 (("n" 00021939) ONT::MANUFACTURED-OBJECT) ; "artifact"
                 (("n" 03183080) ONT::MANUFACTURED-OBJECT) ; "device"
                 (("n" 03575240) ONT::MANUFACTURED-OBJECT) ; "instrumentality" "instrumentation"
		 (("n" 00024264) ONT::ATTRIBUTE) ; "attribute"		 
		 (("n" 00004475) ONT::ORGANISM) ; "body" "organism"
		 (("n" 00019128) ONT::NATURAL-OBJECT) ; "natural" "object"		 
		 (("n" 00023271) ONT::MENTAL-OBJECT) ; "cognition" "knowledge" "noesis"
		 (("n" 06634376) ONT::INFORMATION) ; "information" "info"
		 (("n" 00033020) ONT::COMMUNICATION) ; "communication"
		 (("n" 00026192) ONT::EXPERIENCER-EMOTION) ; "feeling"		 
		 (("n" 00021265) ONT::FOOD) ; "food" "nourishment"
		 (("n" 07567707) ONT::FOOD) ; "meal"
                 (("n" 07555863) ONT::FOOD) ; "food" (solid)
		 (("n" 00027167) ONT::GEO-OBJECT) ; "location"
                 (("n" 08664443) ONT::PLACE) ; "place"
                 (("n" 08513718) ONT::PLACE) ; "place" "property"
		 (("n" 00023773) ONT::MOTIVE) ; "motive" "motivation"
		 (("n" 05980875) ONT::OBJECTIVE) ; "goal"		 
                 (("n" 00007846) ONT::PERSON) ; "person"
		 (("n" 00017222) ONT::PLANT) ; "plant"
		 (("n" 13244109) ONT::POSSESSION)
		 (("n" 00032613) ONT::POSSESSION) ; "possession"
		 (("n" 00029677) ONT::PROCESS) ; process.6
		 (("n" 00033615) ONT::QUANTITY) ; "quantity"
		 (("n" 00031921) ONT::RELATION) ; "relation"		 
		 (("n" 00027807) ONT::PHYSICAL-DISCRETE-DOMAIN) ; "shape"
		 (("n" 05064037) ONT::PHYSICAL-DISCRETE-DOMAIN) ; "shape"
		 (("n" 00024720) ONT::STATUS) ; state.2 "state"
		 (("n" 14479615) ONT::STATUS) ; state.5
		 (("n" 08654360) ONT::STATE) ; state.1 "state" "province"
		 (("n" 08168978) ONT::STATE) ; state.4
		 (("n" 08544813) ONT::STATE) ; state.7
                 (("n" 00020827) ONT::SUBSTANCE) ; "substance"
		 (("n" 00019613) ONT::SUBSTANCE) ; substance.1
		 (("n" 05921123) ONT::GIST) ; substance.2
		 (("n" 14580597) ONT::SUBSTANCE) ; substance.4
		 (("n" 00020090) ONT::SUBSTANCE) ; substance.7
                 (("n" 14940100) ONT::LIQUID-SUBSTANCE) ; "liquid"
                 (("n" 15046900) ONT::SOLID-SUBSTANCE) ; "solid"
		 (("n" 15113229) ONT::TIME-INTERVAL) ; "time" "period"

		 ;; other mappings -- not in unique beginners
                 (("n" 05844663) ONT::COLOR) ; "color"
                 (("n" 04956594) ONT::COLOR) ; "color"
 
		 ;(("n" 00033615) ONT::MEASURE-UNIT) ; "unit" "of" "measurement"
                 (("n" 15169873) ONT::LENGTH-UNIT) ; "linear" "unit"
                 (("n" 13608788) ONT::WEIGHT-UNIT) ; "weight" "unit"
                 (("n" 13600822) ONT::VOLUME-UNIT) ; "volume" "unit"
                 (("n" 13608598) ONT::TEMPERATURE-UNIT) ; "temperature" "unit"
                 (("n" 06284225) ONT::LINGUISTIC-OBJECT) ; "linguistic" "unit"
                 (("n" 04524313) ONT::VEHICLE) ; "vehicle"
                 (("n" 03100490) ONT::VEHICLE) ; "conveyance" "transport"
                 (("n" 04576211) ONT::LAND-VEHICLE) ; "wheeled" "vehicle"
                 (("n" 02686568) ONT::AIR-VEHICLE) ; "aircraft"
                 (("n" 04468005) ONT::LAND-VEHICLE) ; "train" "railroad_train"
                 (("n" 02958343) ONT::LAND-VEHICLE) ; "car" "auto" "automobile" "machine" "motorcar"
                 (("n" 02691156) ONT::AIR-VEHICLE) ; "airplane" "aeroplane" "plane"
                 (("n" 04490091) ONT::LAND-VEHICLE) ; "truck" "motortruck"
                 (("n" 05220461) ONT::BODY-PART) ; "body" "part"
		 (("n" 14299637) ONT::PHYSICAL-SYMPTOM) ; "symptom"
		 (("n" 14061805) ONT::ILLNESS) ; "illness"
		 (("n" 14052046) ONT::ILLNESS) ; "ill" "health"

		 ;; Temporal noun mappings
		 (("n" 15154774) ONT::TIME-UNIT)
		 (("n" 15180528) ONT::TIME-POINT)
		 (("n" 07309599) ONT::TIME-POINT) ; time.1
		 (("n" 15270431) ONT::TIME-INTERVAL) ; time.2
		 (("n" 15245515) ONT::TIME-POINT) ; time.4
		 (("n" 15122231) ONT::TIME-INTERVAL) ; time.3
		 (("n" 00028270) ONT::TIME-INTERVAL) ; time.5
		 (("n" 15129927) ONT::TIME-INTERVAL) ; time.7: clock time
		 (("n" 15269513) ONT::TIME-INTERVAL)
		 (("n" 15209413) ONT::MONTH-NAME)
		 (("n" 15163005) ONT::DAY-NAME)
		 (("n" 00028270) ONT::DATE-OBJECT)
		 (("n" 15160579) ONT::DATE-OBJECT)

		 ;; Physical object mappings
		 (("n" 07810907) ONT::CONDIMENTS)
		 (("n" 13135832) ONT::NUTS-SEEDS)
		 (("n" 07829412) ONT::DRESSINGS-SAUCES-COATINGS)
		 (("n" 11748002) ONT::BEANS-PEAS)
		 (("n" 07583197) ONT::SOUP)
		 (("n" 07707451) ONT::VEGETABLE)
		 (("n" 07705931) ONT::FRUIT)
		 (("n" 04595028) ONT::WIRELESS)
		 (("n" 03082979) ONT::COMPUTER)
		 (("n" 04009552) ONT::PROJECTOR)
		 (("n" 04298308) ONT::STAIRS)
		 (("n" 03748886) ONT::PRODUCT)
		 (("n" 03076708) ONT::COMMODITY)
		 (("n" 06806469) ONT::ICON)
		 (("n" 06481320) ONT::LIST)
		 (("n" 06417598) ONT::REFERENCE-WORK)
		 (("n" 06268096) ONT::ARTICLE)
		 (("n" 06410904) ONT::BOOK)
		 (("n" 02870092) ONT::BOOK)
		 (("n" 06279326) ONT::EMAIL)
		 (("n" 06516595) ONT::FINANCIAL-STATEMENT)
		 (("n" 04105893) ONT::INTERNAL-ENCLOSURE)
		 (("n" 03546340) ONT::LODGING)
		 (("n" 02692232) ONT::AIRPORT)
		 (("n" 02885108) ONT::BOXCAR)
		 (("n" 10388924) ONT::POSSESSOR-RELN)
		 (("n" 10389398) ONT::POSSESSOR-RELN)
		 (("n" 10060175) ONT::ENTRANT)
		 (("n" 09610660) ONT::COMMUNICATION-PARTY)
		 (("n" 09923673) ONT::INHABITANT)
		 (("n" 09620078) ONT::INHABITANT)
		 (("n" 10287213) ONT::MALE)
		 (("n" 10787470) ONT::FEMALE)
		 (("n" 10112591) ONT::FRIEND)
		 (("n" 09612848) ONT::CONSUMER)
		 (("n" 09629752) ONT::TRAVELLER)
		 (("n" 09820263) ONT::ATHLETE)
		 (("n" 10165109) ONT::HEALTH-PROFESSIONAL)
		 (("n" 09617867) ONT::SPECIALIST)
		 (("n" 10794014) ONT::AUTHOR)
		 (("n" 10665698) ONT::SCHOLAR)
		 (("n" 10557854) ONT::SCHOLAR)
		 (("n" 10480253) ONT::PROFESSIONAL)
		 (("n" 10235549) ONT::FAMILY-RELATION)
		 (("n" 10741590) ONT::USER)
		 (("n" 10042300) ONT::EATER)
		 (("n" 02159955) ONT::INSECT)
		 (("n" 02512053) ONT::FISH)
		 (("n" 14856263) ONT::DISPOSABLE)
		 (("n" 14845743) ONT::WATER)
		 (("n" 05397468) ONT::BODILY-FLUID)
		 (("n" 11494638) ONT::PRECIPITATION)
		 (("n" 11525955) ONT::AIR-CURRENT)
		 (("n" 11524662) ONT::WEATHER)
		 (("n" 01348530) ONT::BACTERIUM)
		 (("n" 13083586) ONT::PLANT)
		 (("n" 01098698) ONT::COMMERCIAL-ACTION) ; investment as action
		 (("n" 13329641) ONT::ASSETS) ; investment as object
		 (("n" 00001740) ONT::REFERENTIAL-SEM)
		 
		 ;; Abstract object mappings
		 (("n" 05726596) ONT::ARRANGING)
		 (("n" 01218327) ONT::RESERVE)
		 (("n" 13920835) ONT::COMFORTABLENESS)
		 (("n" 14285662) ONT::INJURY)
		 (("n" 07020895) ONT::MUSIC)
		 (("n" 05846355) ONT::CONSTRAINT)
		 (("n" 00803617) ONT::CONSTRAINT)
		 (("n" 06516955) ONT::ACCOUNT-PAYABLE)
		 ;(("n" 06284225) ONT::LINGUISTIC-COMPONENT) conflicts with linguistic-object above
		 (("n" 06568978) ONT::COMPUTER-PROGRAM)
		 (("n" 06570110) ONT::COMPUTER-PROGRAM)
		 (("n" 07289014) ONT::TROUBLE)
		 (("n" 05687338) ONT::TROUBLE)
		 (("n" 07420770) ONT::TROUBLE)
		 (("n" 08491027) ONT::LOCATION-ID)
		 (("n" 05919866) ONT::CONTENT) ; substance.3
		 (("n" 06598915) ONT::CONTENT) ; substance.6
		 (("n" 05725378) ONT::TEMPERATURE)
		 (("n" 05011790) ONT::TEMPERATURE)
		 (("n" 05093581) ONT::LINEAR-D)
		 (("n" 13600404) ONT::AREA-UNIT)
		 (("n" 13603305) ONT::LENGTH-UNIT)
		 (("n" 13608319) ONT::ACOUSTIC-UNIT)
		 (("n" 13639927) ONT::LUMINOSITY-UNIT)
		 ;; (the following abstract object mappings were found automatically and judged by Mary to be good)
		 (("n" 07185076) ONT::GIVING)
		 (("n" 00265992) ONT::IMPROVE)
		 (("n" 00191142) ONT::CHANGE-STATE) ; change.3		 
		 (("n" 06749881) ONT::PREDICT)
		 (("n" 07196075) ONT::INTERVIEW)
	
		 (("n" 00079018) ONT::PURCHASE)
		 (("n" 00737188) ONT::ABNORMALITY)
		 (("n" 11523538) ONT::VOLTAGE)
		 (("n" 06404582) ONT::SIGNATURE)
		 (("n" 00654885) ONT::TREATMENT)
		 (("n" 14449405) ONT::LACK)
		 (("n" 07215568) ONT::SOFTWARE-FEATURE)
		 (("n" 09815790) ONT::ASSISTANT)
		 (("n" 05790758) ONT::FAVORITE)
		 (("n" 05790944) ONT::OPTION)
		 (("n" 00306426) ONT::TRIP)
		 (("n" 06674188) ONT::PASSWORD)
		 (("n" 07283608) ONT::HAPPEN)
		 (("n" 13604718) ONT::MONEY-UNIT)
		 (("n" 04952242) ONT::LUMINOSITY-VAL)
		 (("n" 05027529) ONT::WEIGHT)
		 (("n" 05026843) ONT::WEIGHT)
		 (("n" 13601596) ONT::MEMORY-UNIT)
		 (("n" 13609214) ONT::WEIGHT-UNIT)
		 (("n" 13602526) ONT::POWER-UNIT)
		 ;(("n" 13583724) ONT::PHYSICAL-UNIT) conflict
		 (("n" 13817526) ONT::PERCENT)
		 (("n" 13244109) ONT::POSSESSION)
		 (("n" 08050678) ONT::FEDERAL-ORGANIZATION)
		 ;(("n" 08008335) ONT::COMPANY) conflict
		 (("n" 07185325) ONT::REQUEST)
		 (("n" 01023820) ONT::PROCESS) ; process.1
		 (("n" 00029677) ONT::COGITATION) ; process.2
		 (("v" 00515154) ONT::NATURE-CHANGE) ;; process.1
		 (("v" 01668603) ONT::NATURE-CHANGE) ;; process.6
		 (("v" 02438535) ONT::MANAGING) ;; process.2
		 (("v" 00638837) ONT::BECOMING-AWARE-OF-VALUE) ;; process.3
		 (("n" 13244109) ONT::ATTRIBUTE) ; property.1
		 (("n" 04916342) ONT::ATTRIBUTE) ; property.2
		 (("n" 05849040) ONT::GEO-FORMATION) ; property.3
		 (("n" 05849040) ONT::ATTRIBUTE) ; property.4

		 ;; measure-unit and related abstract-object mappings found by
                 ;; James
		 (("n" 07963711) ONT::COMBINATION)
		 (("n" 08456993) ONT::SEQUENCE)
		 (("n" 07951464) ONT::COLLECTION)
		 (("n" 08198398) ONT::MILITARY-GROUP)
		 (("n" 08008335) ONT::ORGANIZATION)
		 (("n" 07950920) ONT::SOCIAL-GROUP)
		 (("n" 00031264) ONT::GROUP-OBJECT)
		 (("n" 03225238) ONT::DOSE)
		 (("n" 13614764) ONT::VOLUME-MEASURE-UNIT)
		 (("n" 15279767) ONT::SPEED-UNIT)
		 (("n" 15280346) ONT::SPEED-UNIT)
		 (("n" 15286249) ONT::RATE-UNIT)
		 (("n" 13635108) ONT::POWER-UNIT)
		 (("n" 13634418) ONT::LUMINOSITY-UNIT)
		 (("n" 13600097) ONT::ANGLE-UNIT)
		 (("n" 13583724) ONT::MEASURE-UNIT)
		 
		 ;; noun mappings under ONT::situation-root found by James
		 (("n" 00407535) ONT::ACTIVITY)
		 (("n" 00426928) ONT::PHYSICAL-ACTIVITY)
		 (("n" 07279453) ONT::DIRECT-INFORMATION)
		 (("n" 07955057) ONT::CONTENT)

		 (("n" 07355491) ONT::LIFECYCLE-EVENT)
		 (("n" 07320302) ONT::LIFECYCLE-EVENT)

		 ;; major verb groupings in wn
		 ; body
		 (("v" 0046534) ONT::PUT-ON) ; "bodily" "care" "grooming"
		 ; change
		 (("v" 0109660) ONT::CHANGE) ; change.2
		 ;; cognition
		 ;; we no longer have ONT::think -- wdebeaum
		 ;(("v" 0689344) ONT::THINK) ; "think" "believe" "consider" "conceive"
                 ;(("v" 0628491) ONT::THINK) ; "think" "cogitate" "cerebrate"
		 ;; communication
		 (("v" 0831651) ONT::STATEMENT) ; "inform"
                 (("v" 0962447) ONT::STATEMENT) ; "talk" "speak"
                 (("v" 1009240) ONT::STATEMENT) ; "state" "say" "tell"
		 ;; we no longer have ONT::ask -- wdebeaum
		 ;(("v" 0784342) ONT::ASK) ; "ask" "inquire" "enquire"
		 ;; competition
		 (("v" 1072262) ONT::FIGHTING) ; "compete"
		 ;; consumption
		 (("v" 1156834) ONT::CONSUME) ; "consume" "ingest" "take_in" "take" "have"
		 ;; contact
		 (("v" 1405044) ONT::HITTING) ; "hit"
		 ;(("v" 1236164) ONT::MEET) ; "hit" "against" conflict
		 ;; creation
		 (("v" 1617192) ONT::CREATE) ; "make" "create"
		 ;; emotion
		 (("v" 1771535) ONT::EXPERIENCER-EMOTION) ; "feel" "experience"
		 (("v" 1777210) ONT::EXPERIENCER-EMOTION) ; "like"
		 ;; motion
		 (("v" 1835496) ONT::MOVE) ; "travel" "go" "move" "locomote"
		 ;; perception
		 (("v" 2106006) ONT::ACTIVE-PERCEPTION) ; "feel" "sense"		 
		 ;; possession
		 (("v" 2204692) ONT::POSSESS) ; "own" "have" "possess"
		 ;; social
		 (("v" 2486932) ONT::INTERSECT) ; "meet" "get" "together" ??
		 ;; stative
		 (("v" 2655135) ONT::HAVE-PROPERTY) ; "be"
		 ;; weather
		 (("v" 2756821) ONT::PRECIPITATING) ; "precipitate"

		 ;; other mappings
		 (("v" 2199590) ONT::GIVING) ; "give"
                 (("v" 1850315) ONT::TRANSFER) ; "move" "displace"
		 (("v" 2220461) ONT::TRANSFER) ; "transfer"
                 (("v" 1188725) ONT::WANT) ; "want" "need" "require"
                 (("v" 1158872) ONT::USE) ; use.1 "use" "utilize" "utilise" "apply" "employ"
		 (("v" 01165043) ONT::USE) ; use.2
		 (("v" 01158572) ONT::USE) ; use.3
		 (("v" 02600490) ONT::USE) ; use.4
		 (("v" 02561332) ONT::USE) ; use.5
		 (("v" 02370131) ONT::USE) ; use.6
		 (("n" 00949134) ONT::USE)  ; ??
		 (("n" 00947128) ONT::USE) ; use.1
		 (("n" 05149325) ONT::USE) ; use.2
		 (("n" 05149978) ONT::USE) ; use.3
		 (("n" 13451804) ONT::USE) ; use.4
		 (("n" 00414179) ONT::USE) ; use.5

		 (("n" 05194578) ONT::OBJECTIVE-INFLUENCE) ; force.1
		 (("n" 11458624) ONT::PHYSICAL-PHENOMENON) ; force.2
		 (("n" 05035353) ONT::PHYSICAL-PHENOMENON) ; force.3
		 (("v" 02504562) ONT::PROVOKE) ; force.1
		 (("v" 01650425) ONT::PROVOKE) ; force.2
		 (("v" 01871979) ONT::PUSH) ; force.3
		 
                 ;(("v" 0173338) ONT::REMOVE) ; "remove" "take" "take_away" "withdraw"
                 ;(("v" 2404904) ONT::REMOVE) ; "take_out" "move_out" "remove"
                 ;(("v" 2224055) ONT::REMOVE) ; "get" "rid" "of" "remove"
                 (("v" 0260648) ONT::REPAIR) ; "repair" "mend" "fix" "bushel" "doctor" "furbish_up" "restore" "touch_on"
		 
                 ;; verb mappings for the leaves in ONT::situation-root
		 ;; from ONT::mental-action to ONT::verb
		 ;; found by hand by wdebeaum
		 (("v" 1435380) ONT::SEND)
		 (("v" 1950798) ONT::SEND)
		 (("v" 1031256) ONT::SEND)
		 (("v" 1494310) ONT::PUT) ; put.1
		 (("v" 01493741) ONT::ARRANGING) ; put.2 ?
		 (("v" 00981276) ONT::EXPRESS) ; put.3
;		 (("v" 00735571) ONT::SCHEDULING) ; put.9 ont::scheduling no longer exists
		 
		 (("v" 1315613) ONT::PHYSICAL-SCRUTINY)
		 (("v" 0661824) ONT::PHYSICAL-SCRUTINY)
		 (("v" 2131279) ONT::PHYSICAL-SCRUTINY)
		 (("v" 1957529) ONT::RIDE)
		 (("v" 1955984) ONT::RIDE)
		 (("v" 1940403) ONT::FLY)
		 (("v" 1930874) ONT::DRIVE)
		 (("v" 0120796) ONT::DELIVER)
		 (("v" 1438304) ONT::DELIVER)
		 (("v" 2050132) ONT::PASS)
		 ;(("v" 0811375) ONT::AVOID-LOCATION)
		 (("v" 1277974) ONT::WRAP)
		 (("v" 1906823) ONT::WALKING)
		 (("v" 1904293) ONT::SWIM)
		 (("v" 1907258) ONT::ROTATE)
		 (("v" 1930482) ONT::MOVE-BY-MEANS)
		 (("v" 1842690) ONT::MOVE-BY-MEANS)
		 (("v" 1995549) ONT::GO-ON)
		 ;(("v" 2004874) ONT::GO-BACK) conflict
		 (("v" 1236164) ONT::HITTING)
		 (("v" 2066510) ONT::FLUIDIC-MOTION)
		 (("v" 2066939) ONT::FLUIDIC-MOTION)
		 (("v" 0452512) ONT::FILL-CONTAINER)
		 (("v" 2016523) ONT::ENTERING)
		 (("v" 1489465) ONT::UNLOAD)
		 (("v" 1488123) ONT::UNLOAD)
		 (("v" 2014165) ONT::DEPART)
		 (("v" 2051694) ONT::PASS-BY)
		 (("v" 2428924) ONT::MEET)
		 (("v" 2434976) ONT::GET-TOGETHER)
		 (("v" 2000868) ONT::FOLLOW)
		 (("v" 1998432) ONT::FOLLOW)
		 (("v" 1609287) ONT::PULL)
		 (("v" 1448100) ONT::PULL)
		 (("v" 1499510) ONT::POP)
		 (("v" 2078294) ONT::RETURN)
		 ;(("v" 2004874) ONT::RETURN) conflict
		 (("v" 01849221) ONT::move-toward) ; come
		 ;(("v" 2020590) ONT::ARRIVE-AT) conflict
		 (("v" 0344174) ONT::ARRIVE) ; arrive 
		 (("v" 2005948) ONT::ARRIVE)  ; arrive
		 (("v" 2028994) ONT::DISPERSE)
		 (("v" 2030424) ONT::DISPERSE)
		 (("v" 0549552) ONT::LEAVE-TIME)
		 (("v" 2730135) ONT::LEAVE-TIME)
		 (("v" 2721438) ONT::LEAVE-TIME)
		 (("v" 1988080) ONT::SPACE)
		 (("v" 1007924) ONT::SUMMARIZE)
		 (("v" 0733044) ONT::SOLVE)
		 (("v" 0634906) ONT::SOLVE)
		 (("v" 0610374) ONT::FAMILIAR)
		 (("v" 0594337) ONT::FAMILIAR)
		 (("v" 0608372) ONT::FAMILIAR)
		 (("v" 0712708) ONT::BET)
		 (("v" 2704617) ONT::WEIGH)
		 (("v" 2704818) ONT::WEIGH)
		 (("v" 2006834) ONT::REACH)
		 ;(("v" 2020590) ONT::REACH) conflict
		 (("v" 1127795) ONT::PROTECTING)
		 (("v" 2712914) ONT::ORIENT)
		 (("v" 2713184) ONT::ORIENT)
		 (("v" 2659763) ONT::LOCATIONREL)
		 (("v" 2106506) ONT::ACTIVE-PERCEPTION)
		 (("v" 2130524) ONT::ACTIVE-PERCEPTION)
		 (("v" 1693881) ONT::COPY-DATA)
		 (("v" 1085474) ONT::HINDERING)
		 (("v" 2452885) ONT::HINDERING)
		 (("v" 02653381) ONT::EXISTS) ; consist.1
		 (("v" 02750854) ONT::COMPRISE) ; consist.2
;		 (("v" 02658050) ONT::CORRELATION) ; consist.3
;		 (("v" 02633356) ONT::COMPRISE) ; consist.4
		 (("v" 2627934) ONT::NECESSITY)
		 (("v" 01188725) ONT::NECESSITY)
	 	 (("v" 01189113) ONT::NECESSITY)
		 (("n" 14449126) ONT::NECESSITY)
		 (("n" 09367991) ONT::NECESSITY)
		 (("n" 00023773) ONT::NECESSITY)
		 (("v" 0708538) ONT::INTENTION)
		 (("v" 0620673) ONT::CONFUSE)
		 (("v" 1657254) ONT::CONFUSE)
		 (("v" 0619610) ONT::CONFUSE)
		 
		 ;; ... and from ONT::accommodate to ONT::employing
		 ;; found by hand by james
		 (("v" 0637259) ONT::BECOMING-AWARE-OF-VALUE) ;; calculate
		 (("v" 00670261) ONT::BECOMING-AWARE-OF-VALUE) ;; evaluate
		 (("v" 0703512) ONT::COGITATION)
		 (("v" 00690614) ONT::come-to-understand) ; consider.1
;		 (("v" 02166460) ONT::COGITATION) ; consider.2
;		 (("v" 00734054) ONT::COGITATION) ; consider.3
;		 (("v" 00950431) ONT::COGITATION) ; consider.4
;		 (("v" 00813044) ONT::COGITATION) ; consider.5
;		 (("v" 00689344) ONT::come-to-understand) ; consider.6
;		 (("v" 02133185) ONT::active-perception) ; consider.7
;		 (("v" 02130300) ONT::COGITATION) ; consider.8
		 (("v" 00628491) ont::cogitation)
		 (("v" 0597915) ONT::LEARN)
		 (("v" 2144835) ONT::HIDE)
		 (("v" 2421374) ONT::RELEASING)
		 (("v" 02522864) ONT::ACHIEVE) ; manage#1
		 (("v" 02436349) ONT::MANAGING) ; manage#2
;		 (("v" 02587532) ONT::MANAGING) ; manage#3 ??
;		 (("v" 02443049) ONT::MANAGING) ; manage#4
;		 (("v" 02527431) ONT::ACHIEVE)  ; manage#5
;		 (("v" 02523221) ONT::MANAGING) ; manage#6 ??
;		 (("v" 01224415) ONT::BODY-MANIPULATION) ; manage#7   
		 (("v" 1224744) ONT::MANIPULATE)
		 (("v" 1765392) ONT::SUBDUING)
		 (("v" 1805523) ONT::MISSES)
		 (("v" 1825237) ONT::WANT)
		 (("v" 1805982) ONT::APPRECIATE)
		 (("v" 1820302) ONT::APPRECIATE)
		 (("v" 2702508) ONT::COSTS)
		 (("v" 0719734) ONT::EXPECTATION)
		 (("v" 2635189) ONT::CORRELATION)
		 (("v" 0930599) ONT::CORRELATION)
		 (("v" 0772640) ONT::CORRELATION)
		 (("v" 2732798) ONT::CONTAINMENT)
		 (("v" 2471327) ONT::ENROLL)
		 (("v" 2402825) ONT::FIRE)
		 (("v" 2409412) ONT::EMPLOY)
		 (("v" 2542795) ONT::COMPLIANCE)
		 (("v" 0717358) ONT::RESPONSE)
		 (("v" 1012073) ONT::CONFIRM)
		 (("v" 0797697) ONT::CONSENT)
		 (("v" 0743344) ONT::ESTABLISH-COMMUNICATION)
		 (("v" 0789448) ONT::ESTABLISH-COMMUNICATION)
		 (("v" 1004692) ONT::TYPE)
		 (("v" 1745722) ONT::PRINT)
		 (("v" 0200397) ONT::REVISE)
		 (("v" 0996102) ONT::REVISE)
		 (("v" 0875394) ONT::SUGGEST)
		 (("v" 0892315) ONT::THANK)
		 (("v" 0884011) ONT::PROMISE)
		 (("v" 0843468) ONT::INDICT)
		 (("v" 0846509) ONT::DEFAME)
		 (("v" 0842989) ONT::ACCUSE)
		 (("v" 0907147) ONT::COMPLAIN)
		 (("v" 1020005) ONT::REPORT-SPEECH)
		 (("v" 0815686) ONT::ANSWER)
		 (("v" 0958334) ONT::REPEAT)
		 (("v" 2148788) ONT::SHOW) ; show#1
		 (("v" 00664788) ONT::CONFIRM) ; show#2
;		 (("v" 01015244) ONT::CONFIRM) ; show#3
;		 (("v" 02137132) ONT::SHOW) ; show#4
;		 (("v" 01686956) ONT::SHOW) ; show#5
;		 (("v" 00943837) ONT::NONVERBAL-EXPRESSION) ; show#6
;		 (("v" 00923793) ONT::SHOW) ; show#7
;		 (("v" 02139544) ONT::SHOW) ; show#8 become-visible; change type??
;		 (("v" 00922867) ONT::REGISTER) ; show#9
;		 (("v" 00923307) ONT::CONFIRM) ; show#10
;		 (("v" 02000547) ONT::SHOW) ; show#11
;		 (("v" 01086549) ONT::SHOW) ; show#12 race term, to show; change type??
		 (("v" 0987071) ONT::DESCRIBE)
		 (("v" 0939277) ONT::TRANSFER-INFORMATION)
		 (("v" 1010118) ONT::INFORM)
		 (("v" 0873682) ONT::INFORM)
		 (("v" 0751567) ONT::COMMAND)
		 (("v" 0752764) ONT::REQUEST)
		 (("v" 0784342) ONT::QUESTIONING)
		 (("v" 0937879) ONT::ENCODING)
		 (("v" 0514069) ONT::HIGHLIGHT)
		 (("v" 2140033) ONT::VISUAL-DISPLAY)
		 (("v" 0625119) ONT::READ)
		 (("v" 00626428) ONT::READ)
		 (("v" 00624476) ONT::READ)
		 (("v" 00593852) ONT::READ)
		 (("v" 0941990) ONT::TALK)
		 (("v" 00878136) ONT::TALK)
		 (("v" 0808855) ONT::INTERVIEW)
		 (("v" 0761713) ONT::NEGOTIATE)
		 (("v" 0740577) ONT::COMMUNICATION)
		 (("v" 0742320) ONT::COMMUNICATION)
		 (("v" 1593937) ONT::CRUSH)
		 (("v" 0281101) ONT::COLORING)
		 (("v" 1269008) ONT::COLORING)
		 (("v" 0283911) ONT::COLORING)
		 (("v" 1380638) ONT::COLLECT)
		 (("v" 0468791) ONT::ARRANGING)
		 (("v" 1463963) ONT::ARRANGING)
		 (("v" 0231557) ONT::GROW)
		 (("v" 0339934) ONT::HAPPEN)
		 (("v" 00341917) ONT::HAPPEN)
		 (("v" 0484166) ONT::COMPLETE)
		 (("v" 1356038) ONT::UNATTACH)
;		 (("v" 1552519) ONT::CUT) ; cut.1
;		 (("v" 01610834) ONT::CUT) ; cut.4
;		 (("v" 00429060) ONT::DECREASE) ; cut.2
;		 (("v" 02422283) ONT::CAUSE-COME-FROM) ; cut.5
;		 (("v" 01510576) ONT::TURN-OFF) ; cut.27
		 (("v" 1556921) ONT::SEPARATION)
		 (("v" 1462005) ONT::COMBINE-OBJECTS)
		 (("v" 0184117) ONT::COMBINE-OBJECTS)
		 (("v" 0394813) ONT::COMBINE-OBJECTS)
		 (("v" 2684784) ONT::STOP)
		 (("v" 2609764) ONT::STOP)
		 (("v" 2680814) ONT::STOP)
		 (("v" 1858686) ONT::RESTART)
		 (("v" 00345761) ONT::START)
		 (("v" 00348746) ONT::START)
		 (("v" 1628449) ONT::START)
		 (("v" 0022686) ONT::REVIVING)
		 (("v" 0098083) ONT::REVIVING)
		 (("v" 2014024) ONT::EVACUATE)
		 (("v" 0878636) ONT::SUBMIT)
		 (("v" 2257767) ONT::REPLACEMENT)
		 (("v" 1532589) ONT::CLEAN)
		 (("v" 1345109) ONT::CLOSURE)
		 (("v" 1346003) ONT::CLOSURE)
		 (("v" 1573515) ONT::BREAK-OBJECT)
		 (("v" 0336260) ONT::BREAK-OBJECT)
		 (("v" 0334996) ONT::BREAK-OBJECT)
		 (("v" 0258857) ONT::BREAK-OBJECT)
		 (("v" 0334186) ONT::BREAK-OBJECT)
		 (("v" 01562061) ONT::BREAK-OBJECT)
		 (("v" 02019282) ONT::BREAK-OBJECT)
		 (("v" 01561819) ONT::BREAK-OBJECT)
		 (("n" 00126236) ONT::BREAK-OBJECT)
		 (("n" 07301950) ONT::BREAK-OBJECT)
		 (("v" 00149583) ONT::BECOME) ; become.1
		 (("v" 02623529) ONT::BECOME) ;; become.2
		 (("v" 2623529) ONT::BECOME) ; become.3
		 (("v" 00542120) ONT::BECOME) 
		 (("v" 0140123) ONT::CHANGE-STATE)
		 (("v" 2212825) ONT::REFUSE)
		 (("v" 1752884) ONT::CAUSE-EFFECT)
		 (("v" 1519977) ONT::UNDO)
		 (("v" 1712704) ONT::EXECUTE)
		 (("v" 01640855) ONT::EXECUTE) ; action.2
		 (("v" 0766418) ONT::PROVOKE)
		 (("v" 2547586) ONT::HELP)
		 (("v" 0770437) ONT::CAUSE-ANOTHER-TO-ACT)
		 (("v" 1645601) ONT::CAUSE) ; cause.1
		 (("v" 00770437) ONT::CAUSE-MAKE) ; cause.2
		 (("n" 07326557) ONT::CAUSE) ; cause.1
		 (("n" 06740402) ONT::CAUSE) ; cause.2
		 (("n" 00007347) ONT::CAUSE) ; cause.4
		 (("v" 0822367) ONT::CATEGORIZATION)
;		 (("v" 00956687) ONT::CATEGORIZATION) ; characterize.1
;		 (("v" 02697950) ONT::CATEGORIZATION) ; characterize.2		 
		 (("v" 1802494) ONT::NONVERBAL-EXPRESSION)
		 (("v" 0010054) ONT::UNCONTROLLED-BODY-MOTION)
		 (("v" 1888511) ONT::UNCONTROLLED-BODY-MOTION)
		 (("v" 1983771) ONT::BODY-MOVEMENT)
		 (("v" 1216670) ONT::BODY-MANIPULATION)
		 (("v" 0591519) ONT::BECOMING-AWARE)
		 (("v" 02106506) ont::becoming-aware)
		 (("v" 1543123) ONT::BE-AT-LOC)
		 (("v" 2737876) ONT::SHOULD-BE-AT)
		 (("v" 2133435) ONT::POSSIBLY-EXISTS)
		 (("v" 2749904) ONT::EXISTS)
		 (("v" 2603699) ONT::EXISTS)
		 (("v" 0591115) ONT::come-to-UNDERSTAND)
		 (("v" 0588888) ONT::come-to-UNDERSTAND)
		 (("v" 0631737) ONT::SUPPOSE)
		 (("v" 00917300) ONT::suppose) ;; suppose
		 (("v" 0607780) ONT::REMEMBER)
		 (("v" 00604576) ONT::memorize)
		 (("v" 0917772) ONT::PREDICT)
		 (("v" 0619869) ONT::MISUNDERSTAND)
		 (("v" 0594621) ONT::KNOW)
		 (("v" 00595935) ONT::KNOW)
		 (("v" 00595630) ONT::KNOW)
		 (("v" 0610167) ONT::FORGET)
		 (("v" 00609100) ONT::FORGET)
		 (("v" 00614829) ONT::FORGET)
		 (("v" 00613018) ONT::FORGET)
		 (("v" 0689344) ONT::BELIEVE)
		 (("v" 0632236) ONT::ASSUME)
		 (("v" 0628491) ONT::AWARENESS)
		 (("n" 00624738) ONT::WORKING-OUT)
		 (("v" 1166093) ONT::WORKING-OUT)
		 (("v" 00100551) ONT::WORKING-OUT)
		 (("v" 1196037) ONT::DIETING)
		 (("v" 2413480) ONT::WORKING)
		 (("v" 02373015) ONT::WORKING)
		 (("v" 2708420) ONT::SPEND-TIME)
		 (("v" 2388950) ONT::SOCIAL-ACTIVITY)
		 (("v" 2584661) ONT::RIOT)
		 (("v" 1198101) ONT::SMOKING)
		 (("v" 2684924) ONT::ACTIVITY-ONGOING)
		 (("v" 0010435) ONT::ACTING)  ; behave#1 act#2
;		 (("v" 02518161) ONT::ACTING)  ; behave#2 comport#2
;		 (("v" 02519666) ONT::ACTING)  ; behave#3 comport#1
;		 (("n" 00037396) ONT::ACTING) ; action.1
;		 (("n" 14006945) ONT::ACTING) ; action.2
;		 (("n" 13518963) ONT::ACTING) ; action.4
;		 (("n" 04835950) ONT::ACTING) ; action.6
;		 (("n" 00576451) ONT::ACTING) ; action.10
		 (("v" 0299580) ONT::ACCOMMODATE)
		 (("v" 2242464) ONT::COMMERCE-SELL)
		 (("v" 2251743) ONT::COMMERCE-PAY)
		 (("v" 2229055) ONT::FUTURE-GIVING)
		 (("v" 2294436) ONT::ASSIGN)
		 (("v" 2498320) ONT::RESERVE)
		 (("v" 1439190) ONT::APPROPRIATE)
		 (("v" 2207206) ONT::PURCHASE)
		 (("v" 2210855) ONT::ACQUIRE)
		 ;; mappings for new/renamed LF types:
		 (("v" 1806505) ONT::EVOKE-ATTRACTION)
		 (("v" 1821423) ONT::EVOKE-ATTENTION)
		 (("v" 0064643) ONT::EVOKE-INJURY)
		 (("v" 0075421) ONT::EVOKE-TIREDNESS)
		 (("v" 1792287) ONT::EVOKE-SHAME)
		 (("v" 1770501) ONT::EVOKE-DISTRESS)
		 (("v" 1821884) ONT::EVOKE-BOREDOM)
		 (("v" 1762528) ONT::EVOKE-EXCITEMENT)
		 (("v" 0991385) ONT::EVOKE-CLARITY)
		 (("v" 1817130) ONT::EVOKE-ANNOYANCE)
		 (("v" 0725274) ONT::EVOKE-SURPRISE)
		 (("v" 2194913) ONT::EVOKE-DISGUST)
		 (("v" 1779165) ONT::EVOKE-FEAR)
		 (("v" 1785971) ONT::EVOKE-ANGER)
		 (("v" 01798452) ONT::EVOKE-ANGER)
		 (("v" 01798782) ONT::EVOKE-ANGER)
		 (("v" 1813053) ONT::EVOKE-SADNESS)
		 (("v" 1815628) ONT::EVOKE-JOY)
		 (("v" 1827619) ONT::ENVYING)
		 (("v" 1767163) ONT::WORRYING)
		 (("v" 0724492) ONT::CARE)
		 (("v" 1780202) ONT::FEARING)
		 (("v" 1780729) ONT::FEARING)
		 (("v" 0668099) ONT::ENDURING)
		 (("v" 0911350) ONT::REGRETTING)
		 (("v" 1776727) ONT::DISLIKING)
		 (("v" 1192628) ONT::AFFORD)
		 (("v" 1767949) ONT::IMPRESS)
		 (("v" 2499629) ONT::PUNISH)
		 (("v" 2546075) ONT::REWARD)
		 (("v" 2516594) ONT::ABUSE)
		 (("v" 2237338) ONT::REJECT)
		 (("v" 0804802) ONT::CONTEST)
		 (("v" 1697816) ONT::WRITE)
		 (("v" 0903385) ONT::FORGIVE)
		 (("v" 0881661) ONT::CONGRATULATE)
		 (("v" 0900583) ONT::WELCOME)
		 (("v" 0897241) ONT::GREET)
		 (("v" 0892923) ONT::APOLOGIZE)
		 (("v" 0848420) ONT::INSULT)
		 (("v" 0856824) ONT::PRAISE)
		 (("v" 0823669) ONT::REPRIMAND)
		 (("v" 0826509) ONT::CRITICIZE)
		 (("v" 0870213) ONT::WARN)
		 (("v" 0772967) ONT::ANNOUNCE)
		 (("v" 0049900) ONT::UNDRESS)
		 (("v" 0047945) ONT::PUT-ON)
		 (("v" 0964694) ONT::CONVERSATION)
		 (("v" 0943563) ONT::CONVERSATION)
		 (("v" 0259560) ONT::DENT)
		 (("v" 1279631) ONT::DENT)
		 (("v" 0437732) ONT::DEVICE-ADJUST)
		 (("v" 0387919) ONT::DEVICE-ADJUST)
		 (("v" 0400427) ONT::LANGUAGE-ADJUST)
		 (("v" 0243900) ONT::LANGUAGE-ADJUST)
		 (("v" 2161758) ONT::VISUAL-ADJUST)
		 (("v" 2190632) ONT::VISUAL-ADJUST)
		 (("v" 2190786) ONT::VISUAL-ADJUST)
		 (("v" 0428583) ONT::ADJUST-TO-EXTREME)
		 (("v" 0427802) ONT::ADJUST-TO-EXTREME)
		 (("v" 0126264) ONT::MODIFY) ; change.1
;		 (("v" 0161225) ONT::CHANGE-FORMAT) ; change.6
		 (("v" 0270699) ONT::FLUCTUATE)
		 (("v" 0151689) ONT::DECREASE)
		 (("v" 0156601) ONT::INCREASE)
		 (("v" 00459498) ONT::INCREASE-SPEED)
		 (("v" 02058994) ONT::INCREASE-SPEED)
		 (("v" 02548893) ONT::INCREASE-SPEED)
		 (("v" 01644522) ONT::INCREASE-SPEED)
		 (("v" 2191106) ONT::CHANGE-MAGNITUDE)
		 (("v" 0169651) ONT::CHANGE-MAGNITUDE)
		 (("v" 0241689) ONT::SHRINK)
		 (("v" 0552815) ONT::DETERIORATE)
		 (("v" 0399074) ONT::DETERIORATE)
		 (("v" 0369864) ONT::COOL)
		 (("v" 0582743) ONT::BURN)
		 (("v" 1478603) ONT::CLOG)
		 (("v" 0256507) ONT::SWELL)
		 (("v" 0252019) ONT::LIFE-TRANSFORMATION)
		 (("v" 0142191) ONT::SHAPE-CHANGE)
		 (("v" 1659248) ONT::SHAPE-CHANGE)
		 (("v" 0098346) ONT::START-OBJECT)
		 (("v" 0306017) ONT::EXPLODE)
		 (("v" 0306723) ONT::EXPLODE)
		 (("v" 1663920) ONT::COOKING)
		 (("v" 1664172) ONT::COOKING)
		 (("v" 1653013) ONT::CAUSE-MAKE-THINGS)
		 (("v" 0890590) ONT::ENSURE)
		 (("v" 2423183) ONT::ALLOW)
		 (("v" 1030132) ONT::NAMING)
		 (("v" 0657260) ONT::CLASSIFY)
		 (("v" 00654625) ONT::CLASSIFY)
		 (("v" 02508803) ONT::CLASSIFY)
		 (("v" 00739662) ONT::CLASSIFY)
		 (("v" 0658052) ONT::ORDERING)
		 (("v" 0675412) ONT::RITUAL-CLASSIFICATION)
		 (("v" 2604760) ONT::BE-AT)
		 (("v" 0684645) ONT::DOUBT)
		 (("v" 0687295) ONT::DOUBT)
		 (("v" 2316649) ONT::SURRENDER)
		 (("v" 2327200) ONT::SUPPLY)
		 (("v" 2347220) ONT::LEND)
		 (("v" 2324182) ONT::LEND)
		 (("v" 2460199) ONT::LEASE-HIRE)
		 (("v" 2263027) ONT::DONATE)
		 (("v" 1215421) ONT::CAPTURE)
		 (("v" 1214265) ONT::ACQUIRE-BY-ACTION)
		 (("v" 2324026) ONT::BORROW)
		 (("v" 2320374) ONT::COMMERCE-COLLECT)
		 (("v" 2460619) ONT::LEASE-HIRE)
		 (("v" 0158804) ONT::AMASS)
		 ;; (remove/removing)
		 (("v" 0769553) ONT::CONVINCE)
		 (("v" 2314275) ONT::DEPRIVE)
		 (("v" 0194912) ONT::DESTROY-PART-OF-WHOLE)
	
		 (("v" 1447868) ONT::SQUEEZE)
		 (("v" 1311103) ONT::DIG)
		 (("v" 0449692) ONT::EMPTY)
		 (("v" 1592774) ONT::PULL-OFF)
		 (("v" 0179060) ONT::CAUSE-OFF)
		 (("v" 1351170) ONT::CAUSE-OUT-OF)
;		 (("v" 0173338) ONT::CAUSE-COME-FROM) ; remove#1
;		 (("v" 02404224) ONT::CAUSE-COME-FROM) ; remove#2
;		 (("v" 2224055) ONT::DISCARD) ; remove#3
;		 (("v" 02404904) ONT::CAUSE-COME-FROM) ; remove#4
;		 (("v" 02086458) ONT::TRANSFER) ; remove#5 transfer#8
;		 (("v" 00421535) ONT::DEPART) ; remove#6
;		 (("v" 02482425) ONT::DESTROY) ; remove#7
;		 (("v" 00571061) ONT::CAUSE-COME-FROM) ; remove#8 
		 
;		 (("v" 00761454) ONT::CAUSE-COME-FROM) ; clear (a debt)
		 (("v" 0528990) ONT::COME-FROM)

                 ;; ... and from ONT::enroll to ONT::measure-action
		 ;; found by hand by swift
		 (("v" 00708538) ONT::INTENTION) ;; intend, mean
		 (("v" 0709379) ONT::INTENTION) ;; intend, design, specify
		 (("v" 1790739) ONT::CONFUSE)
		 (("v" 2763740) ONT::LOCATION-OF-LIGHT)
		 (("v" 2765924) ONT::LOCATION-OF-LIGHT)
		 (("v" 2637938) ONT::WAIT)
		 (("v" 1493380) ONT::STOP-MOVE)
		 (("v" 0117985) ONT::STAY)
		 (("v" 0081725) ONT::CURE)
		 (("v" 2632353) ONT::LACKING) ; lack.1
		 (("n" 14449405) ONT::LACKING) ; lack.1
		 
		 (("v" 1467370) ONT::SURROUND)
		 (("v" 2023396) ONT::INTERSECT)
		 (("v" 2622234) ONT::CONNECTED)
		 (("v" 0235368) ONT::CIRCUMSCRIBE)
		 (("v" 1290255) ONT::ATTACH)
		 (("v" 1296462) ONT::ATTACH)
		 (("v" 0713167) ONT::ASSOCIATE)
		 ;(("v" 2622234) ONT::JOINING) conflict
		 (("v" 1291069) ONT::JOINING)
		 (("v" 2376958) ONT::INTERACT)
		 (("v" 2530167) ONT::TRY)
		 (("v" 02531625) ONT::TRY)
		 (("v" 01195299) ONT::TRY)
		 (("v" 00047317) ONT::TRY)
		 (("n" 00786195) ONT::TRY)
;		 (("v" 0017865) ONT::PREPARE-FOR-SLEEP)
		 (("v" 0613683) ONT::LEAVE-BEHIND)
		 (("v" 1431230) ONT::KISSING)
		 (("v" 2219940) ONT::HOST)
		 (("v" 1999798) ONT::GUIDING)
		 (("v" 1090335) ONT::FIGHTING)
		 (("v" 1647229) ONT::ESTABLISH)
		 (("v" 2427103) ONT::ESTABLISH)
		 (("v" 0679389) ONT::SELECT)
		 (("v" 0674607) ONT::SELECT)
		 (("v" 1189823) ONT::AVOIDING)
		 (("v" 2463510) ONT::AVOIDING)
		 (("v" 0811375) ONT::AVOIDING)
		 (("v" 2203362) ONT::HAVE)
		 (("v" 02632940) ONT::HAVE) ; include.1
		 (("v" 00684838) ONT::HAVE) ; include.2
		 (("v" 00183879) ONT::ADD-INCLUDE) ; include.3
		 (("v" 02449847) ONT::ALLOW) ; include.4
		 (("v" 0796588) ONT::EXCLUDE)
		 (("v" 2633714) ONT::EXCLUDE)
		 (("v" 2449340) ONT::EXCLUDE)
		 (("v" 0615774) ONT::EXCLUDE)
		 (("v" 02248465) ONT::FIND) ; find.1
		 (("v" 02285629) ONT::FIND) ; find.3
		 (("v" 02213336) ONT::FIND) ; find.7
;		 (("v" 02154508) ONT::come-to-understand) ; find.2
;		 (("v" 00918872) ONT::scrutiny) ; find.4
;		 (("v" 00715239) ONT::scrutiny) ; find.5
;		 (("v" 00971999) ONT::scrutiny) ; find.11
;		 (("v" 02128873) ONT::come-to-understand) ; find.6
;		 (("v" 01637982) ONT::come-to-understand) ; find.8
;		 (("v" 00721437) ONT::come-to-understand) ; find.9
;		 (("v" 02212275) ONT::managing) ; find.10
;		 (("v" 00522751) ONT::acquire) ; find.12
;		 (("v" 02247977) ONT::acquire) ; find.14
;		 (("v" 02197781) ONT::come-to-understand) ; find.13
;		 (("n" 05808218) ONT::come-to-understand) ; find.1
;		 (("n" 00043195) ONT::find) ; find.1
		 

		 ;; added for verbs with missing mappings
		 (("v" 02619739) ONT::SURVIVE) ;; outlast, outlive
		 (("v" 01108148) ONT::OVERCOME) ;; outsmart, overcome, beat/vanquish
		 (("v" 00983824) ONT::EMIT) ;; emit
		 (("v" 02134927) ONT::APPEARS-TO-HAVE-PROPERTY) ;; sound.1
;		 (("v" 02176268) ONT::MAKE-SOUND) ;; sound.2
;		 (("v" 02135048) ONT::MAKE-SOUND) ;; sound.3
;		 (("v" 02180529) ONT::MAKE-SOUND) ;; sound.4
;		 (("v" 02179518) ONT::MAKE-SOUND) ;; sound.6
;		 (("n" 04981139) ONT::MAKE-SOUND) ;; sound.1
;		 (("n" 05718254) ONT::MAKE-SOUND) ;; sound.2
;		 (("n" 11480930) ONT::MAKE-SOUND) ;; sound.3
;		 (("n" 07371293) ONT::MAKE-SOUND) ;; sound.4
		 (("n" 06278136) ONT::AUDIO) ;; sound.5
		 (("v" 01323958) ONT::destroy) ;; kill
		 (("v" 00462092) ONT::subduing) ;; subdue
		 (("v" 00358431) ONT::die) ;; die
		 (("n" 11444117) ONT::die) ;; death
		 (("n" 13962498) ONT::die) ;; death
		 (("n" 15143276) ONT::die) ;; death, last
		 (("n" 14562960) ONT::die) ;; death, end, destruction
		 (("n" 15143477) ONT::die) ;; death, demise
		 
		 (("v" 00233335) ONT::hindering) ;; restrict, constrain
		 (("v" 01387786) ONT::press) ;; compress, press
		 (("v" 00077698) ONT::bodily-process) ;; gasp, choke, strangle
		 (("v" 00076400) ONT::bodily-process) ; vomit#1
		 (("v" 02475922) ONT::assign) ;; appoint
		 (("v" 00940384) ONT::express) ;; express
		 (("v" 01061481) ONT::express) ;; state.3
		 (("v" 00490968) ONT::register) ; time.1
		 (("v" 00678547) ONT::planning) ; time.2
		 (("v" 02665282) ONT::object-compare) ; resemble.1
		 

		 ;; adj
		 (("a" 01409581) ONT::similar) ; like.1
		 (("a" 01411065) ONT::similar) ; like.2
		 (("a" 01410606) ONT::similar) ; like.3
		 (("a" 02063554) ONT::similar) ; like.4

;		 (("a" 00306314) ONT::able) ; capable.1
;		 (("a" 02361540) ONT::able) ; capable.2
;		 (("a" 00308015) ONT::able) ; capable.3
;		 (("a" 00051373) ONT::able) ; capable.4
;		 (("a" 00510348) ONT::able) ; capable.5
		 
		 (("v" 00705227) ONT::planning) ;; plan
		 (("v" 00704690) ONT::planning)
		 (("v" 01638368) ONT::planning)
		 (("v" 00917772) ont::predict) ;; predict
		 (("v" 00634472) ont::coming-to-believe) ;; conclude
		 (("v" 00681429) ONT::scrutiny) ;; analyse, assess
		 (("v" 02501278) ONT::scrutiny) ;; judge, adjudicate, try
	 	 (("v" 002165543) ONT::scrutiny) ;; inspect
		 (("v" 01844048) ONT::physical-scrutiny) ;; visit, inspect
		 (("v" 00697062) ONT::scrutiny) ;; audit
		 
		 ;; some verb mappings James found on 2010-01-22
		 (("v" 1759326) ONT::EVOKE-EMOTION) ;; arouse
		 (("v" 2247977) ONT::RETRIEVE) ;; retrieve
		 (("v" 2285629) ONT::FIND) ;; find
		 (("v" 1158022) ONT::TAKE-INCREMENTALLY) ;; drain
		 (("v" 2360003) ONT::TAKE-INCREMENTALLY) ;; bleed
		 (("v" 2573275) ONT::TAKE-BY-DECEPTION) ;; cheat

		 ;; senses of take
		 (("v" 02599636) ONT::EXECUTE) ;; take.1
		 (("v" 02267989) ONT::TAKE-TIME) ;; take.2
		 (("v" 01999798) ONT::GUIDING) ;; take.3
		 (("v" 01214265) ONT::APPROPRIATE) ;; take.4
		 (("v" 00524682) ONT::ACQUIRE) ;; take.5 ?
		 (("v" 00624476) ONT::CLASSIFY) ;; take.6
		 (("v" 02077656) ONT::TRANSPORT) ;; take.7
		 (("v" 02205272) ONT::ACQUIRE) ;; take.8
		 (("v" 01842690) ONT::MOVE-BY-MEANS) ;; take.9
		 (("v" 00674607) ONT::SELECT) ;; take.10
		 (("v" 02236124) ONT::ACQUIRE) ;; take.11
		 (("v" 02394183) ONT::APPROPRIATE) ;; take.12
		 (("v" 00734054) ONT::COGITATION) ;; take.13
		 (("v" 02627934) ONT::NECESSITY) ;; take.14
		 (("v" 02109045) ONT::HAVE-EXPERIENCE) ;; take.15
		 (("v" 01002740) ONT::AQUIRE) ;; take.16
		 (("v" 00173338) ONT::CAUSE-COME-FROM) ;; take.17
		 (("v" 01156834) ONT::CONSUME) ;; take.18
		 (("v" 00669762) ONT::ACCEPT) ;; take.19
		 (("v" 02209936) ONT::ACCEPT) ;; take.20
		 (("v" 02206619) ONT::CAPTURE) ;; take.21
		 (("v" 01982395) ONT::APPROPRIATE) ;; take.22
		 (("v" 02236624) ONT::ACCEPT) ;; take.23
		 (("v" 00523095) ONT::REGISTER) ;; take.24
		 (("v" 00599992) ONT::SCRUTINY) ;; take.25
		 (("v" 00756076) ONT::ACQUIRE) ;; take.26 ?
		 (("v" 02075857) ONT::MOVE) ;; take.27
		 (("v" 01151110) ONT::ORIENT) ;; take.28
		 (("v" 00557404) ONT::BECOME) ;; take.29
		 (("v" 02717102) ONT::HAVE) ;; take.30
		 (("v" 02208537) ONT::LEASE-HIRE) ;; take.31
		 (("v" 02209745) ONT::ACQUIRE) ;; take.32 ?
		 (("v" 02208118) ONT::SELECT) ;; take.33
		 (("v" 02201138) ONT::ACQUIRE) ;; take.34 ?
		 (("v" 01427127) ONT::ACQUIRE) ;; take.35 ?		 
		 (("v" 00758333) ONT::APPROPRIATE) ;; take.36
		 (("v" 02741546) ONT::IS-COMPATIBLE-WITH) ;; take.37
		 (("v" 02701210) ONT::CONTAINMENT) ;; take.38
		 (("v" 02590253) ONT::START) ;; take.39 ?
		 (("v" 01930482) ONT::MOVE-BY-MEANS) ;; take.40
		 (("v" 01100830) ONT::ACQUIRE) ;; take.41
		 (("v" 00087736) ONT::HAVE-EXPERIENCE) ;; take.42

		 (("n" 13260190) ONT::VALUE-COST) ; change.1
		 (("n" 00908133) ONT::RECORD) ; change.2

		 ;; senses of change
		 (("v" 00123170) ONT::MODIFY) ;; change.3
		 (("v" 00550117) ONT::REPLACEMENT) ;; change.4
		 (("v" 00169458) ONT::REPLACEMENT) ;; change.5
		 (("v" 02257370) ONT::REPLACEMENT) ;; change.7
		 (("v" 02088241) ONT::TRANSFER) ;; change.8
		 (("v" 00551210) ONT::MODIFY) ;; change.9
		 (("v" 00163251) ONT::REPLACEMENT) ;; change.10

		 (("n" 07296428) ONT::CHANGE-STATE) ; change.1
		 (("n" 13859043) ONT::CHANGE-STATE) ; change.2
		 (("n" 11412727) ONT::MODIFY) ; change.4
		 (("n" 13387689) ONT::VALUE-COST) ; change.5
		 (("n" 03005920) ONT::CHANGE) ; change.6
		 (("n" 03005769) ONT::REPLACEMENT) ; change.7
		 (("n" 13388111) ONT::MONEY) ; change.8
		 (("n" 13387479) ONT::MONEY) ; change.9
		 (("n" 04752034) ONT::CHANGE) ; change.10

		 ; alter
;                 (("v" 01667132) ONT::MODIFY) ;; alter.3
;                 (("v" 00201407) ONT::MODIFY) ;; alter.4
;                 (("v" 00060477) ONT::NATURE-CHANGE) ;; alter.5 neuter#1

		 ; leak
;                 (("v" 00937023) ONT::ANNOUNCE) ;; leak#1
;                 (("v" 00936169) ONT::ANNOUNCE) ;; leak#2
;                 (("v" 00529759) ONT::COME-FROM) ;; leak#3
;                 (("v" 00258109) ONT::COME-FROM) ;; leak#4

		 ;; throw
;                 (("v" 01508368) ONT::CAUSE-TO-MOVE) ;; throw#1
;                 (("v" 02097204) ONT::CAUSE-TO-MOVE) ;; throw#2
;                 (("v" 01513430) ONT::DISCARD) ;; throw#3
;                 (("v" 01499265) ONT::CAUSE-TO-MOVE) ;; throw#4
;                 (("v" 01060317) ONT::COMMUNICATION) ;; throw#5
;                 (("v" 01510173) ONT::START-OBJECT) ;; throw#6
;                 (("v" 01632897) ONT::SEND) ;; throw#7 cast#1
;                 (("v" 02097341) ONT::CAUSE-MAKE) ;; throw#8
;                 (("v" 01791232) ONT::EVOKE-CONFUSION) ;; throw#9
;                 (("v" 01067664) ONT::COMMUNICATION) ;; throw#10 THROW-ACCUSATIONS?
;                 (("v" 01733477) ONT::HAVE-EXPERIENCE) ;; throw#11 have a party??
;                 (("v" 01663443) ONT::CAUSE-MAKE) ;; throw#12
;                 (("v" 01527754) ONT::CAUSE-TO-MOVE) ;; throw#13 CAUSE-TO-FALL?
;                 (("v" 01437011) ONT::CAUSE-TO-MOVE) ;; throw#14
;                 (("v" 00621734) ONT::EVOKE-CONFUSION) ;; throw#15

		 ;; keep
		 (("v" 02681795) ONT::MAINTAIN-STATE) ;; keep#1
		 (("v" 02684924) ONT::MAINTAIN-STATE) ;; keep#2
		 (("v" 02202384) ONT::RETAIN) ;; keep#3
		 (("v" 02450505) ONT::HINDERING) ;; keep#4
		 (("v" 02578872) ONT::COMPLIANCE) ;; keep#5
		 (("v" 00732552) ONT::MAINTAIN-STATE) ;; keep#6 maintain#10
		 (("v" 02202928) ONT::MANAGING) ;; keep#7
		 (("v" 01065877) ONT::RECORD) ;; keep#8 maintain#7
		 (("v" 02651853) ONT::HAVE) ;; keep#9 keep boarders
		 (("v" 02410175) ONT::MAINTAIN-STATE) ;; keep#10 retain#2
		 (("v" 01184625) ONT::MAINTAIN-STATE) ;; keep#11 maintain#3
		 (("v" 02734800) ONT::RETAIN) ;; keep#12 stay_fresh#1
		 (("v" 02578510) ONT::COMPLIANCE) ;; keep#13 observe#6
		 (("v" 02422663) ONT::HINDERING) ;; keep#14 restrain#1
		 (("v" 02733122) ONT::PROTECTING) ;; keep#15
		 (("v" 02652016) ONT::HAVE) ;; keep#16 keep bees, chickens
		 (("v" 02283716) ONT::MAINTAIN-STATE) ;; keep#17
		 (("v" 02204094) ONT::CONTAINMENT) ;; keep#18
		 (("v" 02203844) ONT::HAVE) ;; keep#19
		 (("v" 02203168) ONT::MAINTAIN-STATE) ;; keep#20
		 (("v" 01302019) ONT::RETAIN) ;; keep#21
		 (("v" 00212414) ONT::MAINTAIN-STATE) ;; keep#22

 		 ; eliminate
;                 (("v" 00471711) ONT::DISCARD) ;; eliminate#1 get_rid_of#2
;                 (("v" 02629256) ONT::DISCARD) ;; eliminate#2 obviate#1
;                 (("v" 00470701) ONT::DESTROY) ;; eliminate#3 annihilate#1
;                 (("v" 00685419) ONT::REJECT)  ;; eliminate#4 reject#7
;                 (("v" 00072989) ONT::BODILY-PROCESS) ;; eliminate#5 excrete#1
;                 (("v" 01102839) ONT::CAUSE-COME-FROM) ;; eliminate#6
;                 (("v" 00575561) ONT::DISCARD) ;; eliminate#7

		 ; break
;                 (("v" 00364064) ONT::HINDERING) ;; break#1 interrupt#4
;                 (("v" 00334186) ONT::BREAK-OBJECT) ;; break#2
;                 (("v" 00258665) ONT::RENDER-INEFFECTIVE) ;; break#3
;                 (("v" 01369758) ONT::RENDER-INEFFECTIVE) ;; break#4
;                 (("v" 00334996) ONT::BREAK-OBJECT) ;; break#5
;                 (("v" 02566528) ONT::EVOKE-OFFENSE) ;; break#6
;                 (("v" 02073233) ONT::DEPART) ;; break#7
;                 (("v" 02029369) ONT::SEPARATE) ;; break#8
;                 (("v" 01785395) ONT::EXPLODE) ;; break#9
;                 (("v" 00362348) ONT::STOP) ;; break#10
;                 (("v" 02570684) ONT::EVOKE-OFFENSE) ;; break#11
;                 (("v" 00202569) ONT::TRANSFER-INFORMATION) ;; break#12
;                 (("v" 02668523) ONT::EVOKE-OFFENSE) ;; break#13
;                 (("v" 01106864) ONT::IMPROVE) ;; break#14 SURPASS??     
;                 (("v" 00933821) ONT::ANNOUNCE) ;; break#15 disclose#1 reveal#2
;                 (("v" 01106864) ONT::ARRIVE) ;; break#16

		 ; lose
;                 (("v" 02287789) ONT::LOSE) ; lose #1
;                 (("v" 01099592) ONT::LOSE) ; lose #2
;                 (("v" 01795082) ONT::LOSE) ; lose #3
;                 (("v" 01503101) ONT::LOSE) ; lose #4 misplace#1
;                 (("v" 02287618) ONT::LOSE) ; lose #5
;                 (("v" 02197091) ONT::LOSE) ; lose #6
;                 (("v" 02288828) ONT::LOSE) ; lose #7
;                 (("v" 02288155) ONT::LOSE) ; lose #8
;                 (("v" 01113806) ONT::MOVE-AWAY) ; lose #9 recede#2
;                 (("v" 02127853) ONT::ACTION-SALIENCE) ; lose #10 miss#1
;		 (("v" 00204872) ONT::LOSE) ; lose #11

		 ; concrete
;		 (("v" 01600355) ONT::FILLING) ; concrete#1
;		 (("v" 00374534) ONT::COALESCE) ; concrete#2

		 ; expel
;                 (("v" 02501738) ONT::SOCIALLY-REMOVE) ; expel#1, kick_out
;                 (("v" 02401809) ONT::SOCIALLY-REMOVE) ; expel#2, oust
;                 (("v" 01108951) ONT::PUSH-OUT-OF) ; expel#3, rout
;                 (("v" 00104868) ONT::PUSH-OUT-OF) ; expel#4, discharge
;
;                  ; sleep
;                 (("v" 00014742) ONT::BODILY-PROCESS) ; sleep#1
;                 (("v" 02701445) ONT::ACCOMMODATE) ; sleep#2

		 ; carry
;                 (("v" 01449974) ONT::TRANSPORT) ; carry#1 transport#2
;;                (("v" 02717102) ONT::HAVE) ; carry#2 take#30 dupe
;                 (("v" 02079933) ONT::TRANSPORT) ; carry#3 transmit#2 convey#5
;                 (("v" 01061017) ONT::EXPRESS) ; carry#4 express#3 convey#2
;                 (("v" 01218084) ONT::HAVE) ; carry#5 need new type here : carry courses, a mortgage, a project
;                 (("v" 01601234) ONT::BODY-MANIPULATION) ; carry#6 hold#14
;                 (("v" 02700867) ONT::CONTAINMENT) ; carry#7 contain#2
;                 (("v" 02561168) ONT::REACH) ; carry#8 ??
;                 (("v" 00235110) ONT::REACH) ; carry#9 extend#16
;                 (("v" 02636325) ONT::HAVE) ; carry#10
;                 (("v" 01101416) ONT::ACQUIRE) ; carry#11
;                 (("v" 00738951) ONT::HAVE) ; carry#12

		 ; go
		 ; enter
;                 (("v" 02016523) ONT::HAVE) ; enter#1
;                 (("v" 01082606) ONT::HAVE) ; enter#2
;                 (("v" 02471327) ONT::HAVE) ; enter#3
;                 (("v" 02722207) ONT::HAVE) ; enter#4
;                 (("v" 01000214) ONT::HAVE) ; enter#5
;                 (("v" 01720660) ONT::HAVE) ; enter#6
;                 (("v" 02381397) ONT::HAVE) ; enter#7
;                 (("v" 01421622) ONT::HAVE) ; enter#8
;                 (("v" 00348252) ONT::HAVE) ; enter#9
	                                  
		 ; release
		 ; burst
		 ; set
		 ; start
		 ; survive
		 ; form
		 ; know
		 ; explain
		 ; enter
		 ; exit
		 ; understand
		 
                 ))
  (setf (gethash (car pairs) hierarchy-mapping) (cadr pairs)))
|#

(defun convert-hierarchy (hier)
  "Given a hierarchy (a list of synsets), returns an ontology type for the most specific matching synset.  It tries to find the closest match first, but will eventually settle for the part of speech defaults."
  ; we work in reverse so we start with the most specific and work our way 
  ; upwards
  (let* ((hier-rev (reverse hier))
         (result (car (convert-hierarchy-helper hier-rev))))
    (print-debug "hierarchy conversion result for ~S is ~S~%" hier-rev result)
        (or result
            ; if we can't find matches for a word, we pick a default based on
            ; the part of speech.
	    ; (see also process-synsets, where these are pushed to the end of
	    ; the list and thus scored lower)
            (case (slot-value (car hier-rev) 'ss-type)
                  (|n| 'ONT::ANY-SEM) ; noun
                  (|v| 'ONT::SITUATION-ROOT) ; verb
                  (|a| 'ONT::MODIFIER) ; adjective
                  (|s| 'ONT::MODIFIER) ; adjective satellite
                  (|r| 'ONT::PREDICATE) ; adverb
                  (otherwise (print-debug "WordFinder -- unknown part of speech returned!"))))))

(defun convert-hierarchy-helper (hier)
  "Recursive helper for convert-hierarchy.  Tries to convert the head element of the hierarchy list or the rest of the list."
   (if hier (print-debug "converting ~S in hierarchy ~S~%" (car hier) hier))
  (if hier
      (or (synset-to-ont-type (car hier)) ; convert this synset
          (let ((head (get-head-adjective wm (car hier)))) ; or try the head adj
	    (when head
	      (synset-to-ont-type head)))
          (convert-hierarchy-helper (cdr hier))))) ; or return the rest

(defvar wordnet-synset-to-ont-type-mappings (make-hash-table :test #'equalp))

(defun make-synset-to-ont-type-table ()
  (mapcar #'(lambda (ont-type)
	      (let ((sense-keys (om::ont-type-wordnet-sense-keys ont-type)))
		(when sense-keys
		  (dolist (sense-key sense-keys)
		    (setf (gethash sense-key wordnet-synset-to-ont-type-mappings) ont-type)))))
	  (om::get-all-ont-types)))

(defun ont-type-for-wordnet-sense-key (sense-key)
  (when sense-key
    (print-debug "getting wn mapping in trips for ~S~%" sense-key)
    (or (gethash sense-key wordnet-synset-to-ont-type-mappings)
	(gethash (concatenate 'string sense-key "::") wordnet-synset-to-ont-type-mappings) ;; try adding final ::
	(gethash (subseq sense-key 0 (- (length sense-key) 2))  wordnet-synset-to-ont-type-mappings)) ;; try removing final ::
  ))

			    
(defun synset-to-ont-type (synset)
  "Return ont-type for this synset, or NIL if none exists"
  (let ((sense-keys (sense-keys-for-synset synset)))
    (remove-duplicates (remove-if #'null (mapcar (lambda (sense-key)
				(ont-type-for-wordnet-sense-key sense-key)
				)
			      sense-keys)))
      )
  )

; hierarchy-mapping no longer used
;(defun synset-to-logical-form (synset)
;  "Returns a logical form associated with the synset, if there is one.  Otherwise, returns nil."
;  (with-slots (ss-type offset) synset
;    (gethash (list (string ss-type)
;                   offset)
;             hierarchy-mapping)))

;;;
;;; Report generation functions
;;; These aren't actually used in the operation of wordnet-to-trips, but are
;;; used to produce a report about the mappings between synsets and logical
;;; forms.
;;;

(defun wn-to-trips-to-wn-sense-keys (synset-location ont-type)
  (let* ((pos (pos-sym-to-string (car synset-location)))
         (synset (get-synset wm pos (cadr synset-location)))
	 (sense-keys (sense-keys-for-synset synset))
	 )
 	(om::add-ont-type-wordnet-sense-keys ont-type sense-keys)
  ))

; hierarchy-mapping no longer used. mappings are in the ontology type definitions
;(defun wn-trips-mappings-to-sense-keys ()
;  "Helping function for moving the WF hierarchy mappings to the ontology types. For each mapping in the wf hierarchy-mapping table, add it to its corresponding ontology type slot and print the results"
;  (maphash #'wn-to-trips-to-wn-sense-keys hierarchy-mapping)
;  (om::show-ont-type-wordnet-sense-keys)
;  )

;(defun describe-mapping-values (synset-location logical-form)
;  (let* ((pos (pos-sym-to-string (car synset-location)))
;         (synset (get-synset wm pos (cadr synset-location))))
;        (format t "~A: ~A ~A ~A~%" 
;                logical-form
;                (get-synonyms synset)
;                pos
;                (slot-value synset 'glosses))))

;(defun describe-mappings ()
 ; "Print out a list of each logical form and the synset that we map to it.  This is used to help determine whether the mappings are correct.  To run this, make sure you've loaded wn.lisp and run setup-wordnet first."
 ; (maphash #'describe-mapping-values hierarchy-mapping))

;; add new mappings to the hierarchy 
;(defun add-mapping-to-hierarchy (new-mapping)
;  (setf (gethash (car new-mapping) hierarchy-mapping) (cadr new-mapping)))

(defun make-ontology-mapping-for-sense-key (sense-key ont-type)
  (let* ((synset (get-synset-from-sense-key wm sense-key))
	 (ss-type (string (get-ss-type synset)))
	 (offset (get-offset synset))
	 (new-mapping (list (list ss-type offset) ont-type)))
   new-mapping)
  )

(defun add-sense-key-to-ont-type-mapping (sense-key ont-type)
  "add a wordnet sense key mapping directly to the hash table. Used in Gloss, where sense-keys are passed in from tagged text."
  (setf (gethash sense-key wordnet-synset-to-ont-type-mappings) ont-type)
  )

;; called from Systems/gloss/test.lisp for boostrapping wordnet-synset to trips ontology type mappings
(defun add-ontology-mapping-for-sense-key (sense-key ont-types)
  (print-debug "checking mappings for ~S and ~S~%" sense-key ont-types)
  (dolist (ont-type ont-types)
    (let (;(new-mapping (make-ontology-mapping-for-sense-key sense-key ont-type))
	  (current-type (car (best-ont-type-for-sense-key sense-key)))
	  )
      (if current-type ;; check for a more specific mapping exists already
	  (when (and (not (equal current-type ont-type))
		     (equal (om::more-specific current-type ont-type) ont-type))
;	    (add-mapping-to-hierarchy new-mapping) ; old hierarchy-mapping no longer used
	    (add-sense-key-to-ont-type-mapping sense-key ont-type)
	    (print-debug "new mapping added for ~S for type ~S~%" sense-key ont-type)
	    ))
      ))
    )

(defun get-synsets-for-word (w &key (pos nil))
  (let* ((poss (if pos (list pos) (slot-value wm 'parts-of-speech))))
    (mapcar
      (lambda (synset)
	(with-slots (ss-type words offset) synset
	  (list ss-type
	    (mapcar #'(lambda (w)
		       (slot-value w 'word)) words)
	    offset
	    )
	  )
	)
      (mapcan (lambda (p) (get-synsets wm w p)) poss)
      )
    )
  )

(defun get-synset-for-word (w &key (pos nil))
  (let* ((poss (if pos (list pos) (slot-value wm 'parts-of-speech))))
    (remove-duplicates
      (mapcan
	(lambda (synset)
	  (with-slots (ss-type words) synset
	    (mapcar #'(lambda (w)
;		       (list ss-type (slot-value w 'word))) words)
		       (list (convert-wordnet-pos-to-trips (pos-sym-to-string ss-type)) (read-from-string (format nil "W::~a" (slot-value w 'word))))) words)
	    )
	  )
	(mapcan (lambda (p) (get-synsets wm w p)) poss)
	)
      :test #'equalp
      )
    )
  )

;;;
;;; TRIPS/WN synset matching code
;;; Computes neighborhoods of words for synsets in each system by following
;;; parent/child links, and scores matches between neighborhoods by matching 
;;; words weighted by how close they are to the center of the neighborhood.
;;;

;; neighborhood generation parameters
(defvar parent-weight 0.5)
(defvar parent-limit 2)
(defvar child-weight 0.5)
(defvar child-limit 2)
; weight of words in the exact class is parent-weight + child-weight (1)

;; will somebody please think of the children?!?
(defmethod get-synset-children ((this wordnet-manager) (synset wordnet-synset))
  "Returns a list of the children (hyponyms) of a synset."
  (mapcar #'cadr
    (append (get-pointers-by-relationship this synset "~")
            (get-pointers-by-relationship this synset "~i"))))

;; this is a modified version that gets instance hypernyms as well
(defmethod get-synset-parents ((this wordnet-manager) (synset wordnet-synset))
  "Returns a list of the parents (hypernyms) of a synset."
  (mapcar #'cadr
    (append (get-pointers-by-relationship this synset "@")
            (get-pointers-by-relationship this synset "@i"))))

;;; TRIPS neighborhood functions

(defun convert-word-to-string (word)
  "Converts a TRIPS-style word (which may be a list) to a lowercase, space-separated string."
  (remove-if
    (lambda (c)
      (or (eql c #\() (eql c #\)))
      )
    (string-downcase (format nil "~a" word))
    )
  )

(defun weight-lf-words (lf hash pos weight)
  "Adds weight in hash to the words in lf with part-of-speech pos."
  (dolist (word (mapcar
	          (lambda (w) (list (convert-word-to-string (car w)) (cadr w)))
	          (lxm::get-words-from-lf lf)))
    (when (eql pos (cadr word))
      (if (gethash (car word) hash)
        (setf (gethash (car word) hash) (+ weight (gethash (car word) hash)))
	(setf (gethash (car word) hash) weight)
	)
      )
    )
  )

(defun get-lf-ancestor-weights (lf hash pos &optional (limit parent-limit) (weight parent-weight))
  "Weights words in ancestors of lf with diminishing weights as they get farther away."
  (weight-lf-words lf hash pos weight)
  (when (> limit 0)
    (let ((parent (om::get-parent lf)))
      (when parent
        (get-lf-ancestor-weights parent hash pos (- limit 1) (* weight parent-weight))
	)
      )
    )
  )

(defun get-lf-descendant-weights (lf hash pos &optional (limit child-limit) (weight child-weight))
  "Weights words in descendants of lf with diminishing weights as they get farther away."
  (weight-lf-words lf hash pos weight)
  (when (> limit 0)
    (dolist (child (om::get-children lf))
      (get-lf-descendant-weights child hash pos (- limit 1) (* weight child-weight))
      )
    )
  )

(defvar lf-neighborhood-cache (make-hash-table :test 'equal))

(defun get-lf-neighborhood (lf pos)
  "Returns a hash mapping words with part-of-speech pos to weights representing how close to lf the word is"
  (let ((hashkey (list lf pos)))
    (or
      (gethash hashkey lf-neighborhood-cache)
      (setf (gethash hashkey lf-neighborhood-cache)
	(let ((hash (make-hash-table :test 'equal)))
	  (get-lf-ancestor-weights lf hash pos)
	  (get-lf-descendant-weights lf hash pos)
	  hash
	  )
	)
      )
    )
  )

(defun cache-all-lf-neighborhoods ()
  (maphash
    (lambda (lf description)
      (declare (ignore description)) ;; for now
      (dolist (pos '(w::n w::v w::adj w::adv))
        (get-lf-neighborhood lf pos)
        )
      )
    (om::ling-ontology-lf-table om::*lf-ontology*)
    )
  nil
  )

;;; WordNet neighborhood functions

(defun weight-synset-words (synset hash weight)
  "Adds weight in hash to the words in synset."
  (dolist (word (get-synonyms synset))
    (if (gethash word hash)
      (setf (gethash word hash) (+ weight (gethash word hash)))
      (setf (gethash word hash) weight)
      )
    )
  )

(defun get-synset-ancestor-weights (synset hash &optional (limit parent-limit) (weight parent-weight))
  "Weights words in ancestors of synset with diminishing weights as they get farther away."
  (weight-synset-words synset hash weight)
  (when (> limit 0)
    (dolist (parent (get-synset-parents wm synset))
      (get-synset-ancestor-weights parent hash (- limit 1) (* weight parent-weight))
      )
    )
  )

(defun get-synset-descendant-weights (synset hash &optional (limit child-limit) (weight child-weight))
  "Weights words in descendants of synset with diminishing weights as they get farther away."
  (weight-synset-words synset hash weight)
  (when (> limit 0)
    (dolist (child (get-synset-children wm synset))
      (get-synset-descendant-weights child hash (- limit 1) (* weight parent-weight)) 
      )
    )
  )

(defun get-synset-neighborhood (synset)
  "Returns a hash mapping words to weights representing how close to synset the word is"
  (let ((hash (make-hash-table :test 'equal)))
    (get-synset-ancestor-weights synset hash)
    (get-synset-descendant-weights synset hash)
    hash
    )
  )

;;; now we put the two together...

(defun match-neighborhoods (n1 n2)
  "Returns a score of how well n1 and n2 match. The score is the dot-product of the two word score hashes viewed as vectors indexed by word."
  (let ((score 0))
    (maphash
      (lambda (word n1-score)
        (when (gethash word n2)
	  (setf score (+ score (* n1-score (gethash word n2))))
	  )
	)
      n1
      )
    score
    ) 
  )

;;; WN -> TRIPS
(defun best-ont-type-for-sense-key (sense-key)
  (let* ((*package* *wf-package-var*)
         (*parent-offset-list* nil) ;; reset parent offset list used in hierarchy search
	 (ss (get-synset-from-sense-key wm sense-key))
	 )
    (when ss
      (remove-duplicates (mapcar #'convert-hierarchy (get-hierarchy wm ss))))))

(defun get-best-lf-for-synset (synset)
  "Searches all LFs in TRIPS and finds the one whose neighborhood matches the neighborhood of synset with the highest score. Returns the best lf and the pos of the synset"
  (let ((best-lf nil)
	(best-score -1)
	(synset-neighborhood (get-synset-neighborhood synset))
	(pos (convert-wordnet-pos-to-trips (get-pos-string synset))))
    (maphash
      (lambda (lf description)
	(declare (ignore description)) ;; for now
	(let* ((lf-neighborhood (get-lf-neighborhood lf pos))
	       (score (match-neighborhoods lf-neighborhood synset-neighborhood)))
	  (when (> score best-score)
	    (setf best-lf lf)
	    (setf best-score score)
	    )
	  )
	)
      (om::ling-ontology-lf-table om::*lf-ontology*)
      )
    (list best-lf pos best-score)
    )
  )

(defun get-best-lfs-for-wn-word (word)
  "Returns a list of (lf pos) lists that best match the senses of word in wordnet."
  (remove-if #'null
    (remove-duplicates
      (mapcar #'get-best-lf-for-synset (get-all-synsets wm (substitute #\_ #\Space word)))
      :test #'equal
      )
    )
  )

;;; TRIPS -> WN
;;; this is so we can put sense-keys in the :wn field of trips vocabulary meta-data

(defun get-numerical-ss-type (ss-type)
  (cdr (assoc (if (symbolp ss-type) (symbol-name ss-type) ss-type)
	      '(("n" . 1) ("noun" . 1)
	        ("v" . 2) ("verb" . 2)
	        ("a" . 3) ("adj" . 3)
	        ("r" . 4) ("adv" . 4)
	        ("s" . 5)
	        )
	      :test #'string-equal
	      ))
  )

(defun remove-syntactic-marker-from-adj (str)
  "Remove the syntactic marker (in parens) from the end of an adjective if it's
   there."
  (let ((open-paren (position #\( str :test #'char=)))
    (if (and open-paren (char= #\) (elt str (- (length str) 1))))
      (subseq str 0 open-paren)
      str
      )))

(defmethod get-sense-key ((this wordnet-synset) word)
  "inverse of get-synset-from-sense-key"
  (with-slots (ss-type lex-filenum words) this
    (let* ((word-no-sm (remove-syntactic-marker-from-adj (string-downcase word)))
           (lex-id
            (slot-value
	      (find-if
	        (lambda (e)
		  (equalp word-no-sm
		      (remove-syntactic-marker-from-adj
		          (string-downcase (slot-value e 'word)))))
		words)
	      'lex-id
	      )
	    )
           (num-ss-type (get-numerical-ss-type ss-type))
           )
      (if (= 5 num-ss-type)
        ;; satellite adjective
        (let* ((head-ss (cadar (get-pointers-by-relationship wm this "&")))
	       (head-word (first (slot-value head-ss 'words)))
	       (head-str (remove-syntactic-marker-from-adj (string-downcase (slot-value head-word 'word))))
	       (head-id (slot-value head-word 'lex-id))
	       )
	  (format nil "~a%~d:~2,'0d:~2,'0d:~a:~2,'0d"
		  word-no-sm num-ss-type lex-filenum lex-id
		  head-str head-id)
	  )
	;; not a satellite adjective
        (format nil "~a%~d:~2,'0d:~2,'0d::"
	        word-no-sm num-ss-type lex-filenum lex-id)
	)
      )
    )
  )

(defun get-best-synset-for-trips-sense (word pos lf)
  "Almost the inverse of get-best-lfs-for-wn-word, except that instead of getting all the senses for a word, we get the best match for one sense."
  (let ((best-synset nil)
        (best-score -1)
        (lf-neighborhood (get-lf-neighborhood lf pos)))
    (dolist (synset
	      (remove-if-not
		(lambda (synset)
		  (eql pos (convert-wordnet-pos-to-trips (get-pos-string synset)))
		  )
		(get-all-synsets wm (substitute #\_ #\Space (convert-word-to-string word)))
		)
	      )
      (let* ((synset-neighborhood (get-synset-neighborhood synset))
	     (score (match-neighborhoods lf-neighborhood synset-neighborhood)))
	(when (> score best-score)
	  (setf best-synset synset)
	  (setf best-score score)
	  )
	)
      )
    best-synset
    )
  )

;;;; end of TRIPS/WN synset matching code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; for GLOSS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun underscored-multi-string (wordlist &key (hyphens :as-is))
  (substitute #\_ #\Space (multi-string wordlist :hyphens hyphens)))

(defun get-sense-key-for-word-and-synset (word synset pos)
    (declare (ignore pos)) ; ?
  (let ((sense-key (car (sense-key-for-word-and-synset word synset))))
    (when (not sense-key)
      (let ((base-forms (car (run-morphy wm word))))
	(when base-forms
	  (dolist (form (second base-forms))
	    (let ((base-sense-key (car (sense-key-for-word-and-synset form synset))))
	      (when base-sense-key (setq sense-key base-sense-key)))))
	))
    sense-key)
  )


;; Note: This is apparently used in more than just gloss now. If you change the
;; convert-wordnet-entries pathway, you should also make the corresponding
;; change here if it applies.
(defun wordnet-sense-keys-for-word (wordlist keys)
  (let* ((trips-pos-list (or (util::find-arg keys :trips-parts-of-speech) (util::find-arg keys :tag)))
	 (penn-tags (util::convert-to-package (util::find-arg keys :penn-parts-of-speech) :w))
	 word-str synsets
	 non-core-senses core-senses)
    (setf synsets
	  (or ;; take the first of these that succeeds (see also lookup-unknown-word)
	    ;; first try the word as-is
	    (progn
	      (setf word-str (underscored-multi-string wordlist))
	      (get-all-synsets wm word-str :use-morphy t :use-stoplist t))
	    ;; if it has hyphens, try with spaces instead
	    (when (wordlist-has-hyphens wordlist)
	      (setf word-str (underscored-multi-string wordlist :hyphens :none))
	      (get-all-synsets wm word-str :use-morphy t :use-stoplist t))
	    ;; if it has spaces, try with hyphens instead
	    (when (wordlist-has-spaces wordlist)
	      (setf word-str (multi-string wordlist :hyphens :all))
	      (get-all-synsets wm word-str :use-morphy t :use-stoplist t))
	    ))
    (print-debug "poslists are ~S and ~S synsets are ~S~%" trips-pos-list penn-tags synsets)
    (dolist (synset synsets)
      (let* ((this-pos (convert-wordnet-pos-to-trips (get-pos-string synset)))
	     (this-sense-key (get-sense-key-for-word-and-synset word-str synset this-pos))
	     (this-ont-type (when this-sense-key (car (best-ont-type-for-sense-key this-sense-key))))
	    )
	(when this-sense-key
	  (if (is-core-wordnet-sense this-sense-key)
	      (pushnew (list this-sense-key this-pos this-ont-type) core-senses)
	    (pushnew (list this-sense-key this-pos this-ont-type) non-core-senses)))))
    (values (list :core (reverse core-senses)) (list :non-core (reverse non-core-senses))))
  )

(defun test-synsets (w)
  (setq *parent-offset-list* nil) ;; reset parent offset list used in hierarchy search
  (let* (
	 (w (util::convert-to-package (multi-string w) :w))
	 (synsets (get-all-synsets wm w :use-morphy t))
	 ;non-core-senses core-senses
	 )
    (format t "synsets are ~S~%" synsets)
    (dolist (synset synsets)
      (format t "synset is ~S~%" synset)
      (format t "sense keys are ~S~%" (sense-keys-for-synset synset))
 ;     (format t "pos is ~S~%" (conv(get-pos-string synset))
 ;     (format t "sense key is ~S~%" (get-sense-key-for-word-and-synset w synset (convert-wordnet-pos-to-trips (get-pos-string synset))))

      )))

(defmethod least-common-ancestors ((this wordnet-manager) (ss1 wordnet-synset) (ss2 wordnet-synset))
  "Return a maximal list of wordnet-synsets such that every element is a
   hypernym ancestor of both ss1 and ss2, and no element of the list is an
   ancestor of another element in the list. ss1 or ss2 may appear in the list
   if one is the other's ancestor, or if they are the same synset.
   "
  (let* ((paths1 (let ((*parent-offset-list* nil)) (get-hierarchy this ss1)))
	 (paths2 (let ((*parent-offset-list* nil)) (get-hierarchy this ss2)))
	 (paths (append paths1 paths2))
         (ancs1 (remove-duplicates (apply #'append paths1)
				   :test #'synsets-equal-p))
         (ancs2 (remove-duplicates (apply #'append paths2)
				   :test #'synsets-equal-p))
	 (common-ancs (intersection ancs1 ancs2 :test #'synsets-equal-p))
	 least-common-ancs
	 )
    (dolist (path paths)
      (let ((lca
              (find-if
	        (lambda (anc) (member anc common-ancs :test #'synsets-equal-p))
	        path
		:from-end t)))
        (when lca
	  (pushnew lca least-common-ancs :test #'synsets-equal-p))))
    least-common-ancs))

(defmethod least-common-ancestors ((this wordnet-manager) (sense-key-1 string) (sense-key-2 string))
  "Wrapper for (least-common-ancestors wm wordnet-synset wordnet-synset) (see
   above) that translates from sense keys to wordnet-synsets and back.

   Examples:
   (least-common-ancestors wm \"man%1:18:00\" \"woman%1:18:00\")
    => (\"person%1:03:00\")
   (least-common-ancestors wm \"man%1:18:00\" \"person%1:03:00\")
    => (\"person%1:03:00\")
   (least-common-ancestors wm \"man%1:18:00\" \"man%1:18:00\")
    => (\"man%1:18:00\")
   (least-common-ancestors wm \"man%1:18:00\" \"idea%1:09:00\")
    => (\"entity%1:03:00\")
   (least-common-ancestors wm \"man%1:18:00\" \"handle%2:35:00\")
    => NIL
   (least-common-ancestors wm \"respiratory_rate%1:28:00\" \"heart_rate%1:28:00\")
    => (\"rate%1:28:00\" \"vital_sign%1:26:00\")
   "
  (let* ((ss1 (get-synset-from-sense-key this sense-key-1))
	 (ss2 (get-synset-from-sense-key this sense-key-2))
	 (lcas (least-common-ancestors this ss1 ss2)))
    (mapcar
      (lambda (ss)
        (get-sense-key ss (slot-value (first (slot-value ss 'words)) 'word)))
      lcas
      )))

