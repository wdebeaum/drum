(in-package :wordfinder)
; This is the main file for WordFinder.  It asks the databases for their
; guesses on meanings for words, then combines them.  If both WordNet
; and COMLEX entries are present, they will be combined.
; See combine-wordnet-and-comlex-entries for details on that.

; These are keywords that are combined when we combine WordNet and COMLEX
; entries.
(defvar *keywords-to-merge* '(:LFS :LIKE :FEATURES :COMLEX-SUBCATS 
                              :WORDNET-VERB-FRAME :ROOT :FORM))

(defvar *wf-package-var* (find-package :wordfinder))

(defun wordlist-has-hyphens (wordlist)
  (and (listp wordlist)
       (member 'W::punc-minus wordlist)))

(defun wordlist-has-spaces (wordlist)
  (and (listp wordlist)
       (loop with prev = 'W::punc-minus
	     for word in wordlist
	     do
	       (unless (member 'W::punc-minus (list prev word))
		 (return-from wordlist-has-spaces t))
	       (setf prev word)
	     finally (return nil))))

(defun multi-string (wordlist &key (hyphens :as-is))
  "Convert a TRIPS word list to a single downcased string. The :hyphens
   argument may have one of three values:
   :as-is (the default) - Turn W::punc-minus into '-' and put spaces between
			  words where there isn't a hyphen.
   :all - Put hyphens between all the words, ignoring W::punc-minus.
   :none - Put spaces between all the words, ignoring W::punc-minus.
   When given a single word instead of a list, just convert it to a string.
   "
  (unless (listp wordlist)
    (return-from multi-string (string-downcase (string wordlist))))
  (loop with strs = nil
        with prev-was-word = nil
        for word in wordlist
	do
	  (when prev-was-word
	    (push
	      (ecase hyphens
		(:as-is (if (eq word 'W::punc-minus) "-" " "))
		(:all "-")
		(:none " ")
		)
	      strs))
	  (cond
	    ((eq word 'W::punc-minus)
	      (setf prev-was-word nil))
	    (t
	      (setf prev-was-word t)
	      (push
	        (if (numberp word)
		  (princ-to-string word)
		  (string-downcase (string word)))
		strs)
	      )
	    )
	finally (return (apply #'concatenate (cons 'string (nreverse strs))))))

(defun lookup-unknown-word (wordlist &key senselimit poslist penntag trips-sense-list wn-sense-keys)
  "Returns the content of the message to be sent in reply to an 
  UNKNOWN-WORD request."
  (let ((word-str (multi-string wordlist)) entries)
    ;; give up early if we don't have WordNet at all
    (unless *wordnet-basepath*
      (return-from lookup-unknown-word `(failure :word ,word-str)))
    (setf entries
      (or ;; take the first of these that succeeds
        ;; first try the word as-is
	(convert-wordnet-entries word-str
	    :senselimit senselimit :poslist poslist :penntags penntag
	    :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys)
	;; if it has hyphens, try with spaces instead
	;; e.g. "prime-time" -> "prime time"
	(when (wordlist-has-hyphens wordlist)
	  (convert-wordnet-entries (multi-string wordlist :hyphens :none)
	      :senselimit senselimit :poslist poslist :penntags penntag
	      :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys))
	;; if it has spaces, try with hyphens instead
	;; e.g. "ad lib" -> "ad-lib"
	(when (wordlist-has-spaces wordlist)
	  (convert-wordnet-entries (multi-string wordlist :hyphens :all)
	      :senselimit senselimit :poslist poslist :penntags penntag
	      :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys))
        ))
    ;; If we have any entries, we return success with all the entries
    ;; otherwise, we indicate that the lookup failed.
    (if entries
      `(success :entries ,entries)
      `(failure :word ,word-str))
    ))

#| old version with commented out COMLEX and CMU names stuff
(defun lookup-unknown-word (word &key senselimit poslist penntag trips-sense-list wn-sense-keys)
  "Returns the content of the message to be sent in reply to an 
  UNKNOWN-WORD request."
  (let* ((word (if (listp word) (multi-string word)
		(string-downcase (string word))))
        (entries '()) ; this will be the master list that we pass back to the parser
;        (comlex-entries (and
;			 *comlex-filename*
;			 (convert-comlex-entries word)))
	;; phasing out the comlex lookup
	(comlex-entries nil)
        (wordnet-entries (and
			  *wordnet-basepath*
			  (convert-wordnet-entries word :senselimit senselimit :poslist poslist :penntags penntag :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys)))
	;; not currently using this since we rely on NER -- no longer loaded from defsys.
 ;       (cmuname-entries (and
;			  *cmu-names-filename*
;			  (convert-cmunames-entries word)))
	)

     ;  (if cmuname-entries
     ;      (setf entries (append cmuname-entries entries)))

       ; if we have COMLEX and WordNet entries, we should combine them
       ; where appropriate, while returning the uncombined 
       (if (and comlex-entries wordnet-entries)
           (setf entries (append (combine-wordnet-and-comlex-entries 
                                  wordnet-entries
				  comlex-entries
				  )
                                 entries))
           ; otherwise, just return whatever we got
           (setf entries (append wordnet-entries comlex-entries entries)))

           ; (progn 
            ; (if comlex-entries
                ; (setf entries (append comlex-entries entries)))
            ; (if wordnet-entries
                ; (setf entries (append wordnet-entries entries)))))
       
       ; If we have any entries, we return success with all the entries
       ; otherwise, we indicate that the lookup failed.
       (if entries
           (list 'success :entries entries)
           (list 'failure :word word))))
|#

;; to get the packages right -- need to fix 
(defun lookup-unknown-word-string (w &key senselimit poslist penntag trips-sense-list wn-sense-keys)
  (let ((*package* *wf-package-var*))
    (format nil "~S" (lookup-unknown-word w :senselimit senselimit :poslist poslist :penntag penntag :trips-sense-list trips-sense-list :wn-sense-keys wn-sense-keys))
    )
  )

; build-entry creates entries to send to return to the lxm, which sends them on to the parser.
; Most arguments are optional.  
; word is a string describing the entry.  
; constit-type is the part of speech.
; :root and :form are for derived words.  For example:
;   (build-entry "abaci" :constit-type 'N :root "abacus" :form :plural
;                :source :comlex)
; :root specifies the root form and :form specifies the relation between the
; root form and the given the form.
; :score: Different sources give different scores in an attempt to rate the sources.
; :lfs is a list of guesses for a TRIPS logical form, if available.
; :like is a list of synonyms (strings)
; :features and :comlex-subcats come from COMLEX (though WordNet will
; eventually contribute to the :features list).
; :features is a list of TRIPS features.
; :comlex-subcats is a list of COMLEX subcategorizations.
; :wordnet-verb-frame is a list of verb frame numbers for a WordNet verb.
; See http://www.cogsci.princeton.edu/~wn/man1.7.1/wninput.5WN.html for a l
; description of verb frame numbers.
; The plan is for :comlex-subcats and :wordnet-verb-frames to one day be
; turned into subcategorization information for the parser.
(defun build-entry (word &key constit-type root form score lfs like 
                    features comlex-subcats wordnet-verb-frame 
		    domain-info
                    (source :unknown))
  "Creates an entry to send back to the parser.  See lookup.lisp for a full description of all parameters."
  (let ((entry (list 'ENTRY :word word :score (or score .99) :lfs lfs :like like 
                     :features features
                     :comlex-subcats comlex-subcats
                     :wordnet-verb-frame wordnet-verb-frame
		     :domain-info domain-info
                     :source source)))
       (if constit-type
           (setf entry (append entry (list :constit (list '% constit-type)))))
       (if (and root form)
           (setf entry (append entry (list :root root :form form))))
       (print-debug "built entry ~S~%" entry)
       entry))

#| old COMLEX-related stuff
; Assumption: We will get at most one COMLEX entry per part of speech.
; Thus, all we need to do is to combine each COMLEX entry for a part of
; speech with all the WordNet entries for the same part of speech.
; We accomplish this by hashing the COMLEX entries by part of speech,
; then iterating through the WordNet entries.
(defun combine-wordnet-and-comlex-entries (wordnet-entries comlex-entries)
  "Given a list of all COMLEX entries and all WordNet entries, returns a list of combined WordNet and COMLEX entries *and* the original uncombined WordNet and COMLEX entries at a lower score."
  (let ((comlex-pos (make-hash-table :test #'equal))
        (combined-entries '()))
       ; build POS to COMLEX entry table
       (dolist (comlex-entry comlex-entries)
               (setf (gethash (get-keyword-arg comlex-entry :CONSTIT) 
                              comlex-pos)
                     comlex-entry))
       ; now iterate over WordNet entries, and match them up with COMLEX
       ; entries in the hash table (if necessary)
       (setf combined-entries
             	(remove-if #'null (mapcar (lambda (wordnet-entry)
                             (combine-wordnet-and-comlex-entry wordnet-entry
                                                               comlex-pos))
                     wordnet-entries)))

       ; even if we don't make any combinations, (which seems very unlikely --
       ; WordNet and COMLEX would have to not agree on the part of speech)
       ; we return the originals at a lower score.
       (print-debug "wordnet entries are ~S~%" wordnet-entries)
       (print-debug "comlex entries are ~S~%" comlex-entries)
       (print-debug "combined entries are ~S~%" combined-entries)
       (if combined-entries (append (apply #'append combined-entries) 
				    (set-score comlex-entries 0.96)
				    (set-score wordnet-entries 0.96)
				    )
	 (append comlex-entries wordnet-entries))))

(defun set-score (entries new-score)
  "Given a list of entries and a new score for them, returns a list of modified entries with the :score field set to the new score."
  (mapcar (lambda (entry)
                  (set-keyword-arg entry :score new-score))
          entries))

(defun combine-wordnet-and-comlex-entry (wordnet-entry comlex-pos-hash)
  "If wordnet-entry can be combined with one of the entries in the COMLEX part of speech hash table, return it.  Otherwise, return nil.  The WordNet entry will only be matched with a COMLEX entry in the comlex-pos-hash if they have the exact same :CONSTIT information."
  (let ((comlex-entry (gethash (get-keyword-arg wordnet-entry :CONSTIT)
                               comlex-pos-hash)))
       (if comlex-entry
           (merge-keywords wordnet-entry comlex-entry))))

; This is mainly intended for merging COMLEX and WordNet entries together,
; and makes several assumptions that may only apply to the situation of
; combining the two.
(defun merge-keywords (entry1 entry2 &key (keywords *keywords-to-merge*)
                       (new-source :comlex-and-wordnet))
  "Given two entries, returns a new entry with some keywords merged.  The list of keywords to merge is specified in the :keywords keyword, or *keywords-to-merge* if :keywords is omitted.  Keywords are merged by combining lists or by ORing values together."
  ; We assume that entry1 and entry2 refer to the same word and that they
  ; have the same CONSTIT information.
  (let ((build-args (list (get-keyword-arg entry1 :word)
                          :constit-type (cadr (get-keyword-arg entry1 :constit))
                          :score (get-keyword-arg entry1 :score) ;; retain WN sense ranking
                          :source new-source)))
       (dolist (kwd keywords)
               (let ((val1 (get-keyword-arg entry1 kwd))
                     (val2 (get-keyword-arg entry2 kwd))
                     (combined-val nil))
                    (if (listp val1) ; if one is a list, we assume both will be
                                     ; lists and our new value is the combined
                                     ; lists.  otherwise, we assume that only
                                     ; one will have the keyword (which is the
                                     ; case as it turns out) and we OR them
                                     ; together.
                        (setf combined-val (append val1 val2))
		      (setf combined-val (or val1 val2)))
                    (setf build-args (append build-args (list kwd 
                                                              combined-val)))))
       (setf build-args (append build-args (list :source new-source)))
       (list (apply #'build-entry build-args))))

;;;
;;; Keyword utility functions
;;;

(defun set-keyword-arg (lst kwd val)
  "Sets a keyword argument in a list of keywords to a new value.  Returns the value if successful and nil otherwise."
  (let ((pos (position kwd lst)))
       (if pos
           (progn
            (setf (nth (1+ (position kwd lst)) lst) val)
            lst))))
          
; From Paul Graham's "ANSI Common Lisp"
(defun filter (fn lst)
  "Apply fn to each element in lst.  If fn returns a non-nil value, place that element in a new list.  Returns a list of those elements."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
           (if val (push val acc))))
    (nreverse acc)))

; Unused
(defun get-all-keywords (lst)
  "Returns all keywords in list."
  (filter (lambda (lst)
                  (if (keywordp lst)
                      lst))
          lst))
|#

(defun lookup-test-word (word)
  "dummy function for interface development purposes to return wordfinder results without actually calling wordnet, comlex or cmu list of names so those resources don't need to be available"
(let ((res nil))
  (case word
  (w::frog 
   (setq res '(SUCCESS :ENTRIES
 ((ENTRY :WORD "frog" :score 1 :LFS (ONT::PHYS-OBJECT) :LIKE
   ("toad" "toadfrog" "anuran" "batrachian" "salientian") :FEATURES NIL
   :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX-AND-WORDNET
   :CONSTIT (% w::N))
  (ENTRY :WORD "frog" :score 1 :LFS (ONT::PERSON) :LIKE ("Gaul") :FEATURES NIL
   :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX-AND-WORDNET
   :CONSTIT (% N))
  (ENTRY :WORD "frog" :score 1 :LFS (ONT::PHYS-OBJECT) :LIKE ("frogs") :FEATURES
   NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX-AND-WORDNET
   :CONSTIT (% N))
  (ENTRY :WORD "frog" :score 0.8 :LFS NIL :LIKE NIL :FEATURES NIL
   :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX :CONSTIT (% N))
  (ENTRY :WORD "frog" :score 0.8 :LFS (ONT::PHYS-OBJECT) :LIKE
   ("toad" "toadfrog" "anuran" "batrachian" "salientian") :FEATURES NIL
   :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :WORDNET :CONSTIT (% N))
  (ENTRY :WORD "frog" :SCORE 0.8 :LFS (ONT::PERSON) :LIKE ("Gaul") :FEATURES NIL
   :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :WORDNET :CONSTIT (% N))
  (ENTRY :WORD "frog" :SCORE 0.8 :LFS (ONT::PHYS-OBJECT) :LIKE ("frogs")
   :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :WORDNET
   :CONSTIT (% N))))))
  (w::kwikly
   (setq res '(SUCCESS :ENTRIES
 ((ENTRY :WORD "quickly" :SCORE 1 :LFS (ONT::PREDICATE) :LIKE
   ("rapidly" "speedily" "chop-chop" "apace") :FEATURES NIL :COMLEX-SUBCATS NIL
   :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX-AND-WORDNET :CONSTIT (% w::ADV))
  (ENTRY :WORD "quickly" :SCORE 1 :LFS (ONT::PREDICATE) :LIKE
   ("promptly" "quick") :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME
   NIL :SOURCE :COMLEX-AND-WORDNET :CONSTIT (% ADV))
  (ENTRY :WORD "quickly" :SCORE 1 :LFS (ONT::PREDICATE) :LIKE ("cursorily")
   :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE
   :COMLEX-AND-WORDNET :CONSTIT (% ADV))
  (ENTRY :WORD "quickly" :SCORE 0.8 :LFS NIL :LIKE NIL :FEATURES NIL
   :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX :CONSTIT
   (% ADV))
  (ENTRY :WORD "quickly" :SCORE 0.8 :LFS (ONT::PREDICATE) :LIKE
   ("rapidly" "speedily" "chop-chop" "apace") :FEATURES NIL :COMLEX-SUBCATS NIL
   :WORDNET-VERB-FRAME NIL :SOURCE :WORDNET :CONSTIT (% ADV))
  (ENTRY :WORD "quickly" :SCORE 0.8 :LFS (ONT::PREDICATE) :LIKE
   ("promptly" "quick") :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME
   NIL :SOURCE :WORDNET :CONSTIT (% ADV))
  (ENTRY :WORD "quickly" :SCORE 0.8 :LFS (ONT::PREDICATE) :LIKE ("cursorily")
   :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :WORDNET
   :CONSTIT (% ADV))))))
  (w::dificult
   (setq res '(SUCCESS :ENTRIES
 ((ENTRY :WORD "difficult" :SCORE 1 :LFS (ONT::DOMAIN) :LIKE ("hard") :FEATURES
   ((GRADABILITY +)) :COMLEX-SUBCATS
   ((ADJ-PP :PVAL ("about" "for")) (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
   :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX-AND-WORDNET :CONSTIT (% w::ADJ))
  (ENTRY :WORD "difficult" :SCORE 1 :LFS (ONT::DOMAIN) :LIKE NIL :FEATURES
   ((GRADABILITY +)) :COMLEX-SUBCATS
   ((ADJ-PP :PVAL ("about" "for")) (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
   :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX-AND-WORDNET :CONSTIT (% ADJ))
  (ENTRY :WORD "difficult" :SCORE 0.8 :LFS NIL :LIKE NIL :FEATURES
   ((GRADABILITY +)) :COMLEX-SUBCATS
   ((ADJ-PP :PVAL ("about" "for")) (EXTRAP-ADJ-FOR-TO-INF-NP-OMIT))
   :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX :CONSTIT (% ADJ))
  (ENTRY :WORD "difficult" :SCORE 0.8 :LFS (ONT::DOMAIN) :LIKE ("hard") :FEATURES
   NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :WORDNET :CONSTIT
   (% ADJ))
  (ENTRY :WORD "difficult" :SCORE 0.8 :LFS (ONT::DOMAIN) :LIKE NIL :FEATURES NIL
   :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME NIL :SOURCE :WORDNET :CONSTIT
   (% w::ADJ))))))
  (w::extinguish
   (setq res '(SUCCESS :ENTRIES
 ((ENTRY :WORD "extinguish" :SCORE 1 :LFS (ONT::SITUATION-ROOT) :LIKE
   ("snuff out") :FEATURES NIL :COMLEX-SUBCATS ((NP)) :WORDNET-VERB-FRAME
   (8 9 10 11) :SOURCE :COMLEX-AND-WORDNET :CONSTIT (% w::V))
  (ENTRY :WORD "extinguish" :SCORE 1 :LFS (ONT::SITUATION-ROOT) :LIKE ("quench")
   :FEATURES NIL :COMLEX-SUBCATS ((NP)) :WORDNET-VERB-FRAME (8 11) :SOURCE
   :COMLEX-AND-WORDNET :CONSTIT (% V))
  (ENTRY :WORD "extinguish" :SCORE 1 :LFS (ONT::SITUATION-ROOT) :LIKE
   ("stub out" "crush out" "press out") :FEATURES NIL :COMLEX-SUBCATS ((NP))
   :WORDNET-VERB-FRAME (8) :SOURCE :COMLEX-AND-WORDNET :CONSTIT (% V))
  (ENTRY :WORD "extinguish" :SCORE 1 :LFS (ONT::SITUATION-ROOT) :LIKE
   ("eliminate" "annihilate" "eradicate" "wipe out" "decimate" "carry off")
   :FEATURES NIL :COMLEX-SUBCATS ((NP)) :WORDNET-VERB-FRAME (9 10) :SOURCE
   :COMLEX-AND-WORDNET :CONSTIT (% V))
  (ENTRY :WORD "extinguish" :SCORE 0.8 :LFS NIL :LIKE NIL :FEATURES NIL
   :COMLEX-SUBCATS ((NP)) :WORDNET-VERB-FRAME NIL :SOURCE :COMLEX :CONSTIT
   (% V))
  (ENTRY :WORD "extinguish" :SCORE 0.8 :LFS (ONT::SITUATION-ROOT) :LIKE
   ("snuff out") :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME
   (8 9 10 11) :SOURCE :WORDNET :CONSTIT (% V))
  (ENTRY :WORD "extinguish" :SCORE 0.8 :LFS (ONT::SITUATION-ROOT) :LIKE
   ("quench") :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME (8 11)
   :SOURCE :WORDNET :CONSTIT (% V))
  (ENTRY :WORD "extinguish" :SCORE 0.8 :LFS (ONT::SITUATION-ROOT) :LIKE
   ("stub out" "crush out" "press out") :FEATURES NIL :COMLEX-SUBCATS NIL
   :WORDNET-VERB-FRAME (8) :SOURCE :WORDNET :CONSTIT (% V))
  (ENTRY :WORD "extinguish" :SCORE 0.8 :LFS (ONT::SITUATION-ROOT) :LIKE
   ("eliminate" "annihilate" "eradicate" "wipe out" "decimate" "carry off")
   :FEATURES NIL :COMLEX-SUBCATS NIL :WORDNET-VERB-FRAME (9 10) :SOURCE
   :WORDNET :CONSTIT (% V))))))
  (w::nonword
   (setq res '(FAILURE :WORD "djxvy")))
  (t nil)
  )
  res)
)
