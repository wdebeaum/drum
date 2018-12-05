(in-package parser) 

;; From ../../Ontology/Code/KROntology/hierarchy-functions.lisp
(declaim (special *warned-already*))

;;
;; robustParser.lisp
;;
 
;;  This file contains the code for the ROBUST PARSER. It provides interface functions
;;  to the KQML interface, and contains the code that extracts speech acts from the 
;;  Chart.

;;  CONTROL VARIABLES


(defvar *filter-sa-on* nil)            ;; supress low probability and empty speech acts
(defvar *reliability-threshold* 0)     ;; minimum weight for a speech act to be considered
;;(defvar *number-interps-desired* 1)    ;; number of speech act interpretations desired

(defvar *online* t)                    ;; when NIL, it suppresses all output until an SA_GO-ONLINE is detected

;; discourse state variables
(defvar *current-conversant* nil)
(defvar *current-channel* nil)

(defvar *in-system* nil)
(defvar *uttnum* nil)
(defvar *include-parse-tree-in-messages*)
(defvar *ignore-prefer-msgs* nil)

(defun set-reliability-threshold (x)
    (setq *reliability-threshold* x))

(defun set-number-of-interps-desired (n)
  (setf (number-parses-desired *chart*) n))

(defun incr-number-found nil
  (setf (number-parses-found *chart*) (+ (number-parses-found *chart*) 1)))


;;================
;;  TOP-LEVEL FUNCTIONS

;;  HANDLING Channels - who are we speaking to?

(defun get-current-channel nil
  *current-channel*)

(defun set-current-channel (act)
  (setq *current-conversant* (find-arg-in-act act :who))
  (setq *current-channel* (find-arg-in-act act :channel))
  )

;;  PARSE handles input messages

;; (defvar *end-seen*)
(defvar *incremental-parsing-enabled* nil)

(defun parse-and-extract (text &optional cost-table chartsize)
  (let ((*cost-table* (or cost-table *cost-table*)))
    (parse-text text chartsize)))

(defun parse-text (text &optional chartsize)
  (let ((old (maxNumberentries *chart*)))
    (when chartsize
      (setf (maxNumberentries *chart*) chartsize))
    (parse '(start-sentence))
    (if (stringp text)
	(if *no-positions-in-lf*
	    (parse (list :text text))
	    (mapcar #'parse (generate-positional-words text 0)))
	;; already parsed into segments
	(mapcar #'parse	(mapcar #'(lambda (x) (list* (convert-to-package (car x) :parser) (cdr x)))
				 text)))
    (let ((ans (parse '(end-sentence))))
      (when chartsize
	(setf (maxNumberentries *chart*) old))
      ans)))

(defun continueParse (wordlist)
  (if (consp wordlist)
      (progn (mapcar #'parse wordlist)
	     (parse '(continue)))
      (format t "~%ERROR: contineParse must take a list of tagged words")))

(defun parseblock (cats start end initconstits wordmsgs)
  (let ((prefer-msgs (restart-parse-with-constits initconstits)))
    (continueparse (append prefer-msgs wordmsgs))
    (find-spanning-constit-names-in-chart cats start end)))

;;  cost-table management

(defvar *original-cost-table* nil)
(defvar *original-cost-table-was-modified* nil)

(defun adjust-cost-table (mods duration)
  "cost table is either modified for the next utterance only, or permanently. Default is next utterance only"
  (if (and (consp mods) (every #'consp mods))
      (if (eq duration 'permanent)
      	  (setq *cost-table* (append mods ;(convert-to-package mods *ont-package*)
				     *cost-table*))
	  (progn
	    (setq *original-cost-table* *cost-table*)
	    (setq *original-cost-table-was-modified* t)
	    (setq *cost-table* (append mods ;(convert-to-package mods *ont-package*)
				       *cost-table*))))
      (format t "~% BAD format of MODS in ADJUST-COST-TABLE: ~S" mods)))
      

;; =====
;; Parse
;; =====

(defun Parse (content)
  "Handle new input to the parsing process"
  
  ;; Do different stuff, depending on which Parse command is 
  ;; being given.
  
  (case (car content)

    ;; new word(s)
    ;; do the lexical lookup
    (word
     (trace-msg 2 "PARSER:: Processing input ~S" content)
     ;; check that there's not further punctuation in word that needs to be broken up
     ;;  but if we have a semantic tag then we just leave it as it is
     (if  (and (> (length (cadr content)) 1)  ;;a string with more than one character
	       (position-if #'(lambda (x) (member x *break-chars*)) (cadr content))  ;; which contains a break character
	       ;;(not (intersection (car (find-arg content :sense-info)) '(:ont-types :wn-sense-keys :domain-specific-info)))
	       (not (find-arg content :sense-info)))  ;; and has no sense tagged info
	  (let* ((frame (find-arg content :frame))
		 (start (car frame))
		 (end (cadr frame)))
	    (mapcar #'parse
		    (adjust-ending (generate-positional-words (cadr content) (or start 0)) end)))
	   ;;  normal processing - one lexicon item (though may be a contraction)
	  (mapcar #'add-to-agenda 
		  (flatten
		   (mapcar #'process-word-entry (prepare-input-for-parser content nil)))))
     nil
     )
    (prefix 
     ;; prefixes get a hyphen added onto them to distinguish from regular words
     (trace-msg 2 "PARSER:: Processing prefix ~S" content)
     (mapcar #'add-to-agenda
	     (flatten (mapcar #'process-word-entry (prepare-input-for-parser (list* 'word 
									 (concatenate 'string (cadr content) "-") 
									 (cddr content)) 'prefix  ))))
     nil)
    ;; STARTED SPEAKING
    ;; Prepares the parser for handling a new sentence.
    ;; Basically, sets the *end-seen* variable to false,
    ((started-speaking start-new-sentence start-sentence)  ;; speech and text versions
     ;; gf: ignore started-speaking from ourselves
     (update-genre (find-arg-in-act content :genre))
     (let ((who (find-arg-in-act content :who)))
       (unless (or (eq who 'sys) (eq who 'calo))
	 (trace-msg 1 "~%Started new utterance")
	 (set-current-channel content)
	 (setf (end-seen *chart*) nil)
	 (setf (lex-items *chart*) nil)
	 ;; Set up an empty chart and agenda.    
	 (startNewUtterance))))

    ;; UTTERANCE END
    ;;   start the actual parser
    ((utterance end-sentence)
     (compute-barriers)
     (setq *uttnum* (find-arg-in-act content :uttnum))
     (setf (end-seen *chart*) t)
     ;; adjust the average length of words (dramatically difference between text and speech!)
     (adjust-word-length-parameter content)
     ;;  manage the temporary cost-table changes
     (if *original-cost-table-was-modified*
	 (setq *original-cost-table-was-modified* nil)
	 (when *original-cost-table*
	     ;; we reset the cost table
	   (setq *cost-table* *original-cost-table*)
	   (setq *original-cost-table* nil)))
	 
     (continue-BU-parse)
     (let ((utts (get-parse :lf :numb (number-parses-to-find *chart*))))
       (if *include-parse-tree-in-messages*
	   (let ((syntax (get-parse :syntax :numb (number-parses-to-find *chart*) :feats *include-parse-tree-in-messages*)))
	     ;; currently only return the syntax for the first tree
	     (cons (append (car utts) (list :tree syntax)) (cdr utts))
	     )
	   utts)))
	
    ;; CONTINUE

    (continue
     ;; if we've hit max chart sIzesome more
     (when (> (numberEntries *chart*) (- (getmaxnumberEntries) 100))
       (setmaxnumberentries (+ (numberEntries *chart*) 100))
        (trace-msg 2 "Allocating more entries to max for chart to ~S" (getmaxnumberentries))
		   )
     (let ((n (find-arg (cdr content) :index)))
       (setf (end-seen *chart*) t)
       (continue-BU-parse)
       (build-parse-result (or n 2) nil))
     )
    ;; for backwards compatability
    (lattice
     
     (mapcar #'parse (cons '(started-speaking) (cdr content)))
     (parse '(utterance)))

    (prefer
     (when (not *ignore-prefer-msgs*)
       (handle-constit-preference-msg content))
     (values))
    
    (stopped-speaking ;; button was released but we don't care as word may still come
     T)

    (otherwise 
     (parser-warn "parser: can't interpret message: ~S" content)
     nil)
    ))

(defvar *genre* nil)

(defun update-genre (genre)
  (when (and genre (not (eq genre *genre*)))
    (setq *genre* genre)
    (lg genre)))
  

(defun adjust-ending (words end)
  "when we break up a word into several, we can't tell what the final ending position should be +/- 1"
  (let* ((lastone (car (last words)))
	(lastframe (find-arg lastone :frame)))
    (if (or (null lastframe) (eq (cadr lastframe) end) (< end (car lastframe)))
	words
	(append (butlast words) (list (append (remove-arg lastone :frame) (list :frame (list (car lastframe) end))))))))
	

(defun adjust-word-length-parameter (content)
  "we adjust the average word length based on number of words in utterance and utterance length"
  (let ((text (find-arg-in-act content :text)))
    (setq *word-length*
	  (if text
	      (/ (max (max-position-found *chart*) (length text))   ;; we add the (length text) part in case text tagger sends an UTTERANCE mesage without any word messages before hand - we can't let *word-Length* be 0.
		 (max (list-length (remove-if #'(lambda (x) (string= (cadr x) "<SIL>"))
					      (generate-positional-words (find-arg-in-act content :text) 1))) 1))
		 *default-word-length*))))
  
(defun process-word-entry (item)
  (when (consp item)
     ;; next-entry is of form
     ;; (word :start <start> :end <end> :prob <prob> ....)
     (let* ((word (cadr item))
	    (start (find-value item :start))
	    (end (find-value item :end))
	    (score (find-value item :score))
	    (prob (find-value item :prob))
	    (remaining-feats (cddr item)))
       (if (null start) (break "no start specified: process word entry"));;(setq start (up-max-position)))
       (if (null end) (break "no end specified: process word entry"))
       (update-min-position-found start)
       (update-max-position-found end)
       ;; MYROSIA's vars updates
       (new-chart-input-end end)
       (if (stringp word)
	   ;; strings are either numbers or actual strings
	   (let ((n (extract-number-value word)))
	     (if n
		 ;; build a number constituent
		 (mapcar #'(lambda (x) (add-digits-feature x word))
			 (find-lex-entries n start end (compute-lexical-prob prob score) (remove-arg remaining-feats :sense-info)))
		 ;; build a string constituent
		 (list (build-entry (make-string-constit item)
				    start
				    end
				    nil
				    'string-builtin
				    1.0
				    nil
				    (list 'W::name)))))
	   (find-lex-entries word start end (compute-lexical-prob prob score)
			     remaining-feats))
	   
       )
     ))

(defvar *score-factor* .05)   ;; this is the probabilty mass affected by the score

(defun compute-lexical-prob (prob score)
  (or prob
      (if score (+ (- 1 *score-factor*) (* score *score-factor*))
	  1)))

(defun extract-number-value (strng)
  "if the string is a sequence of digits, we read it as a number"
  (let ((n (and (not (position #\Space strng))
		(read-from-string strng nil))))
    (if (numberp n) n)))

(defun add-digits-feature (e digs)
  (when (entry-p e)
    (push (list 'W::NUMBER-DIGITS (length digs)) (constit-feats (entry-constit e)))
    (push (list 'W::DIGITS digs) (constit-feats (entry-constit e))))
  e)

;(defun make-string-constit (strconstit)21
(defun make-string-constit (strconstit)
  (make-constit :cat 'w::name :feats (list (list :value (second strconstit)))))


(defun build-parse-result (N features-to-pass-on)
  "Builds a representation of the N'th best parse"
  (let ((answer (getanalyses (get-current-channel) N features-to-pass-on)))
    (cond
     ((eq (car answer) 'SA_GO-OFFLINE)
      (setq *online* nil)
      answer)
     ((eq (car answer) 'SA_GO-online)
      (setq *online* t)
      answer)
     (*online*
      answer)
     (t (trace-msg 1 "Ignoring input because system is off line" nil)
        (values)))))

(defun convert-frame-to-start-end (expr)
  (let ((frame (find-arg expr :frame)))
    (list* 'phrase (tokenize (cadr expr)) 
	   (append (remove-arg (cddr expr) :frame) (list :start (car frame)
						       :end (+ (cadr frame) 1))))))

;; =================
;; startNewUtterance
;; =================
;; Set *utterance-parsing-complete* and *end-of-utternace-processing-performed*
;; both to nil, since neither have been done for this sentence yet.

(defun startNewUtterance ()
  ;;(clear-context-after 0)
  (Reset-max-position 0)
  (reset-min-position 1000)
  (setf (skeleton *chart*) nil)
  (setq *semantic-skeleton-map* *debug-skeleton-info*)
  (setq *var-type-map* nil)
  (setq *vnum* 0)
  (setq *skel-boost-constits-seen-already* nil)
  (when (barrier-penalty *chart*)
    ;;(if (null (barriers *chart*)) 
    (setf (barriers *chart*) (make-array (+ (maxChartSize *chart*) 1) :initial-element nil)))
;;	(dotimes (i (maxChartSize *chart*))
;;	  (setf (aref (barriers *chart*) i) nil))))
  (setf (barrier-violation-count *chart*) 0)
  (setf (barriers-computed *chart*) nil)
  (setq *warned-already* nil) ;; enable unknown type warning each tim
  (setf *reference-feedback-already-this-sentence* nil) ;; no feedback yet!  new sentence!
  (setf *utterance-parsing-complete* nil) ;; we're just getting started.
  (setf *end-of-utterance-processing-performed* nil) ;; haven't done this either.
  ;; reinitialize the chart
  (setf (constituent-count *chart*) 0)
  (setf (number-parses-found *chart*) 0)
  (make-chart (getmaxChartSize))
  (setf (StopFlag *chart*) nil)
  (init-agenda *number-of-buckets-for-agenda*)
  T)


(defun continueUtterance (input)
  (do-one-step-parse input)
   T)

(defun backUpWords (numb)
  ;;  backs up the parser n words
  (trace-msg 1 "~%Backing up ~s words" numb)
  (backup numb)
  T)

(defvar *remove-subsumed-skeleton-constit* nil)

(defun handle-constit-preference-msg (content)
  "adds skeleton information to *skeleton* to filter parses, and constructs barriers"
  (let ((pos (find-arg content :frame))
	(c (intersection *skeleton-constit-cats*
			 (flatten 
			  (mapcar #'convert-penn-to-trips 
				  (remove-if #'null
					     (or  (convert-to-package (find-arg content :penn-cats) :w) (find-arg content :trips-cats))))))))
    (when (or (not *remove-subsumed-skeleton-constit*) (not (subsumed-in-skel c pos (skeleton *chart*))))
	(setf (skeleton *chart*) 
	      (Cons (cons c (list (car pos) (if (numberp (cadr pos)) (+ (cadr pos) 1))))
		    (if (not *remove-subsumed-skeleton-constit*)
			(skeleton *chart*)
			(remove-subsumed-in-skel c pos (skeleton *chart*)))))
	(define-barrier (car pos) (cadr pos) c)
    )))

(defun subsumed-in-skel (cat pos values)
  "checks if (cat pos) is already subsumed in the skeleton"
  (when values
    (let ((val (car values)))
      (if (and (eq (car val) cat) 
	       (<= (second val) (car pos))
	       (>= (third val) (second pos)))
	  t
	  (subsumed-in-skel cat pos (cdr values))))))

(defun remove-subsumed-in-skel (cat pos values)
  "removes values subsumed by (cat pos)"
  (when values
    (let ((val (car values)))
      (if (and (eq (car val) cat) 
	       (>= (second val) (car pos))
	       (<= (third val) (second pos)))
	  (remove-subsumed-in-skel cat pos (cdr values))
	  (cons val (remove-subsumed-in-skel cat pos (cdr values)))))))

(defstruct barrier :starts :bracketing :durings)

(defun define-barrier (startpos endpos tripscats)
  "gather the start and end cohorts for each of the cats and combine them"
  (when (barrier-penalty *chart*)
    (let ((end-index (+ endpos 1)))
      (when tripscats
	(let* ((cohorts (mapcar #'get-cohorts tripscats))
	       (starts (remove-duplicates (flatten (mapcar #'(lambda (c) (car c)) cohorts))))
	       (durings (remove-duplicates (flatten (mapcar #'(lambda (c) (cadr c)) cohorts))))
	       (start-barrier (or (aref (barriers *chart*) startpos) (make-barrier))))
	  (setf (barrier-starts start-barrier) (cons (list tripscats starts end-index)  (barrier-starts start-barrier)))
	  (setf (aref (barriers *chart*) startpos) start-barrier)
	  (set-bracketing startpos end-index (remove-duplicates (append starts durings))))))))

(defun set-bracketing (start end cats)
  "adds bracketing constrains for each posituion between start and end"
  (add-bracketing-constraint (+ start 1) end start end cats))

(defun add-bracketing-constraint (i N start end cats)
  (when (< i N)
    (let* ((b (or (aref (barriers *chart*) i) (make-barrier)))
	   (lowerbound (second (barrier-bracketing b)))
	   (upperbound (third (barrier-bracketing b))))
      (when (or (null lowerbound) (> start lowerbound) (< end upperbound))
	(setf (barrier-bracketing b) (list cats start end))
	(setf (aref (barriers *chart*) i) b))
      (add-bracketing-constraint (+ i 1) N start end cats))))

(defun get-cohorts (c)
  (copy-list (cdr (assoc c (barrier-cats *chart*)))))

(defun convert-penn-to-trips (c)
  "c may be a PENN-TAG or a TRIPS TAG"
 (when c
   (case c
      ((w::sbar w::CP) (list 'w::CP 'w::ADVBL))   ;; we need ADVBL because Stanford parser counts "even though X, Y" as a SBAR
      ((w::sbar-nom w::NP-lgs w::s-nom) (list 'w::NP))
      ;;(w::NX (list 'w::NP 'w::N1))
      ((w::sbar-prp w::sbar-adv) (list 'w::ADVBL))
      (w::rrc (list 'w::CP))
      (w::VP (list 'w::VP 'w::S 'w::CP 'w::VP-))   ;; stat parser treats infinitives like TO SEE in "I was able to see" as an S, and sometimes as a VP
      (w::S (list 'w::VP 'w::S 'w::CP 'w::UTT))   ;; parser treats infinitives like TO SEE in "I was able to see" as an S, and sometimes as a VP
      ((w::pp w::pp-clr) (list 'w::ADVBL 'w::PP))
      ((w::sbarQ w::sinv w::sq) (list 'w::utt))
      (w::advp (list (list 'w::ADVBL)))
      (w::adjp-pred (list 'w::PRED))
      (w::frag nil)
      ;;(w::whnp (list 'w::NP))  NO- this is a REL PRO form!
      (w::whnp nil)
      (otherwise (list (simplify-tag c))))))

(defun simplify-tag (c)
  "strip anything after a hyphen"
  (let ((p (position #\- (symbol-name c))))
    (if p (convert-penn-to-trips (intern (subseq (symbol-name c) 0 p) :W))
	c)))
    
;;  Computing the infor for use in guiding the parser based
;;   on advice


(defun compute-barriers nil
  "all the preferred constits have been processed, we now compute the bracket crossing info"
  (when (and (barrier-penalty *chart*) (null (barriers-computed *chart*))) ;; we do this test in case the input has several "end" messages
    (setf (barriers-computed *chart*) t)
    ;;(format t "~% Barriers are: ~S" (barriers *chart*))
    ;;(dotimes (i (+ (get-max-position) 1))
    ;;  (setf (aref (barriers *chart*) i) (if (aref (barriers *chart*) i)
    ;;    (cons nil (aref (barriers *chart*) i)))))
    ;;(add-start-posns 0 (get-max-position))
    (remove-false-starts (get-max-position))))

(defun show-barriers nil
  (dotimes (i (get-max-position))
    (let ((x (aref (barriers *chart*) i)))
      (when x
	(if (barrier-starts x)
	    (format t "~%~S: ~S" i (mapcar #'(lambda (y) (list (car y) (caddr y))) (barrier-starts x)))
	    (if (barrier-bracketing x)
		 (format t "~%~S: ~S" i  (cdr (barrier-bracketing x)))))))))

(defun add-start-posns (i max)
  "this adds the nearest preceeding constituent boundary so we can check that the arc being extended
      can't start before that"
  (when (<= i max)
    ;; see if we have new start 
    (let ((end-posns (gather-end-posns (cadr (aref (barriers *chart*) i)))))
      (set-barrier-posns (+ i 1) i end-posns)
      (add-start-posns (+ i 1) max))))

(defun gather-end-posns (starts)
  "gets all the costituents that start and sort by end posn"
  (sort (mapcar #'caddr starts) #'<))

(defun set-barrier-posns (i val ends)
  "thjis scanes forward setting the laest start time, popping the stack as each limit is exceeded"
  (when ends
    (if (>= i (car ends))
	(set-barrier-posns i val (cdr ends))
	(let ((barr (aref (barriers *chart*) i)))
	  (setf (aref (barriers *chart*) i) (cons val (cdr barr)))
	  (set-barrier-posns (+ i 1) val ends)))))

(defun remove-false-starts (max)
  "These removes a few cases where the statistical parser uses constructions like NP <- NP PP,
   we need to remove the sub NP"
  (dotimes (i max)
    (clean-up-posn i)))

(defun clean-up-posn (i)
  (let* ((barriers (aref (barriers *chart*) i)))
    (if (barrier-p barriers)
	(let ((starts (barrier-starts barriers)))
	  ;;(format t "~% ~S: checking ~S" starts (caaar starts))
	  ;;(when (and (> (list-length starts) 1) (eq (caaar starts) 'W::NP))
	  ;; (format t "~%CHECKING for false starts in ~S" starts)
	  (let* ((sorted-starts (sort starts #'> :key #'(lambda (x) (caddr x))))
		 (reduced-starts (remove-dups sorted-starts nil)))
	    (if (< (list-length reduced-starts) (list-length starts))
		(setf (barrier-starts barriers)
		      reduced-starts))))
	(setf (aref (barriers *chart*) i) (make-barrier)))))

(defun remove-dups (starts seen)
  (when starts
    (if (and (eq (caaar starts) 'W::NP) (member 'W::NP seen))
	(remove-dups (cdr starts) seen)
	(cons (car starts)
	      (remove-dups (cdr starts) (union (caar starts) seen))))))
    
     

  
;; ========================
;; prepare-input-for-parser
;; ========================

(defun prepare-input-for-parser (in isprefix)
  (remove-if #'null 
	     (prepare-input-for-parser-one in isprefix))
 
)
;; ============================
;; prepare-input-for-parser
;; ============================

;;  This prepares kqml parse requests into the input format for the parser
;;  e.g., it takes a list of the form
;;   (word "dog's" :frame (3 5) ...)
;;   and produces
;;    ((DOG :start 3 :end 4) (^S :start 5 :end 6))

(defun prepare-input-for-parser-one (in isprefix)
 
  (let* ((new-tokens (if ;;(or 
			  isprefix ;;(not (position-if #'(lambda (x) (member x *break-chars*)) (second in))))
			 (list (convert-to-word (coerce (string-upcase (second in)) 'list)))
			 ;; if break chars we must tokenize
			 (tokenize (second in))))
			      
	 (number-of-tokens (length new-tokens))
	 (frame (find-value in :frame))
	 (start (if (eql (find-value in :index) 1) 1 (car frame)))
	 (end (if (numberp (cadr frame)) (+ (cadr frame) 1)))
	 (span-length (if end (- end start)))
	 ;; (score (find-value in :score))
	 (prob (find-value in :prob))
	 (sense-info (find-value in :sense-info))
	 (alternative (find-value (car sense-info) :alternate-spellings))
	 (reduced-features (remove-args (cddr in) '(:start :end)));; :prob)))
	 (word (if (and (consp new-tokens) (eq number-of-tokens  1))
		   (car new-tokens)
		   new-tokens)))
    
    ;; a special case where we have an unusual word (e.g., 2day, or a number that has a non-number interpretation) that has an alternate spelling attached (e.g., "today")
    (if (and (some #'stringp new-tokens) alternative  ;;(some #'symbolp new-tokens)   commented out to allow "8" with a protein sense in a sequence!!
	     ;; this last condition ensures we don't recurse 
	     (not (string-equal (car alternative) (second in))))
	(prepare-input-for-parser-one (list* 'word (car alternative) (cddr in)) isprefix)
	
	;; continuing on regular case
	(progn
	  (if (null end) (break "found missing END value"))
	  
	  (cond 
	    (sense-info   ;; if the sense-info is defined, its a single lexical item even if multiple words)
	     ;; we also remove the under-bars which might have been introduced from sources of compound words, like WordNet
	     (list (list* 'word (if (consp word)
				    (remove-if #'(lambda (x) (eq x 'w::UNDER-BAR)) word) word) :start start :end end :prob prob reduced-features)))
	    ((< span-length number-of-tokens)
	     ;;  ERROR condition: more tokens that can fit between frame numbers
	     ;;    we ignore the end index after generatingan error message
	     (parser-warn (Format nil "Bad Args to parser: number of tokens not consistent with frame: ~A, ~A" 
				  new-tokens frame)))
	    ;; case of an untagged single token
	    ((or (symbolp word) (numberp word))
	     (list (list* 'word word :start start :end end ;;:prob prob 
			  reduced-features)))
	    ;; need to break apart into a sequence of lexical entries
	    (t
	     (generate-standard-sequence new-tokens start end prob reduced-features))
	    )))))

(defun token-length (x)
  (cond
    ((eq x 'W::punc-ordinal) 2)  ;;e.g., 1st, 2nd, 3rd, 4th, ...
    ((member x *punc-tokens*) 1)
    ((symbolp x) 
     (length (symbol-name x)))
    ((numberp x) (length (format nil "~A" x)))
    (t 1))
  )
    

;; Now also expects a value for :alternatives
;; Handle-first-word now makes the recursive call if alternatives is set

(defun generate-standard-sequence (tokens start end prob others)
  "generates the standard sequence of sequentially indexed tokens. Note that the filter
     only applies to the first element e.g., 'dog's' with a filter N, only 'dog' would have to be a N"
  (if (or (null end) (<= start end))
      (if tokens
	  ;;(if alternatives
	  ;;    (handle-first-word tokens start end prob score feats filter)
	  (let* ((word (car tokens))
		 (newend (if (cdr tokens)
			     (compute-end-position start word);;(floor (+ start (/ (- end start) 2)))
			     end)))
	    (if (> newend start)
		(cons (list* 'word word :start start :end newend :prob prob others)
		      (generate-standard-sequence (cdr tokens) newend end prob others))
		(progn
		  (format t "~%WARNING - bad tokenization of ~S" tokens)
		  nil)))
	      
	  nil)
      (progn 
	(format t "~%~% Error is tokenization found (start > end!):start=~S end=~S tokens= ~S" start end tokens)
	nil)))

(defun compute-end-position (start word)
  (+ start (list-length (coerce (symbol-name word) 'list)) 1))
     

;; ==============================================
;; CHART EXTRACTION FOR THE PARSER DISPLAY MODULE
;; ==============================================

(defun get-display-chart (feats &key constits)
  (get-display-chart-span nil nil feats :constits constits)
  )

(defun get-display-chart-span (start end feats &key constits)
  (reverse (mapcar #'(lambda (x) (prepare-entry-for-display x feats))
		  (get-constits-from-chart start end :constits constits)
		  ))
  )
	
(defun prepare-entry-for-display (entry features)
  (list
   (remove-packages (entry-name entry))
   ':start (entry-start entry)
   ':end (entry-end entry)
   ':rule (entry-rule-id entry)
   ':prob (entry-prob entry)
   ':score (calculate-entry-score entry)
   ':constit (prepare-constit-for-display (entry-constit entry) features 'skips)
   )
  )

;; (defun show-entry-with-name (entry &optional features)
;;  (pprint-logical-block (nil nil)
;;    (format t "~s: " (remove-packages (entry-name entry)))
;;    (pprint-logical-block (nil nil)
;;      (format-constit (entry-constit entry) features)
;;      (write-char #\space)
;;      (format t "from ~S to ~S using rule ~S" (entry-start entry) 
;;              (entry-end entry) (entry-rule-id entry))
;;      (format t " [P = ~,4F S = ~,4F]" (entry-prob entry) (calculate-entry-score entry))
;;      (format t "~%"))))

(defun prepare-constit-for-display (constit features skips)
  "Prints out a constituent"
   (when (constit-p constit)
	 ;; This winnows down the feature list to those features
	 ;; requested by the calling function
       (let ((feats (remove-packages
		     (remove-if #'(lambda (x)
                                   (null (cadr x)))
                               ;; remove unwanted features (if indicated)
                               (if (consp features)
                                 (remove-if-not #'(lambda (fv)
                                                    (member (car fv) features))
                                                (constit-feats constit))
                                 (constit-feats constit))))))
		 ;;This makes sure that the skips are at the top level, if desired
		 (if skips
			 (append (get-skips-for-display (constit-feats constit)) feats)
		   feats
		   )
		 )
	   )
   )

(defun get-skips-for-display (flist)
  (remove-if #'null
			 (mapcar #'handle-skip-for-display
					 ;; the notes feature value, if notes exists.
					 (cadr (find-if #'(lambda (x) (equal (car x) 'notes)) flist))
					 )
			 )
  
  
  )

(defun handle-skip-for-display (skip-feature)
  (cond
   ((null skip-feature) 'nil)
   ((equal (car skip-feature) 'skipped) (list 'skipped (remove-packages (entry-name (second skip-feature))))
	)
   (T 'nil)
   )
  )
	

;;=====================================
;;
;;    Code to post-process the chart to extract the best speech act interpretations

;;   GETANALYSIS
;;   interprets the chart and traces from monitors to construct a speech act
;;   interpretation of the input.
;;   Args: SPEAKER - the speaker id, NUMBER-INTERPS - how many speech act interpretations
;;       to return (in ranked order), GLOBAL-FEATURES - a list of slot-value pairs to add
;;       onto resulting speech act (e.g., (:SIGNAL :TOO-SOFT))
(defun getAnalyses (channel N global-features)
  "handles N calls to getAnalysis to construct the speech acts"
  (let ((end (get-max-position)))
    (mapcar #'(lambda (a) (getAnalysis channel end a global-features))
	    (find-best-paths 0 end N))))

(defun getAnalysis (channel end fsa-result global-features)
  ;; Builds the speech act representation of the provided path
  (let*
    ((speechActs (cadr fsa-result))
     (goodness (car fsa-result))
     (interps
      (remove-if #'null
                 (mapcar 
                  #'(lambda (sa) 
                      (interp-speech-act sa goodness channel))
                  speechActs))))
    
    ;; now, build the appropriate speech act constructions
    (case (length interps)
      (0 
       (list 'w::UTT :reliability 0 :channel channel
             :noise (filter-objects (get-unknown-words) 0 end)
	     :uttnum *uttnum* :start (find-first-non-null 0 end) :end end))
      (1 (append (car interps) global-features))
      (otherwise 
       (append
        (list 'compound-communications-act
              :acts interps
              :reliability goodness :channel channel
              :noise (mapcan  #'(lambda (interp)
                                  (copy-list (find-arg (cdr  interp) :noise)))
                              interps)
              :words (mapcan #'(lambda (interp)
                                 (copy-list (find-arg (cdr  interp) :words)))
                             interps))
	      
        global-features))
      ))
  )

;; Remove ":RELIABILITY XX" from wherever it appears in the
;; list structure (used to compare SAs for duplicates except for :reliability score)
(defun remove-rel (s)
  (if (consp s)
      (if (atom (car s))
	  (if (eql (car s) :RELIABILITY)
	      (remove-rel (cddr s))
	    (cons (car s)
		  (remove-rel (cdr s))))
	(cons (remove-rel (car s))
	      (remove-rel (cdr s))))
    s))


(defun interp-speech-act (e goodness channel)
  "Takes a speech act entry from the parser,or a set of fragments,
   and build a speech act structure"
  (if (entry-p e)
   (let* ((utt (entry-constit e))
          (start (entry-start e))
          (end (entry-end e))
	  ;;(prob (entry-prob e))
          (tree (build-constit-tree e nil))
          (abbrev-tree (abbreviate-tree tree))
          (words (get-value utt 'w::input))
          (content (get-value utt 'w::lf))
          (sa-var (get-value utt 'w::var)) ;;(if (constit-p orig-LF) (get-value orig-LF 'var))
          (content-var (if (constit-p content) (get-value content 'w::var)))
          )
     (if (eq (constit-cat utt) 'w::utt)
       (multiple-value-bind
         (objects unknowns) 
         (find-objects-in-constit-tree tree)
         ;;  Build speech act
         (make-speech-act 'w::UTT channel
                          (get-value utt 'w::focus)
			  objects
                          sa-var
                          unknowns
                          goodness
                          abbrev-tree
                          (list (get-value utt 'w::subjvar) (get-value utt 'w::dobjvar))
                          words
                          )
         )
       
       ;; No UTT found, we have a fragment
       (let
         ((objects (find-objects-in-constit-tree (build-constit-tree e nil)))
          (input (remove-if #'(lambda (w)
                                (member w '(w::START-OF-UTTERANCE w::END-OF-UTTERANCE)))
                            (get-input-seq-from-entry e )))
          ;;(lf (get-value utt 'w::lf))
	  )
         (make-speech-act 'w::SA_FRAGMENT channel
                          nil
                          objects ;;(cons utt objects)
			  (or (get-value utt 'w::var) 'unknown)
                          (filter-objects (get-unknown-words) start end)
                          (case (constit-cat utt)
                            ((NP PATH) (* .9 goodness))
                            (otherwise (* .7 goodness)))
                          abbrev-tree
                          nil ;; subject and object not found
                          input
                          )
         )))
   ;; couldn't even find a good entry
   (list 'w::FRAGMENT :reliability 0)
   ))

(defvar *pros-found* nil)

(defun Add-to-pros-found (x)
  (when (not (assoc (car x) *pros-found*))
    (setq *pros-found* (cons x *pros-found*))))

(defun remove-*pro*-from-LF (lf)
  "This removes replaces the *PRO* constits in the LF with their variables and creates new objects"
  ;;(format t "~%REmoving PRO from ~S, which is a list? ~S a constit? ~S" lf (consp lf) (constit-p lf))
  (let ((*pros-found* nil))
    (remove-*PRO*-from-constraints lf)))

(defun remove-*PRO*-from-constraints (lf)
  "This searches the constraints and class of an LF constit to find *PRO* structures"
  (if (constit-p lf)
      (multiple-value-bind (new-class class-pros)
	  (remove-*pro*-from-LF (get-value lf 'w::class))
	(let* ((c (get-value lf 'w::constraint))
	       (new-c c)
	       (newest-class (remove-pros new-class))
	       )
	  ;; replace the class
        (when *pros-found* ;;class-pros
          (setq lf (replace-feature-value lf 'w::class newest-class)))
	
        (cond
         ((consp c)
          (setq new-c (remove-pros c)))
         ((and (constit-p c) (eq (constit-cat c) '&))
          (setq new-c (make-constit
                       :cat (constit-cat c)
                       :feats (remove-pros (constit-feats c)))))
         )
        (if *pros-found*
          (values (replace-feature-value lf 'w::constraint new-c) 
                  (append class-pros (build-pro-objects *pros-found*)))
          (values lf class-pros))
        ))	  	  
    (values lf nil)))

(defun remove-pros (c)
  "replaces any (% *pro* (var v) ...) and (% *wh-term* (var v) ...) forms with v and remembers the forms in *pros-found*"
  (cond 
   ((constit-p c) 
    (if (consp (constit-feats c))   
      ;;  have a real constit here rather some other structure, check if an implicit object
      (if (member (constit-cat c) '(w::*PRO* w::*WH-TERM* w::*NP*))
        (let ((v (get-value c 'w::var)))
          (if (not (assoc v *pros-found*))
            (add-to-pros-found (list v (remove-*pro*-from-constraints c))))
          v)
        (make-constit :cat (constit-cat c)
                  :feats (mapcar #'remove-pros (constit-feats c))))
      ;;  non-standard constits are not changed
      c))
   ((consp c)
    (mapcar #'remove-pros c))
   (t c)))

(defun build-pro-objects (pros)
  (mapcar #'build-pro-object pros))

(defun build-pro-object (d)
  "Build the object table constit entry: (car d) is the var, (second d) is the constit"
  (let ((var (car d))
        (c (second d)))
    (when (constit-p c)
      (let ((status (get-value c 'w::status)))
      (cons var
            (make-constit :cat (if (eq status 'w::F) 'w::PROP 'w::description)
                          :feats
                          `((w::var ,var)
                            (w::class ,(or (get-value c 'w::class) (get-value c 'w::sem)))
                            (w::sem ,(get-value c 'w::sem))
			    (w::tma ,(get-value c 'w::tma))
                            (w::status ,(or status 'w::*PRO*))
                            (w::constraint ,(get-value c 'w::constraint))
			    (w::start ,(get-value c 'w::start))
			    (w::lex ,(get-value c 'w::lex))
			    (w::end ,(get-value c 'w::end))))
            )))))
  
    
;;;=========
;;  MAKE SPEECH ACT
;;

(defun make-speech-act (newsa channel focus objects lf unknown-words reliability syntax subj-object words)
  (if (and (eq newsa 'NULL) (null unknown-words))
      (list 'sa-null :reliability 100)
    (if (or (not  *filter-sa-on*)
	    (and (not (and (eq newsa 'SPEECH-ACT)
			   (null objects)
			   (null lf)))
		 (>= reliability *reliability-threshold*)))
	
	(let nil
	  ;;  if NULL act got through, there are unknown words - convert to a SPEECH-ACT
	  (cond
	   ((eq newsa 'NULL) (setq newsa 'SPEECH-ACT))
	   ((var-p newsa)
	    (let ((vals (var-values newsa)))
	      (setq newsa
		(case (list-length vals)
		  (0 'SPEECH-ACT)
		  (1 (car vals))
		  (otherwise (cons 'or vals)))))))
	  
	  (trace-msg 1 "-Speech Act Found: ~s~%" newsa)
	  (when (assoc lf objects)
	    (list
	     newsa
	     :channel channel
	     :focus  (undo-constits focus)
	     :objects (undo-constits  objects)
	     :root (if (not (member lf '(- :-)))  (undo-constits lf))
	     :noise unknown-words
	     ;; gf: 3/10/2000: Need undo-constits here also ("hello please")
	     :reliability reliability
	     ;; gf: Added undo-constits calls for :syntax arg to keep
	     ;;     #S syntax out of KQML stream
	     :syntax (list :tree (constit-to-list syntax)
			   :subject (if (not (member (car subj-object) '(- :-))) (undo-constits (car subj-object)))
			   :object (if (not (member (cadr subj-object) '(- :-))) (undo-constits (cadr subj-object))))
	     :words (remove-if #'(lambda (x) (member x '(w::START-OF-UTTERANCE w::END-OF-UTTERANCE))) words)
	     :uttnum *uttnum*)
	    )))
    ))



(defun undo-constits (c)
  (constit-to-list c))


;;==========================================================================
;;
;; CODE FOR EXTRACTING OBJECTS AND UNKNOWNS FROM PARSE TREE       
;; George's new more functional version of this (2/6/2007)

(defun find-objects-in-constit (x)
  (find-objects-in-constit-tree (build-constit-tree (get-entry-by-name x) nil)))
  
(defun find-objects-in-constit-tree (constit-tree)
  (multiple-value-bind (symbol-table unknowns)
      (traverse-tree-for-objects constit-tree)
    ;; I actually doubt that REVERSE is necessary here
    (values (reverse symbol-table) unknowns)))

(defun replace-sem-in-lf (c sem)
  (setf (constit-feats c)
              (replace-feat (constit-feats c) 'w::sem sem))
  c)
  
(defun traverse-tree-for-objects (constit-tree &optional temp-symbol-table unknowns)
  (flet ((add-to-temp-symbol-table (var val)
	   (when (not (assoc var temp-symbol-table))
	     (setq temp-symbol-table (cons (cons var val) temp-symbol-table))))
	 (get-temp-symbol-table ()
	   ;; I really doubt that we want to be doing REVERSE every time here
	   temp-symbol-table))
    (let* ((constit (car constit-tree))
	   (lf (get-value constit 'w::lf))
	   (lex (get-value constit 'W::lex))
	   (sem (get-value constit 'w::sem))
	   (input (get-value constit 'w::input))
	   (notes (get-value constit 'w::notes))
	   (var (if (constit-p lf) (get-value lf 'w::var))))
      (unless (or (null var) (assoc var (get-temp-symbol-table)))
	(multiple-value-bind (new-lf pros)
	    (remove-*pro*-from-lf lf)
	  (add-to-temp-symbol-table var (if (constit-p new-lf) 
					    (replace-sem-in-lf
					     (add-feature-value
					      (add-feature-value new-lf 'w::input input)
					      'w::lex
					      lex)
							 sem)
					    new-lf))
	  ;; if there were *PRO* objects found, add them too
	  (if pros
	      (mapc #'(lambda (x)
			(add-to-temp-symbol-table (car x) (cdr x)))
		    pros))))
      (if (assoc 'unknown notes)
	  (setq unknowns (append (remove-if #'null
					    (mapcar #'(lambda (x)
							(if (eq (car x) 'unknown)
							    (cadr x)))
						    notes))
				 unknowns)))
      (loop for x in (cdr constit-tree)
	    do (multiple-value-bind (new-symbol-table new-unknowns)
		   (traverse-tree-for-objects x temp-symbol-table unknowns)
		 (setq temp-symbol-table new-symbol-table)
		 (setq unknowns new-unknowns)))
      (values temp-symbol-table unknowns))))


(defun abbreviate-tree (c)
  "given a list of constits, returns a tree for each constit indicating category, var, LF and ROLE values"
  (abbreviate-with-roles c nil))

(defun abbreviate-with-roles (c roles)
  (cond
   ;; a constit returns its category and Variable name if it is specified
   ((constit-p c)
    (cons (constit-cat c) 
          (let ((v (get-value c 'w::var))) 
            (if (and v (not (var-p v)))
              (list :VAR v)))
              ))
   ;;  a list consists of the mother constit followed by children (if any) If no children, return the input
   ((consp c)
    (let ((con (car c)))
      (cons (if (constit-p con) (constit-cat con) 'ERROR)
            (if (and (consp (cdr c)) (> (list-length (cdr c)) 0))
              (let ((v (get-value con 'w::var)))
                (multiple-value-bind
                  (role-features roles-to-pass-on)
                  (extract-roles con roles)
                  (append (if (and v (not (var-p v))) (list :VAR v)) 
                          (if role-features (list :ROLES role-features))
                          (list :SUB (mapcar #'(lambda (x) (abbreviate-with-roles x roles-to-pass-on))
                                                                             (cdr c))))
                  ))
              (let ((input (remove-if #'null (get-value con 'w::input))))
                (append
                 (if input (list :INPUT (get-value con 'w::input))) 
                 (list :LF (get-value con 'w::lf))
                 ))))))
   (t c)))

(defun extract-roles (c roles)
  "Returns two values: 1) a list of roles that this constit is involved in, 
     and 2) the roles to pass on to subconstituents"
 (if (constit-p c)
   (let* ((lf (get-value c 'w::LF))
          (newroles (if (constit-p lf) (get-value lf 'w::CONSTRAINT)))
          (var (get-value c 'w::VAR))
          (participating-roles (remove-if-not #'(lambda (con)
                                                 (and (consp con) (eq (third con) var)))
                                              roles))
          (remaining-roles (remove-if #'(lambda (con)
                                                 (and (consp con) (eq (third con) var)))
                                              roles))
          )
     (values (mapcar #'(lambda (x) (list (first x) (second x)))
                     participating-roles)
             ;; If we have new roles, we used them, otherwise we pass on the old ones
             (if (and (consp newroles) (eq (car newroles) '&))
                                                  (cdr newroles) remaining-roles)))
    
   (values nil roles)))
               
                
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

;;  This returns any values that is in  the interval (start end)

(defun filter-objects (elements start end)
   (mapcar #'(lambda (x)
	      (indexedObject-object  x))
	   (remove-if #'(lambda (e)
			  (or (< (indexedObject-start e) start)
			      (> (indexedObject-end e) end)))
		      elements)))

	 
;; =
;; P
;; =

;; This is a sentence-at-a-time access to the system, allowing the 
;; user to type in (p "this is my sentence"), and get a parse back.

;; The assumption is that however the system is currently being run,
;; for this parse, you want the parser to run in standalone mode,
;; which means that in the (parse) function, certain messages aren't
;; sent.  Specifically, it supresses TAKE-TURN and END-TURN messages.

(defun p (x)
  ;; Store old *standalone* state to be restored after sentence is 
  ;; parsed, and set the new *standalone* state to t to suppress messages.
  (let ((old (standalone *chart*)))
    (setf (standalone *chart*) t)

	;; Tell the parser, through this call to parse, to prepare for 
	;; processing an utterance
	
    (parse '(start-sentence))
	
	;; Tell the parser that it has input words coming in.
    
    (if *no-positions-in-lf*
	(parse (list 'word x))
	(mapcar #'parse  (generate-positional-words x 0)))
			
	
	;; return suggests to the parser that it should find a parse,
	;; and return it.  This then prints it nicely :-)
    (print-nicely (parse '(end-sentence)))
	
	;; restore the state of the *standalone* variable.
    (setf (standalone *chart*) old)))

(defun generate-positional-words (x n)
  (let ((wordstring  (coerce (string-upcase (String-trim '(#\space #\Tab #\Newline #\return) x)) 'list)))
    (generate-positional-word wordstring n)))

(defun generate-positional-word (chars n)
 "generate indexed words starting at position N"
  (when chars
    (let* ((nextbreakindex (position-if #'(lambda (x) (member x *break-chars*)) chars))
	   (nextwordindex (if nextbreakindex 
			    (if (eq nextbreakindex 0) 1 nextbreakindex)))  ;; if we get 0, the the next item is the punctuation
	 )
      (let*
	  ((word (coerce (if nextwordindex (butlast chars (- (length chars) nextwordindex))
			   chars) 'string))
	   (rest (if nextwordindex (subseq chars nextwordindex)))
	   (end (if nextbreakindex (+ n nextwordindex)
		    (+ n (length chars)))))
	;;(format t "~% break=~S index=~S word=~S rest=~S end=~S ~%" nextbreakindex nextwordindex word rest end)
	;; skip over blanks
	(loop while (eql (car rest) #\space)
	   do
	     (setq end (+ end 1))
	     (setq rest (cdr rest)))
	(if rest
	    (cons (list 'word word :frame (list n (- end 1)))
		  (generate-positional-word rest end))
	    (list (list 'word word :frame (list n end))))))))

(defun pe (entries)
  (parse '(start-sentence))
  (mapcar #'parse entries)
  (print-nicely (parse '(end-sentence))))

(defun parse-to-lfs (text)
  "this takes a text string or a list of preprocessed WORD and PREFER lists and returns a list of LFS"
  (let* ((result (parse-text (if (stringp text) (remove-if-not #'graphic-char-p text)
					 text))))
    (if (eq (car result) 'PARSER::COMPOUND-COMMUNICATION-ACT)
	(mapcan #'(lambda (sa) (extract-lfs (find-arg-in-act sa :terms)))
		(find-arg-in-act result :acts))
	(extract-lfs (find-arg-in-act result :terms)))))


(defun p-graph (str)
 (let ((old (standalone *chart*)))
   (setf (standalone *chart*) t)
   (comm::send 'parser '(tell :receiver im :content (started-speaking :channel desktop :direction input :mode text :uttnum 1)))
   (parse '(start-sentence))
   (parse (list 'word str))
   (send-new-speech-act (parse '(end-sentence)))
   (setf (standalone *chart*) old)
   ))

(defun c (&optional n)
  (continue-parse-and-return-nth n))

(defun lg (&optional genre)
  (load-grammar :genre genre)
  )

(defun continue-parse-and-return-nth (n)
  (let ((old (standalone *chart*)))
    (setf (standalone *chart*) t)
    (print-nicely (parse `(continue :index ,n)))
    (setf (standalone *chart*) old)))

(defun start-test (x)
  (parse '(start-sentence))
  (parse (list 'word x)))

(defun end-test (x)
  (parse (list 'word x))
  (parse '(end-sentence)))

(defvar *test-mode* 'print)
;;

;;===========
;;
;;   Code for Printing out speech acts in readable format
;;

(defun print-nicely (x)
  "takes a speech act of form (name :slot1 val1 ....) and prints it out readably"
  (format t "~&~S" (car x))
  (print-pairs (cdr x)))


(defun print-pairs (x)
  (when x
    (Format t "~%~T~S" (first x))
    (if (consp (second x))
      (pprint (second x))
      (format t " ~S" (second x)))
    (print-pairs (cddr x))))


(defun contains-speech-acts (output)
  (AND
   output
   (consp output)
   (OR
    (bare-speech-act output)
    (AND
     (eq (car output) 'compound-communications-act)
     (member-if #'bare-speech-act (third output))
     ))))

(defun bare-speech-act (sa)
  (AND
   (listp sa)
   (eq (car sa) 'SPEECH-ACT)
   ))


(defun plf (x)
  "Run P function to parse input string X, suppressing its output, then LF
function to print the logical form."
  ;; It would be more elegant to use a "discard" flavor of output-stream
  ;; rather than cons for the string, but this is not trivial in ANSI CL.
  (let ((*standard-output* (make-string-output-stream)))
    (p x))
  (lf))

(defun fixup-word-messages (msgs)
  "This removes all words of form <SIL> starting at position 1, and changes the start positions of
    all words that immediately follow the gap. It adds silences after the last words in each sequence 
    so that they all end at the same place. It also changes to score of all <SIL> to 1"
   ;; generate any final silences
  (multiple-value-bind
    (beginnings endings) ;; non-sil-beginnings)
    (find-beginnings-and-endings msgs)
    (let* ((max (apply #'max endings))
           (silences (mapcar #'(lambda (n)
                                 `(word "<SIL>" :frame (,(+ n 1) ,max) :score 1))
                             (remove-if #'(lambda (x) (eq  x max)) endings)))
	   )
      ;; MYROSIA's hack: supposedly we here know the ending of a sequence we want. So we get it as chart-input-end
      (set-chart-input-ends endings)

;; Myrosia's fix: never remove non-silent beginnings!
      (let* (
	     (New-msgs
	      (mapcar #'(lambda (x)
			  (let ((f (find-arg x :frame)))
			    (if (member (car f) beginnings) 
				(adjust-sil-score (replace-arg x :frame (list 1 (second f))))
			      (adjust-sil-score x))))
		      msgs)))
	(append new-msgs silences))
      ))
  )

(defun adjust-sil-score (msg)
  "Changes score of <SIL>'s to 1 so they don't degrade the parse score"
  (if (equal (find-arg msg 'word) "<SIL>")
    (replace-arg msg :score 1.0)
    (replace-arg msg :score (find-arg msg :score))))


(defun remove-initial-gaps (beginnings msgs)
  (when msgs
    (multiple-value-bind
      (pos remaining-msgs)
      (remove-initial-gaps beginnings (cdr msgs))
      (let* ((msg (car msgs))
             (f (find-arg msg :frame)))
        (if (and (member (car f) beginnings)
                 (equal (second (car msgs)) "<SIL>"))
          
          (values (cons (+ (second f) 1) pos)
                  remaining-msgs)
          (values pos (cons msg remaining-msgs)))))))

;; Modified by Myrosia to separately find the non-silent beginnings
(defun find-beginnings-and-endings (msgs)
  "This finds the beginnings and endings of all possible chains though the lattice"
  (let* ((beginnings (remove-duplicates (mapcar #'(lambda (msg) (car (find-arg msg :frame))) msgs)))
         (beginnings-minus-1 (mapcar #'(lambda (x) (- x 1)) beginnings))
         (endings (remove-duplicates (mapcar #'(lambda (msg) (second (find-arg msg :frame))) msgs)))
	 (latbeginnings  (mapcar #'(lambda (y) (+ y 1))
				  (remove-if #'(lambda (n) (member n endings)) beginnings-minus-1))
			  )
	 )
    
    (values
     latbeginnings
     (remove-if  #'(lambda (n) (member n beginnings-minus-1)) endings)
     (remove-if (lambda (x) (every (lambda (msg) (or (not (eql (car (find-arg msg :frame)) x)) 
						     (string= (second (find-arg msg :word)) "<SIL>"))
					   )
				   msgs))
		latbeginnings)
     )
    ))
