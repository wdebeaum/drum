(in-package "PARSER")
 
;;;======================================================================
;;; NLP code for use with Natural Language Understanding, 2nd ed.
;;; Copyright (C) 1994 James F. Allen
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 MassAve, Cambridge, MA 02139, USA.
;;;======================================================================
;;
;;  A SERIAL BU PARSER FOR USE IN SPEECH APPLICATIONS, WHERE WORDS
;;   appear incrementally and sentence boundaries are not explicit.

;;  Notes on the basic best-first bottom-up parsing algorithm.
;;    This uses a best-first to build a chart 
;;    To run the parser, you must have made a lexicon and grammar "active" using the
;;    functions make-grammar and make-lexicon. (see file "GrammarandLexicon" for code)
;;
;;  e.g.,  Here is a sample session, assuming *lexicon1* and *grammar1* are
;;              appropriately defined
;;          (make-lexicon *lexicon1*)
;;          (make-grammar *grammar1*)
;;

;;  There are two functions to invoke the parser.

;;  START-BU-PARSE initializes the chart and then runs the parser on
;;      the words that it is passed. e.g. (start-BU-parse '(the train))

;;  CONTINUE-BU-PARSE continues the parse with additional words,
;;     e.g., (continue-BU-parse '(went to Dansville))

;; 

;;==================================================================================
;;
;;  MANAGING THE NUMBER OF WORDS

(defvar *agenda-trace* nil)
;;(defvar *constituent-count* 0)


#||(let (
      (max-position-found 0)
      (min-position-found 10000)
      ;; MYROSIA's vars
      (chart-input-ends '(0))
      (chart-ends-calculated nil)
      )
 ||#
(defun set-word-count (n)
    (if (numberp n)
	(setf (word-count *chart*) n)))
  
  (defun reset-word-count ()
    (setf (word-count *chart*) 0)
    (setf (max-position-found *chart*) 0)
    (setf (min-position-found *chart*) 10000)
    ;; MYROSIA's vars updates
    (setf (chart-input-ends *chart*) '(0))
    (setf (chart-ends-calculated *chart*) nil)
    )
  
  (defun up-word-count ()
    (let ((x (word-count *chart*)))
      (setf (word-count *chart*) (+ 1 (word-count *chart*)))
      (if (> (word-count *chart*) (max-position-found *chart*))
	  (setf (max-position-found *chart*) (word-count *chart*)))
      x)
    )
    
  (defun get-max-position nil
    (max-position-found *chart*))
      
  (defun get-min-position nil
    (min-position-found *chart*))
  
  (defun up-max-position nil
    (let ((x (max-position-found *chart*)))
      (setf (max-position-found *chart*) (+ 1 (max-position-found *chart*)))
      x)
    )
  
  (defun update-max-position-found (n)
    (if (> n (maX-position-found *chart*))
	(setf (max-position-found *chart*) n)))
  
  (defun update-min-position-found (n)
    (if (and (< n (min-position-found *chart*)) (> n -1))
	(setf (min-position-found *chart*) n)))
  
  (defun reset-max-position (n)
    (setf (max-position-found *chart*) n))

  (defun reset-min-position (n)
  (setf (min-position-found *chart*) n))
  
  ;;  word count is only decreased if the parser has to back up because of
  ;;   backspacing in the input
  (defun decrease-Word-Count (N)
    (setf (word-count *chart*) (- (word-count *chart*) N))
    (if (< (word-count *chart*) 0) (setf (word-count *chart*) 0))
    (setf (max-position-found *chart*) (word-count *chart*)))
  
  
  ;; MYROSIA's vars update
  (defun get-chart-input-ends () (chart-input-ends *chart*))
  (defun set-chart-input-ends (ends)
    (setf (chart-ends-calculated *chart*) t)
    (setf (chart-input-ends *chart*) ends))
  
  (defun new-chart-input-end (n)
    (when (and (not (chart-ends-calculated *chart*))
	       (> n (first (chart-input-ends *chart*))))
      (setf (chart-input-ends *chart*) (list n))))
 

;;==================================================================================
;;THE AGENDA
;;   The agenda is a prioritized list organized into buckets to speed up management of the lists.

;;    use the variable AGENDA
;;  AGENDA is a 10 element array - each containing an ordered list of the entries with a score in the appropriate percentile

(defvar *number-of-buckets-for-agenda* 200)

(defvar *score-length-multiplier* 0.2)
(defvar *score-corner-multiplier* 0.0)
(defvar *score-probability-threshold* 0.5)
(defvar *rules-per-word* 2)
(defvar *word-length*)
(defvar *default-word-length* 6) ;; generally, default word length is only used in text
(defvar *rule-multiplier* 0.99)
(defvar *use-tags-as-filter* nil)
(defvar *use-senses-as-filter* nil)
(defvar *bad-tag-multiplier* .95)
(defvar *bad-sense-multiplier* .95)

(defun set-score-to-prob-only ()
  "A convenience function. Sets the parameters so that the score is calculated by probability only"
  (setq *score-probability-threshold* 0)
  (setq *score-length-multiplier* 0)
  )

(defun probability-score (i)
  (if (> *score-probability-threshold* 0)
      (max 0 (/ (- (agenda-item-prob i) *score-probability-threshold*) *score-probability-threshold*))
    (agenda-item-prob i)
    ))

(defun probability-entry-score (i)
  (if (> *score-probability-threshold* 0)
      (max 0 (/ (- (entry-prob i) *score-probability-threshold*) *score-probability-threshold*))
    (entry-prob i)
    ))

(defun arc-length-score (i)
  (let* ((start (or (agenda-item-start i) 1))
	 (entry-end (entry-end (agenda-item-entry i))))
  (compute-length-score (or (if (numberp entry-end)
				(- entry-end start))
			    1))))

(defun length-entry-score (e)
  "computes a factor based on average prob. of a constituent of length L"
  (compute-length-score (entry-size e)))

(defun compute-length-score (size)
  "computes a factor based on average prob. of a constituent of length L:
      MUST return a score between 0 and 1"
  (let* ((number-constit (/ (or size 1) *word-length*))
	 (raw-factor (compute-log-factor number-constit))
	 (over-one (- raw-factor 1))
	 (factor (if (>= *score-length-multiplier* 1)
		     	     raw-factor
		     (if (> *score-length-multiplier* 0)
			 (+ 1 (* *score-length-multiplier* over-one))
			 1))))
    (min (- (max factor 1) 1) 1)
  ))

(defun compute-log-factor (n)
  (if (< n 2.7) 1
      (log n)))
    #||  (let ((ll (- (log n) 1)))
	(+ 1 (/ ll 3)))))||#

(defun corner-score (i)
  )

(defun calculate-score (i)
  "This function computes the score for AGENDA_ITEM I for placement in the agenda. You may modify this to
     experiment with different scoring functions"
  (cond
   ((eql (agenda-item-start i) (agenda-item-end i)) 1) ;;  insurance check
   (t (min #|(* (probability-score i) 
	      (arc-length-score i)) 1))))|#
       (+ (probability-score i) 
	  (* (- 1 (probability-score i)) (arc-length-score i))) 1))))

(defun calculate-entry-score (e)
  "This is used for final output, so need not be efficient"
  (calculate-score
   (make-agenda-item :entry e :start (entry-start e) :end (entry-end e) :prob (entry-prob e))))


(defun show-agenda nil
  (list (top-bucket *chart*) (agenda *chart*)))
 

  ;;  INIT-AGENDA
  
(defun init-agenda (number-of-buckets)
    "Initializes the agenda data structure"
    (if (numberp number-of-buckets)
	(progn
	  ;; MYROSIA's change: threshold set to 0 - will break things if we want smth else here
	  (setThreshold 0)
	  ;;(setq *number-of-buckets-for-agenda* number-of-buckets)
	  (setf (top-bucket *chart*) 0)
	  (setf (agenda *chart*) (make-array (+ number-of-buckets 1) :initial-element nil))
	  )
      (parser-warn "Init-agenda called with non numberic arg: ~S. it was ignored" number-of-buckets))
    
    )
   
  (defun add-to-agenda (entry)
    "Add an entry onto the agenda"
    (when entry
      (when (and (eq *agenda-trace* 'ADD)
		 (entry-p entry))
	(format t "~%Making ENTRY ~S into agenda item with prob ~S ..." (entry-name entry) (entry-prob entry)
		))	
      (Cond
       ((Entry-P entry) (compute-score-and-add (make-agenda-item
						:type 'entry :prob (entry-prob entry) :entry entry
						:Id (Entry-name entry)
						:Start (Entry-start entry) :end (entry-end entry))))
       ((agenda-item-p entry)
	(compute-score-and-add entry))
       (t (break "Bad call to add-to-agenda: ~S" entry)))
      ))
  
  (defun compute-score-and-add (i)
    (if (>= (agenda-item-prob i) (entry-threshold))
	(let* ((score (calculate-score i))
	       (bucket (max 0 (min (floor (* score (/ *number-of-buckets-for-agenda* 2)))
				   *number-of-buckets-for-agenda*)))
	       ;;(bucket (min (floor (* prob *number-of-buckets-for-agenda*)) *number-of-buckets-for-agenda*))
	       )
	  
	  (when (eq *agenda-trace* 'ADD)
	    (format t "~%Adding ~S ~S from ~S to ~S. p=~Ss=~S~%"
		    (agenda-item-type i) (agenda-item-id i) 
		    (agenda-item-start i) (agenda-item-end i) (agenda-item-prob i) score)
	    )
	  (setf (agenda-item-score i) score)
	  ;;(format t "%Bucket is ~S with size ~S~%" bucket (length (aref (agenda *chart*) bucket)))
	  (if (< (length (aref (agenda *chart*) bucket)) 4000)
	      (setf (aref (agenda *chart*) bucket)
		     (insert-in-agenda i score (aref (agenda *chart*) bucket)))
	      ;;(format t "~%Warning: bucket exceeds 4000 elements"))
	      )

	  (if (> bucket (top-bucket *chart*))
	      (setf (top-bucket *chart*) bucket)
	    ))
      ;;	(format t "~% DIDN'T ADD THE ENTRY ~S because it is below threshold ~S ~%" entry (entry-threshold))
      ))

  (defun insert-in-agenda (item score agenda-bucket)
    (cond ((null agenda-bucket) (list item))
          ((> score (agenda-item-score (car agenda-bucket)))
           (cons item agenda-bucket))
          (t (cons (car agenda-bucket) 
                   (insert-in-agenda item score (cdr agenda-bucket))))))

  (defun get-next-entry nil
    (let ((entry (car (aref (agenda *chart*) (top-bucket *chart*)))))
      (pop (aref (agenda *chart*) (top-bucket *chart*)))
      (when (null (aref (agenda *chart*) (top-bucket *chart*)))
        (setf (top-bucket *chart*) (find-new-top-bucket (top-bucket *chart*)))
        )
      ;;      (format t "~% exploring entry ~S~%" entry)
      ;; (format t "~% exploring entry ")
      ;; (show-entry-with-name entry '(lex))      
      entry))

  (defun find-new-top-bucket (n)
    (if (> n 0)
      (if (aref (agenda *chart*) n) n (find-new-top-bucket (- n 1)))
      0))
  
(defun peek-agenda nil
    (car (aref (agenda *chart*) (top-bucket *chart*))))
  
(defun empty-agenda nil
    (null (aref (agenda *chart*) (top-bucket *chart*))))
  
(defun agenda-item-pending (threshold)
    "this checks that there is an item on the agenda that is above the
      current threshold. Threshold is relative to length of the utterance
      and is halved every *decline-unit* steps"
    (when (< (numberEntries *chart*) (getmaxnumberentries))
      (let ((e (car (aref (agenda *chart*) (top-bucket *chart*)))))
	(if e
	    (>= (agenda-item-score e) threshold)
	    ;; e is empty (should not happen but does very rarely), reset the top
	    (progn 
	      (trace-msg 2 "~%~% TOP BUCKET WAS EMPTY -- RESETTING~%~%")
	      (setf (top-bucket *chart*) (find-new-top-bucket (top-bucket *chart*)))
	      (if (> (top-bucket *chart*) 0) (agenda-item-pending threshold))
	      )))))

(defun show-rule-in-agenda (rule start end)
  (dotimes (i *number-of-buckets-for-agenda*)
    (find-arc-in-agenda (aref (agenda *chart*) i) rule start (+ end 1) i)))

(defun find-arc-in-agenda (bucket rule-id  start end buck-num)
  (mapcar #'(lambda (x) (format t "~%bucket ~S:~S" buck-num x))
	  (remove-if-not #'(lambda (b)
			     (and (eq (agenda-item-start b) start)
				  (eq (agenda-item-end b) end)
				  (eq (agenda-item-id b) rule-id)))
			 bucket)))
	  

(defun remove-blanks (chars)
  (when chars
    (if (eql (car chars) #\space)
	(cons #\_ (remove-blanks (cdr chars)))
      (cons (car chars) (remove-blanks (cdr chars))))))
  ;; =====================
  ;; gen-start-indices-for-word
  ;; =====================
  
  ;; This is mostly a copy of the last chunk of the code in the previous function,
  ;; except that its job is just to associate the appropriate start and end times 
  ;; with the word, that can be passed on to the continuation, so the parser knows
  ;; where the word goes when the lexical look-up message call to the lexiconManager
  ;; comes back.
  
  ;; This takes a bare word and specifies the necessary information on it to make it 
  ;; look like it might have come from a lattice.  
  
  (defun gen-start-indices-for-word (word)
	
	;; set the start position to the latest time index seen, and update various 
	;; variables so that the indices for later words will work out.
	;; the +20 is an arbitrary number given to words that haven't come from a lattice.
	
	(let* ((start (up-max-position))
	      (end  (+ start  (length (symbol-name word)) 1)))
	  (update-min-position-found start)
	  (update-max-position-found end)
	  ;; MYROSIA's vars updates
	  (new-chart-input-end end)
	
	  ;; Find the lexical entries for this item.
	  (list  word  start end)
	  )
	)
  
  (defun find-value (fvlist f)
    (cond ((null fvlist) nil)
	  ((eq (car fvlist) f)
	   (cadr fvlist))
	  (t (find-value (cddr fvlist) f))))
  
;; FIND-LEX-ENTRIES checks the first word in a word list and returns
;; all single entries found in the lexicon. The second
;; argument gives the entry position in the chart. If no
;; pattern found, it returns an *unknown-word* entry.

;; ================
;; find-lex-entries
;; ================

;; grabs and processes all lexical entries (e.g. all senses) for the 
;; input word, applying the filter passed in
(defvar *duplicate* nil)

(defun find-lex-entries (word start end prob others)
  ;; first make sure we haven't already added it (this happens when we get input form different sources
  (let* ((entries nil)
	 ;;(index-term (list start end word))
	 (*duplicate* nil)
	 (extrafeats (find-arg others :feats))
	 ;;(domain-specific-info (find-arg others :domain-specific-info))
	 (wn-senses (find-arg (car (find-arg others :sense-info)) :wn-sense-keys))
	 (wn-domain-info  (if wn-senses (list (cons 'WN wn-senses)))))
			  
    (mapcar #'(lambda (lex-entry)
		(when (lex-entry-is-new lex-entry word start end)
		    (cond
		      ;;  Normal case:  we have a lex-entry struct.
		      ((lex-entry-p lex-entry)
		       (let ((id (lex-entry-id lex-entry))
			     (domain-info (or (get-value (lex-entry-constit lex-entry) 'w::kr-type)
					      wn-domain-info)))

			 (setq entries (cons (build-entry 
					      (instantiateVAR (append-features (set-domain-info word (copy-constit (lex-entry-constit lex-entry)) 
											    domain-info)
										 extrafeats))
					  start end nil 
					  id
						;;  PROB is the probability passed in as an arg, 
						;; and named entities (NNP) without domain specific into are a bit suspect
					  (rescore-lex-entry lex-entry prob)
					  nil
					  (list (constit-cat (lex-entry-constit lex-entry))))
					 entries))))
		  ;;  We have a composite  - need to add an arc to the chart
		  ((and (consp lex-entry) (eq (car lex-entry) 'composite))
		   (add-arc-for-composite-word word start (second lex-entry) (third lex-entry)))
		  (t 
		   (parser-warn "Illegal lexicon entry found: ~S" lex-entry)
		   nil))))
	    ;; END LAMBDA FUNCTION
	    (lib-retrieve-from-lex word others)
)
    (when (and (null entries) (not *duplicate*))
      (parser-warn "Unknown word ~s" word)
      (setq entries (list (build-entry (build-constit 'UNKNOWN (list (list 'w::input (list word))) nil)
                                       start end  nil 'UNK 1 nil 'w::unknown))))
    entries))

(defun lex-entry-is-new (le word start end)
  t)
#||(if (lex-entry-p le)
      (let* ((lexe (lex-entry-constit le))
	     (domain-info (get-value lexe 'w::kr-type))
	     (index-term (list (list word start end (constit-cat (lex-entry-constit le)))
			       (get-value lexe 'w::LF)
			       (let ((sem (get-value lexe 'w::sem)))
				 (if (not (or (eq sem '-) (and (var-p sem) (eq (aref (var-values sem) 0) '-))))
				     sem))
			       domain-info)))
	(if (match-lex-entries index-term (gethash word (lex-items *chart*)))
	    (progn 
	      (FORMAT T "~%~%Removing duplicate lexical item ~S" index-term)
	      (trace-msg 2 "Removing duplicate lexical item ~S" index-term)
	      (setq *duplicate* t)
	      (if domain-info (push index-term (suppressed-lex-entries *chart*)))
	      nil)
	    (push index-term (gethash word (lex-items *chart*)))))
      t))||#
     
(defun match-lex-entries (index terms)
  (when terms
    (let ((term (car terms)))
      (if (and (not (member (fourth (car index)) '(W::V W::ADV W::ADJ W::PRO W::QUAN W::ART)))
	       (equal (car term) (car index))  ;; words are identical
	       (equal (cadr index) (cadr term))  ;; LFs are identical
	       (or (null  (cadddr term))
		   (equal (cadddr index) (cadddr term)))  ;; domain-info is identical
	       (or (and (null (caddr index))       ;; SEMS are defined and identical
			(null (caddr term)))
		   (multiple-value-bind
		       (val bndgs prob)
		     (unify-sems (caddr index) (caddr term))
		   (and val (or (null prob) (= prob 1))))))
	t
	(match-lex-entries index (cdr terms))))))

(defvar *domain-boosting-factor* nil)
(defvar *no-krtype-name-penalty* .95)

(defvar *referential-sem-penalty* .95)

(defun rescore-lex-entry (le prob)
  "here we rescore proposed lex constituents, downgrading the ones that appeared generic and have no domain info,
     except for cases where REFERENTIAL-SEM is truly the right interpretation (e.g., pronouns and a few cases like ONE as a noun)"
  (let* ((c (lex-entry-constit le))
	 (lf (get-value c 'w::lf))
	 (lex (get-value c 'w::lex))
	 (score
	  (* (if (and (not (eq (constit-cat c) 'w::pro))
		      (not (member (get-value c 'w::lex) '(w::one)))
		      (or (if (symbolp lf) (member lf '(ont::referential-sem ont::modifier)))
			  (if (consp lf) (intersection lf '(ont::referential-sem ont::modifier)))))
		 ;; addition penalty if it contains a hyphen!
		 (if (position #\- (coerce (symbol-name lex) 'list))
		     (* *referential-sem-penalty*  *referential-sem-penalty*)
		      *referential-sem-penalty*)
		      1)
		  prob (lex-entry-prob le))))
    ;; now boost entries with domain specific info
    (normalize
     (if (get-value c 'w::kr-type)
	 (boost-by-percent score *domain-boosting-factor*)
	 score))
    ))

(defun boost-by-percent (prob factor)
  (if (numberp factor)
      (progn
	(if (> factor 1) (setq factor (- factor 1)))  ;; we do this to catch left over numbers from prior scoring method
	(let ((diff (- 1 prob)))
	  (+ prob (* diff factor))))
      (progn
	(format t "~% ERROR: bad argument to BOOST-BY-PERCENT: ~S" factor)
	prob)))


(defun normalize (x)
  x)
#||  "reduces a number to two decimal point precision"
  (coerce (/ (round x .0001) 10000) 'short-float))||#

(defvar *wn-wsd-enabled* nil)   ;; turns on WSD to WN sense

(defun set-domain-info (word c sense-info)
  ;; note::this works because array operations are destructive
  (let ((sem (get-value c 'w::SEM))
	(lf (get-value c 'W::LF))
	)
    (when (and (var-p sem) (consp lf)) ;;(not (member (third lf) '(W::BE W::HAVE W::BEING W::HAVING))))
	  
      (if (and *wn-wsd-enabled* (member 'wn sense-info))  ;; here we try to guess the best sense from WN
	  (let ((sense
		 (multiple-value-bind (core non-core)
		     (get-wordnet-sense-keys word (list (constit-cat c)))
		   
		   (let* ((reduced-senses (filter-senses-by-cat (append (cadr core) (cadr non-core)) (constit-cat c)))
			  (lftype (second lf))
			  (best-matches (find-sense-matching-lf reduced-senses lftype)))
		    
		     ;; use a core sense if possible
		     (or (caar (intersection (cadr core) best-matches :test #'equal))
			 (caar best-matches))))))
		(if sense
		    (set-kr-type sem (append sense-info (list (list 'WN (list sense)))))
		    (set-kr-type sem sense-info)))
	  (set-kr-type sem sense-info)))
    c))


(defun get-wordnet-sense-keys (word cats)
  "cleans up sensekeys"
  (when wf::*use-wordfinder*
    (multiple-value-bind (core noncore)
	(wf::wordnet-sense-keys-for-word word cats)
    (values (list :core (mapcar #'(lambda (triple) (reuse-cons (cleanup-sense (car triple)) (cdr triple) triple))
		  (cadr core)))
	    (list :non-core (mapcar  #'(lambda (triple) (reuse-cons (cleanup-sense (car triple)) (cdr triple) triple))
				    (cadr noncore)))))))

(defun cleanup-sense (sense)
  "removes odd things like the '(p)' modifier that gets added in wordnet"
  (let ((sensechars (coerce sense 'list)))
    (if (not (member #\( sensechars))
	sense
	(let* ((pos (position #\( sensechars))
	      (lengthtoend (- (list-length sensechars) pos))
	      (prefix (butlast sensechars lengthtoend))
	      (suffix (last sensechars (- lengthtoend 3))))
	  (coerce (append prefix suffix) 'string)))))
	
	

(defun filter-senses-by-cat (results cat)
  (remove-if-not #'(lambda (x) (or (eq (second x) cat)
				   (and (eq (second x) 'W::N) (eq cat 'W::name))))
	     results))

(defun find-sense-matching-lf (senses lf)
  (or (remove-if-not #'(lambda (x) (equal (third x) lf)) senses)
      (remove-if-not #'(lambda (x) (subtype-check (third x) lf))
			   senses)
      (remove-if-not #'(lambda (x) (subtype-check lf (third x)))
				   senses)
      senses))

(defun set-kr-type (sem domain-info)
  (cond ((arrayp (var-values sem))
	 (set-feature-values-in-array (list (list 'f::kr-type domain-info)) (var-values sem)))
	;; the SEM feature is a variable
	((var-p sem)
	 (setf (var-values sem)
	       (read-expression `($ ?x (F::kr-type ,domain-info)))))
	
	 ))
     
(defun get-lf-type (x)
  (if (consp x)
      (second x)
      x))
;;===========

(defun add-arc-for-composite-word (word start remaining-words lex-entry)
  "This adds an active arc to the chart looking for compound entry at the start of the first word. When found
    the arc will produce the constit. NB: we depend on the fact that we just built the
    mother CONSTIT for this expression, so that the var-table is set correcting for any
    variables used in the word entries."
  (let* ((id (lex-entry-id lex-entry))
         (rhs (mapcar #'(lambda (w)
                          (if (symbolp w)
                            (make-constit :cat 'w::word :feats (list (list 'w::lex w)))
                            (read-value w id)))
                      (cons word remaining-words)))
          (arc (make-arc :mother (instantiatevar (lex-entry-constit lex-entry))
                        :post RHS
                        :start start
                        :end start
                        :prob (/ (rescore-lex-entry lex-entry 1) (ntimes .99 (length remaining-words)))  ;; we boost the rule prob to compensate for the penalties from each individual word
                        :rule-id id)))
    ;; (mapc #'(lambda (x) (lexiconmanager::add-word-def-if-necessary x 'w::word)) remaining-words)  ;; make sure each word is defined in lexicon
    (add-arc arc)
    ))

(defun ntimes (p n)
  (if (> n 0)
      (* .99 (ntimes p (- n 1)))
      p))
  
(defun append-features (constit feats)
  "adds feature values to a constituent unless they are already defined"
  (if feats
      (let ((feat (caar feats))
	    (val (cadar feats)))
	(if (not (get-value constit feat))
	    (append-features (add-feature-value constit feat val) (cdr feats))))
    constit))
			  
		      
;;===========================================================================
;;   The PARSER
;;   This parser accepts words incrementally until it is reset

  ;; =================
  ;; continue-BU-parse
  ;; =================
  
  ;; This adds any new input to the chart and then enters the parse loop,
  ;; parsing until no more agenda items are above threshold.
  

  (defun continue-BU-parse nil
    "parses until no constituents on the agenda are above threshold or until max # constituents are built"
    (setf (StopFlag *chart*) nil)
    (BU-parse-loop (threshold *chart*)))

  ;; ================
  ;; do-one-step-parse
  ;; ================
  
  ;; Same as continue-BU-parse, but only does one step of the parse instead
  ;; of entering the loop.
  
  (defun do-one-step-parse (words &optional chart)
    "Adds any new input to the agenda and performs one parse operation"
    (if chart
	(instantiate-chart chart))
    (add-new-input words)
    ;;(format *trace-output* "Added words ~S" words)
    (if (agenda-item-pending (threshold *chart*))
	(process-agenda-item (get-next-entry)))
    )

  ;; =============
  ;; BU-parse-loop
  ;; =============
  
  ;; This loops the parser.  Presumably this is only called when the main
  ;; event-loop in messages.lisp is NOT running, but I need to check that 
  ;; out still.  ScS June 12, 2003.
  
  
  ;; This loops the parser until the StopFlag variable is set
  ;; or until it runs out of stuff to do.
  
  (defun BU-parse-loop (threshold)
    "Keeps adding constituents to chart as long as they are above threshold"
    (loop 
      (cond
       ;; return nil if StopFlag has been set
       ((StopFlag *chart*)
	(return nil))
	   
       ;; If there is an agenda-item above threshold, process it.
       ((agenda-item-pending threshold)
	(process-agenda-item (get-next-entry)))
       
       ;;   parse is not finished but there's nothing to do right now,
       ;; so retun from this function.
       (t (return nil)))))
  
  (defun setThreshold (x)
    (if (and (numberp x) (>= x 0))
	(setf (threshold *chart*) x)))

  (defun entry-threshold ()
    (threshold *chart*))
  
  (defun suspendParse nil
    (setf (StopFlag *chart*) t))

;;  ) ;; end scope of StopFlag


(defun process-agenda-item (item)
  (if (not (agenda-item-p item)
	   )
      (break "Bad item: ~S" item))
  (when *agenda-trace*
    ;; (format t "Processing ~S length ~S score ~S~%" (agenda-item-type item) (- (agenda-item-start item) (agenda-item-end item)) (agenda-item-score item)))
    (format t "~%Processing ~S  ~S from ~S to ~S.p=~s,s=~S~%" 
	    (agenda-item-type item) (agenda-item-id item) (agenda-item-start item) (agenda-item-end item) (agenda-item-prob item) (agenda-item-score item)))
  (case (agenda-item-type item)
      (entry 
       (Add-Entry-to-chart (agenda-item-entry item)))
      (arc
       (extend-arc-with-constit (agenda-item-entry item) ;;(agenda-item-id item)
				(agenda-item-arc item) (agenda-item-bndgs item) (agenda-item-prob item)))
      (new-arc
       (Opt-Make-arc-from-rule (agenda-item-arc item) (agenda-item-entry item)
			     ;;(agenda-item-id item)
			     (agenda-item-start item) (agenda-item-end item)
			     (agenda-item-bndgs item) (agenda-item-prob item)))
    (otherwise (break "UNknown agenda op: ~S" (agenda-item-type item)))
    ))
;;;

;; ADD-ENTRY-TO-CHART  first invokes any procedural filters, and then
;;    inserts the resulting entry into the chart, adding any new
;;    active arcs introduced by grammar rules that can start with the
;;    constituent, and extending any existing arcs that can be extended by
;;    the consituent. If the procedure downgardes the probabilty, however,
;;    the entry may be reinstated onto the agenda.

(defun add-entry-to-chart (entry)
  ;; We invoke the procedural filters, we may affect the entry PROB (and hence the score)
  (setq entry (procedural-filter entry))
  ;;check stopping condition
  (let ((sc (get-stopping-condition)))
    (if (and sc
	     (constit-match sc (entry-constit entry)))
	(suspendParse)))
  (if entry (add-entry-to-chart-continued entry))
  )

;; ADD-ENTRY-TO-CHART-CONTINUED

(defun add-entry-to-chart-continued (entry &optional no-agenda-check)
  (let ((next-score (let ((i (peek-agenda)))
		      (if (agenda-item-p i) (agenda-item-score i)
			  0)))
	(new-score (calculate-entry-score entry)))
      (cond

	;; If the new-score is still higher than the next-score, then we still add the
	;; constit to the chart, as done below.
	((or (>= new-score next-score) no-agenda-check)
	 (trace-entry entry)
	 (when (put-in-chart entry)
	   (setf (constituent-count *chart*) (+ (constituent-count *chart*) 1))
	   (Make-New-BU-Active-Arcs entry (entry-name entry))
	   (Chart-Extend entry (entry-name entry))))

	;; If the news-score is higher than the next-score, and still non-zero, then
	;; add it back to the agenda, to get re-processed once it is again the highest
	;; probability constituent.
	((> new-score 0)
	 (trace-msg 2 "Adding constit ~S back to agenda" (list (entry-name entry) (entry-prob entry)))
	 (add-to-agenda entry))

	;; Otherwise, we have a score of 0 or less for the constit, and so we will remove it
	;; from the agenda entirely, so as to not have to process it later.
	(t (trace-msg 2 "Dropping constit ~S since prob is 0" (list (entry-name entry) (entry-prob entry)))
	   ))
      ))



(defun Make-New-BU-Active-Arcs (entry name)
  (let ((c (entry-constit entry))
	 (start (entry-start entry))
	 (end (entry-end entry)))
     (mapcar #'(lambda (x)   ;; NB: x is a RULE structure
		 (multiple-value-bind (bndgs matchprob)
		     (Constit-Match (car (rule-rhs x)) C)
		   (let ((prob (* (if lxm::*kr-in-lexicon* lxm::*no-kr-probability* 1)
				  (compute-new-arc-prob x entry start bndgs)
				  (if matchprob matchprob 1))))
		     (if (and matchprob (< matchprob 1))
		       (trace-msg 2 "~%SEM unify failed - penalizing rule ~S extended with ~S with prob ~S" 
				  (rule-id x) name  prob)
		       (trace-msg 2 "~% rule ~S extended with ~S with prob ~S" 
				  (rule-id x) name  prob))
		     (when  bndgs
		       
		       (add-to-agenda (make-agenda-item :type 'NEW-ARC
							:prob prob 
							:arc x
							:entry entry
							:id (rule-id x)
							:start start
							:end end
							:bndgs bndgs))
		     ))))
	     (lookup-rules c))))

(defun Opt-make-arc-from-rule (rule entry start end bndgs prob)
  (Declare (Optimize (speed 3) (safety 0) (debug 0)))
  (let ((*vars-seen* nil))
    ;;  Add arc or new entry to agenda depending on whether arc is
   ;; completed or not
  ;;(trace-msg 2 "~%Starting arc ~S at ~S with entry ~S with prob ~S" (rule-id rule) start (entry-name entry) prob)
    (If (> (length (rule-rhs rule)) 1)
	 (extend-arc-with-constit
		       entry 
		       (make-arc-from-rule rule
					   (entry-start entry) bndgs)
		       bndgs
		       prob)

	 ;;   rule contains only one constit on RHS - just build new constit
	 ;;    rather than building an ARC and then extending it to build the constit
	 (let* ((copyrule (copy-vars-in-rule rule bndgs))
		(id (rule-id copyrule))
		(rhs (rule-rhs copyrule))
		(name (entry-name entry))
		(prob-aux nil)
		(foot-feats (augment-foot-feats entry (car (rule-rhs rule)) nil bndgs))
		(mother (merge-feature-values (rule-lhs copyrule) (cons (list 1 name) foot-feats))))
	   ;;(multiple-value-bind
	   ;;    (prob prob-aux)
	   ;;(compute-new-arc-prob rule entry start bndgs)
	   (Add-entry-to-chart (build-entry mother start end rhs id prob prob-aux (cons (constit-cat (entry-constit entry))
											(entry-first-cat entry)))
	   )))))
    
(defun chart-extend (entry name)
     (mapcar #'(lambda (x) (extend-arc entry name x)) 
             (get-arcs (entry-start entry))))


;;  PREPARSING AND FILTERING SYSTEM 

;;    As we deal with complex domains (like biology) we have to do some filtering of the set of options
;;    texttagger sends in order to control the search space, especially in dealing with complex names and sequences


;;(defvar *preparse-tracing-on* nil)    ;; OBSOLETE - now use regualt tracing facility
(defvar *message-cache* nil)
(defvar *filter-and-preparse-input* nil)

(defun trace-preparse (&rest args)
  (apply #'trace-msg (cons 1 args)))
  #||(if *preparse-tracing-on*
      (apply #'format (cons t args))
      ))||#

(defstruct message 
  content
  start
  end
  pos
  domain-specific-info
  ont-types
  word)

(defvar *preparses* nil)

(defun remember-preparse (seq root terms)
  (push (list (message-start seq) (+ (message-end seq) 1) (message-word seq) root terms) *preparses*)
  (trace-preparse "~%Storing preparsed segment for ~S. Segment is ~S" seq terms))


(defun cache-message (content)
  (let* ((args (cddr content))
	 (frame (find-arg args :frame))
	 (senses (find-arg args :sense-info))
	 (revised-senses (Elim-low-scores senses))
	 (ont-types (flatten (mapcar #'(lambda (s) (find-arg s :ont-types)) revised-senses))))
	 
	 
   ;; (format t "~%Ont types: ~S" ont-types)
    (add-message-to-cache (make-message :content (replace-arg content :sense-info revised-senses)
			       :start (car frame)
			       :end (cadr frame)
			       :word (if (eq (car content) 'word) (cadr content))
			       :pos (find-arg (car revised-senses) :trips-parts-of-speech)
			       :domain-specific-info (find-arg (car revised-senses) :domain-specific-info)
			       :ont-types ont-types))
    ))

(defun elim-low-scores (infos)
  (let ((halfmax (/ (find-max-score infos 0) 2)))
    (remove-if #'(lambda (x) (if (numberp (find-arg x :score))
				 (< (find-arg x :score) halfmax)))
	       infos)))

(defun find-max-score (infos best-so-far)
  (if infos
      (let ((sc (find-arg (car infos) :score)))
	(if (and (numberp sc) (> sc best-so-far))
	    (find-max-score (cdr infos) sc)
	    (find-max-score (cdr infos) best-so-far)))
      best-so-far))


(defun get-cached-messages-and-clear nil
  (setq *preparses* nil)
  (let ((r (if *filter-and-preparse-input*
	       (mapcar #'message-content (reverse (filter-cached-messages *message-cache*)))
	       (mapcar #'message-content (reverse *message-cache*)))))
    (setq *message-cache* nil)
    r))



(defun filter-cached-messages (messages)     ;; this is disabled and just returns messages
  (let* ((noms-with-senses (remove-if-not #'(lambda (x) (and (message-domain-specific-info x)
							     (intersection (message-pos x) '(W::NAME w::N))))
					  messages))
	 (non-subsumed-noms-with-senses (filter-out-messages #'(lambda (x y) 
							    (and (substretch x y)
								 (not-equal-content x y)))
							noms-with-senses noms-with-senses))
	 (non-subsumed (filter-out-messages #'(lambda (x y) (and (substretch x y)
								 (all-numbers (second (message-content x)))
								 (not (find-arg (message-content x) :sense-info)))) ;; only removing numbers not tagged with senses
					    *message-cache* non-subsumed-noms-with-senses)))
    (trace-preparse "~% Revised message cache after sense filtering: ~S" non-subsumed)
    ;;(mapcar #'refine-type-of-multi-words      ;; disabled this as it lessens the chance of getting a compositional meaning, e.g., "other sources" comes in as a multiword
	    (filter-sequences non-subsumed)))
;;)   ;; i removed non-subsumed here as we have problems with prefixed verb that occur in specialist! (e.h., hyperstimulateps has a tagged sense but stimulates does not, so it is removed
;;    and my second attempt fails on terms such as "threonine residue 185"

(defun all-numbers (str)
  (and str
       (every #'(lambda (n) (member n '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
	      (coerce str 'list))))

(defun filter-out-messages (fn list ref)
  "returns sublist that doesn't meet the specification in FN for any of the ref messages"
  (remove-if #'(lambda (x) (and (eq (car (message-content x)) 'word)
				(check-instance fn x ref))) list))

(defun check-instance (fn i ref)
  (some #'(lambda (y) (apply fn (list i y))) ref))

(defun substretch (s1 s2)
  (and (message-start s1)
       (message-end s2)
       (>= (message-start s1) (message-start s2))
       (<= (message-end s1) (message-end s2))))

(defun not-equal-content (s1 s2)
  (not (equal (message-content s1) (message-content s2))))


(defun add-message-to-cache (msg)
  (push msg  *message-cache*))

;; code to refine the type of multi words

(defun refine-type-of-multi-words (msg)
  (if (not (and (message-word msg) (message-ont-types msg)))
      msg
      ;;  otherwise check for multi-words
      (let ((words (tokenize (message-word msg))))
	(if (> (list-length words) 1)
	    (let* ((lastword (last words))
		   (defs (lib-retrieve-from-lex lastword nil))
		   (ont-types (remove-if #'null
					(mapcar #'extract-type-from-lex-entry defs))))
	      (if (and ont-types 
		       (om::subtype (car ont-types) (car (message-ont-types msg))))
		  (replace-pos-and-ont-type msg nil  (remove-duplicates ont-types))
		  msg))
	    msg))))

(defun extract-type-from-lex-entry (le)
  (let ((x (get-value (lex-entry-constit le) 'w::lf)))
    (if (consp x) (cadr x)
	x)))
      
      
(defun replace-pos-and-ont-type (msg pos ont-type)
  (let ((c (message-content msg)))
    (setf (message-content msg) 
	  (list* (car c) (cadr c) 
		 (replace-arg (cddr c) :sense-info
			      (mapcar #'(lambda (x) (replace-ont-in-sense x pos ont-type))
				      (find-arg (cddr c) :sense-info)))))
    (setf (message-ont-types msg) (list ont-type))
    msg))

(defun replace-ont-in-sense (sense-info pos-list ont-type)
  (let ((c (replace-arg sense-info :ont-types (if (consp ont-type) ont-type (list ont-type)))))
    (if pos-list
	(replace-arg
	 (replace-arg c :penn-parts-of-speech '(NNP NNPS))
		     :trips-parts-of-speech pos-list)
	c)))

;;  preprocessing sequences

(defvar *sequence-separators* '(#\/ #\: #\- #\U+2013))

(defun filter-sequences (msgs)
  "we are looking for sequences using punctuation that are not domain known entities already"
  (let ((sequences (remove-if-not #'(lambda (x)
				      (progn
					;(format t "~%~%message domain-specific-info:~S pos:~S word:~S" (message-domain-specific-info x) (message-pos x) (message-word x))
					(and (not (message-domain-specific-info x))
					     (intersection (message-pos x) '(W::NAME w::N))
					   (let ((w  (coerce (message-word x) 'list)))
					     (and (> (list-length w) 1)
						  (intersection *sequence-separators* w  :test #'string-equal))))))
				  msgs)))
    (format t "~%~%SEQUENCES FOUND ARE ~S" sequences)
    (if (null sequences)
	msgs
	(preprocess-sequences (filter-out-messages #'(lambda (x y) 
							    (and (substretch x y)
								 (not-equal-content x y)))
						   sequences sequences)
			      msgs))))


(defun preprocess-sequences (sequences msgs)
  (if sequences
      (let ((seq (car sequences)))
	(trace-preparse "~%~% FOUND POSSIBLE SEQUENCE: ~S" seq)
	(preprocess-sequences (cdr sequences) (preprocess-sequence seq msgs)))
      msgs))
      

(defun preprocess-sequence (seq msgs)
  (let ((start (message-start seq))
	(end (message-end seq)))
    (multiple-value-bind
	  (subsumed others)
	(split-list #'(lambda (x) 
			(and (numberp (message-start x))
			     (numberp (message-end x))
			     (>= (message-start x) start) 
			     (<= (message-end x) end)
			     ))
		    (remove-if #'(lambda (m) (and (numberp (message-start m))   ;; remove the seq message here
						  (numberp (message-end m))
						  (eq (message-start m) start) 
						  (eq (message-end m) end)))
					 msgs))
      (trace-preparse "~%ATTEMPTING PREPARSE on ~S" (mapcar #'message-content subsumed))
      (let ((seq-parse (parse-and-extract (mapcar #'message-content (reverse subsumed)) '((ont::SA_identify 1)) 500)))
	(validate-and-extract-lf seq-parse seq others msgs)))))

(defun validate-and-extract-lf (terms seq others msgs)
  "this checks if we got a good parse of a sequence, and if so removes the subparts and rememebrs the LF for the sequence"
  (let* ((sa (car terms))
	 (utt (find-arg-in-act sa :type))
	 (root (find-arg-in-act sa :root))
	 (SAterms (find-arg-in-act sa :terms))
	 (root-lf (find-arg-in-act (find-if #'(lambda (x) (eq root (find-arg-in-act x :var))) SAterms) :lf))
	 (root-content (find-arg (cdddr root-lf) :content))
	 (content-lf (find-arg-in-act
		      (find-if #'(lambda (x) (eq root-content (find-arg-in-act x :var))) SAterms) :lf)))
    (if (and utt root root-lf content-lf)
	(let ((type (third content-lf)))
	  (trace-preparse "~%~% FOUND CONTENT ~S " content-lf)
	  (if (find-arg-in-act content-lf :sequence)  ;;(eq type 'ont::SEQUENCE)
	      (let ((modified-seq (modify-term-with-preparse seq content-lf)))
		(remember-preparse modified-seq root-content (remove-if #'(lambda (x) (eq (find-arg (cdddr x) :var) root))
							       SAterms))
		(cons modified-seq others))
	      ;; otherwise, we return all the messages
	      msgs))
	msgs)))


(defun modify-term-with-preparse (term content-lf)
  (let ((new-ont-type (or (find-arg-in-act content-lf :element-type)
			  (third content-lf))))
    (if (consp new-ont-type)
	(setq new-ont-type (cadr new-ont-type)))
    (replace-pos-and-ont-type term '(w::name) new-ont-type)))

;;  here we add preparse segments into a full speech act

(defun insert-preparses (acts)
  (if *preparses*
    (mapcar #'insert-preparse acts)
    acts))

(defun insert-preparse (act)
  (if (eq (car act) 'compound-communication-act)
      (replace-arg-in-act act :acts (mapcar #'(lambda (x) (insert-preparse1 x *preparses*))
					    (find-arg-in-act act :acts)))
      (insert-preparse1 act *preparses*)))

(defun insert-preparse1 (act preparses)
  "This looks for the LF term with the same start and end as the parsed segment and replaces it"
  (if preparses
    (let* ((pre (car preparses))
	   (start (car pre))
	   (end (cadr pre))
	   (word (caddr pre))
	   (root (car (cdddr pre)))
	   (newterms (car (cddddr pre)))
	   (newlf (find-if #'(lambda (x) (eq root (find-arg-in-act x :var))) newterms))
	   (input (find-arg-in-act newlf :input))
	   (oldterms (find-arg-in-act act :terms)))

     ;; (format t "~%New LF=~S. Oldterms are ~S" newlf oldterms)
      
      ;; we can't use exact matching for :START here because 
      ;; there's a few cases where the start/end doesn't match -- specifically the sequence NP occus in a larger phrases -- as in 
      ;;  prepositional phrases where the prep does not contribute meaning, and in some cases with terms built by *PRO* constructions
      (multiple-value-bind
	    (targets rest)
	  (split-list #'(lambda (x)
			  ;;(format t "~%CHECKING ~S" x)
			  (let ((thislf (find-arg-in-act x :lf)))
			    (and (not (member (car thislf) '(ont::F ont::speechact)))
				 (<= (find-arg-in-act x :start) start) 
				 (or (= (find-arg-in-act x :end) end)
				     ;; the following covers cases where sequences are expanded in the input - e.g., "Smad1/3/5 ends up as (smad 1 / smad 3 / smad 5"
				     (and (> (find-arg-in-act x :end) end)
					  (consp (find-arg-in-act thislf :name-of))
					  (subsetp input (find-arg-in-act thislf :name-of))))))) ; there might not be a :name-of
		      oldterms)
	(if targets
	    ;; find the minimal one
	    (let ((sorted-targets (sort targets #'> :key #'(lambda (x) (find-arg-in-act x :start)))))
	      (replace-term-with-sequence-and-continue (car sorted-targets) act preparses root newterms (append rest (cdr sorted-targets))))
	    ;; we failed to find a target LF Term in input, so just go on to the next
	    (insert-preparse1 act (cdr preparses)))))
    ;; if no more preparses, return the act
    act))

(defun replace-term-with-sequence-and-continue (target act preparses root newterms rest)
  (let* ((target-root (find-arg-in-act target :var))
	 (newtermswithargs (find-root-term-and-insert-args newterms root target)))
    (trace-preparse "~%~%Replacing target ~S with root ~S for preparsed fragment: ~%~S~%" target target-root newterms)
    (insert-preparse1 (replace-arg-in-act act :terms (append rest 
							     (subst target-root root newtermswithargs)))
		      
		      (cdr preparses))))
  
(defun find-root-term-and-insert-args (newterms root target)
  (multiple-value-bind
	(rootterm rest)
      (split-list #'(lambda (x)
		      (eq (find-arg-in-act x :var)
			  root))
		  newterms)
    (let ((rootlf (find-arg (cdar rootterm) :lf)))
      (if rootlf
	  (cons (replace-arg-in-act (car rootterm) :lf (append (remove-args-in-act rootlf '(:start :end)) ;;; we'll be using the start and end from the new parse
							       (cdddr (find-arg-in-act target :lf))))
		rest)))))

(defun remove-args-in-act (act args)
  (list* (car act)
	 (cadr act)
	 (caddr act)
	 (remove-args (cdddr act) args)))
