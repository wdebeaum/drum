(in-package "PARSER")

;; Testing shorcut stuff

(defun loads2 ()
  (DUL-load-dialogue "../ParserTests/monroe/s2-sent-entry-final.lisp")
  )

(defun dp (utt)
  (DUL-id-parse utt)
  )

;; end testing shortcut stuff

(defvar *standard* nil)
(defvar *final-file-list* nil)

(defstruct pcfg-lhs cat count rules)
(defstruct pcfg-rule name count prob)

(defvar *pcfg-db* nil)

;; This holds a bunch of information about the a dialogue utterance.

(defstruct dialogue-utterance corpus id sentence goldparse myparse goldlf mylf
  reference-feedback
  goldparse-constituent-count
  myparse-constituent-count
  constituent-count
  constituent-returned-count
  labelled-found
  soft-labelled-found
  goldlf-term-count
  mylf-term-count
  lf-terms-found
  lf-terms-found-count
  labelled-bracket-precision
  labelled-bracket-recall
  crossing-brackets
  finished-processing)

(defun DUL-CDU-init()
 ;; (if *save-reference-feedback* 
 ;;	  (setf (dialogue-utterance-reference-feedback *current-dialogue-utterance*) nil)
 ;;	(setf (dialogue-utterance-reference-feedback *current-dialogue-utterance*) 'NA)
 ;;	)
  (setf (dialogue-utterance-goldparse-constituent-count *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-myparse-constituent-count *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-myparse *current-dialogue-utterance*)
		nil)
  (setf (dialogue-utterance-constituent-count *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-constituent-returned-count *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-labelled-found *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-goldlf-term-count *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-mylf-term-count *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-lf-terms-found *current-dialogue-utterance*)
		0)
  (setf (dialogue-utterance-lf-terms-found-count *current-dialogue-utterance*)
		0)
   (setf (dialogue-utterance-labelled-bracket-precision *current-dialogue-utterance*)
		0)
   (setf (dialogue-utterance-labelled-bracket-recall *current-dialogue-utterance*)
		0)
   (setf (dialogue-utterance-crossing-brackets *current-dialogue-utterance*)
		0)
   (setf (dialogue-utterance-finished-processing *current-dialogue-utterance*) nil)
  )

;; increment the constituent-count  

(defun DUL-CDU-constituent-count-incr()
  (setf (dialogue-utterance-constituent-count *current-dialogue-utterance*)
		(+ (dialogue-utterance-constituent-count *current-dialogue-utterance*) 1)
		)
  )

;; increment the constituent-returned count

(defun DUL-CDU-constituent-returned-count-incr()
  (setf (dialogue-utterance-constituent-returned-count *current-dialogue-utterance*)
		(+ (dialogue-utterance-constituent-returned-count *current-dialogue-utterance*) 1)
		)
  )

;; process a parse.

(defun DUL-CDU-process-parse (parse lf constit-count)
  (let ( (CDU *current-dialogue-utterance*))
	
	(setf (dialogue-utterance-myparse CDU) parse)
	(setf (dialogue-utterance-mylf CDU) lf)
	(setf (dialogue-utterance-constituent-count CDU) constit-count)
	(setf (dialogue-utterance-goldparse-constituent-count CDU)
		  (reduce #'+ (mapcar #'DUL-count-parse-constituents (dialogue-utterance-goldparse CDU)))
		  )
	(setf (dialogue-utterance-myparse-constituent-count CDU)
		  (reduce #'+ (mapcar #'DUL-count-parse-constituents (dialogue-utterance-myparse CDU)))
		  )
	(setf (dialogue-utterance-labelled-found CDU)
		  (reduce #'+ (mapcar #'(lambda (x)
								  (DUL-count-labelled-found (dialogue-utterance-goldparse CDU) x nil))
							  (dialogue-utterance-myparse CDU))
				  )
		  )
	(setf (dialogue-utterance-soft-labelled-found CDU)
		  (reduce #'+ (mapcar #'(lambda (x)
								  (DUL-count-labelled-found (dialogue-utterance-goldparse CDU) x 'SOFT))
							  (dialogue-utterance-myparse CDU))
				  )
		  )
	(setf (dialogue-utterance-labelled-bracket-precision CDU)
		  (/ (dialogue-utterance-labelled-found CDU)
			 (dialogue-utterance-goldparse-constituent-count CDU)
			 )
		  )
	(setf (dialogue-utterance-labelled-bracket-recall CDU)
		  (/ (dialogue-utterance-labelled-found CDU)
			 (dialogue-utterance-myparse-constituent-count CDU)
			 )
		  )

	;; cheesy cheat for the moment until the final.lisp files get fixed.
	;; Converts (ONT::F to (F in the LF forms.
;    (setf (dialogue-utterance-mylf CDU) (cheesy-lfform-fix (dialogue-utterance-mylf CDU)))
;    
    (setf (dialogue-utterance-goldlf-term-count CDU)
          (length (get-gold-termlist)))
    (setf (dialogue-utterance-mylf-term-count CDU)
          (length (get-my-termlist)))
    (setf (dialogue-utterance-lf-terms-found CDU)
          (DUL-exact-terms-matched (get-my-termlist) (get-gold-termlist))
          )
    (setf (dialogue-utterance-lf-terms-found-count CDU)
          (length (dialogue-utterance-lf-terms-found CDU))
          )
	

  ;; Let the driver know that we are done!
  ;; This should be done last!
    (setf (dialogue-utterance-finished-processing CDU) T)
	)	  
  )

;;(defun cheesy-lfform-fix (old-lf)
;;  (replace old-lf (list (mapcar #'cheesy-term-fix (get-keyword-arg old-lf :terms)));
;;		   :start1 (position (get-keyword-arg old-lf :terms) old-lf))
;;  )

;; fixes ONT::F vs. F problem, and :assoc-with v17 vs. :assoc-with (v17) problem

;;(defun cheesy-term-fix (term)
 ;; (replace term (list (cheesy-lf-fix (get-keyword-arg term :lf)))
;;		   :start1 (position (get-keyword-arg term :lf) term))
;;  )

;;(defun cheesy-lf-fix (lf)
 ;; (let* ((fixed-first-lf (cons (dc::strip-colon-operator (first lf)) (cdr lf)))
 ;;		 (fixed-assoc-with
;;		  (if (get-keyword-arg fixed-first-lf :assoc-with)
;;			  (replace fixed-first-lf (list (list (get-keyword-arg fixed-first-lf :assoc-with)))
;;					   :start1 (position (get-keyword-arg fixed-first-lf :assoc-with) fixed-first-lf))
;;			fixed-first-lf)
;;		  )
;;		 (fixed-context-rel
;;		  (if (get-keyword-arg fixed-assoc-with :context-rel)
;;			  (replace fixed-assoc-with (list (list (get-keyword-arg fixed-assoc-with :context-rel)))
;;					   :start1 (position (get-keyword-arg fixed-assoc-with :context-rel) fixed-assoc-with)
;;					   )
;;			fixed-assoc-with)
;;		  )
;;		 )
;;	fixed-context-rel
;;	)
 ;; )

;; count the number of constituents in a parse

(defun DUL-count-parse-constituents (parse)
  (+ 1 (reduce #'+ (mapcar #'DUL-count-parse-constituents (get-keyword-arg parse :sub))))
  )

(defun DUL-count-labelled-found (goldparse myparse soft)
  (+
   (if (DUL-contains-true (mapcar #'(lambda (x) (DUL-find-labelled-constituent x
									  (cons (first myparse)
											(DUL-find-words myparse))
									  soft
									  ))
		   goldparse)
						 )
	   1  ;; count it if we found the constituent, 
	 0)   ;; don't count it if we didn't
   ;; Now find and count all sub-constituents of this parse.
   (reduce #'+ (mapcar #'(lambda (mp) (DUL-count-labelled-found goldparse mp soft))
					   (get-keyword-arg myparse :sub)))
   )
  )

(defun DUL-contains-true (l)
  (if (null l) nil
	(if (car l) 1
	  (DUL-contains-true (cdr l))
	  )
	)
  )


(defun DUL-find-words (parse)
  (let ((wordlist ;;(get-fvalue (get-keyword-arg parse :feats) 'w::input))
		 (second (find 'w::input (get-keyword-arg parse :feats) :key #'first))
		))
	(if wordlist
		;; If we get a wordlist, make sure that it's actually a list.
		(if (listp wordlist) wordlist (list wordlist))
	  (if (get-keyword-arg parse :sub)
		  (mmapcan #'DUL-find-words (get-keyword-arg parse :sub))
		(list (second (find 'w::lex (get-keyword-arg parse :feats) :key #'first)))
		)
	  )
	)
  )

;; The constituent is of the form (LABEL WORD1 WORD2 WORD3...)
;; We search until we find a constituent that matches (or return nil if no matches)

(defun DUL-find-labelled-constituent (parse constituent soft)
  (cond
   ((and
	 (equal soft 'SOFT)
	 (equal (first parse) (first constituent))
	 (DUL-soft-equal (DUL-find-words parse) (rest constituent))
	 )
	T)
   
   ((and
	 (equal (first parse) (first constituent)) ;; labels match
	 (equal (DUL-find-words parse) (rest constituent)) ;; wordlist matches
	 )
	T)  ;; If both match, we've found a match, so YEAH! return T.

	;; If they don't match, then try this on the subtree list, then reduce
	;; the subtree list to a single T or false value that is either T or nil
	;; Note that this is not a very efficient way of doing this.
	
	(T
	 (reduce #'(lambda (a b) (or a b))
			 (mapcar #'(lambda (x) (DUL-find-labelled-constituent x constituent soft))
					 (get-keyword-arg parse :sub))
			 :initial-value nil
			 )
	 )
	)
  )

(defun DUL-find-labelled-constituent-soft (parse constituent)
  (if (and
	   (equal (first parse) (first constituent)) ;; labels match
	   (DUL-soft-equal (DUL-find-words parse) (rest constituent)) ;; wordlist matches
	   )
	  T  ;; If both match, we've found a match, so YEAH! return T.
	;; If they don't match, then try this on the subtree list, then reduce
	;; the subtree list to a single T or false value that is either T or nil
	;; Note that this is not a very efficient way of doing this.
	
	(reduce #'(lambda (a b) (or a b))
			(mapcar #'(lambda (x) (DUL-find-labelled-constituent-soft x constituent))
						(get-keyword-arg parse :sub))
			:initial-value nil
			)
	)
  )

;; Here, the first non-silent words should match.  That is, skip all <sil> tokens

(defun DUL-soft-equal (wlist1 wlist2)
  (equal (remove-silences wlist1) (remove-silences wlist2))
  )

(defun remove-silences (l)
  (if (null l) nil
	(if (equal (car l) '<sil>)
		(remove-silences (cdr l))
	  (cons (car l) (remove-silences (cdr l)))
	  )
	)
  )


;; ------------------------------------------
;;             SEMANTIC MATCHING STUFF
;; ------------------------------------------


(defun DUL-exact-terms-matched (mylist goldlist)
  (remove-if #'(lambda (y) (equal y 'FAIL)) (mapcar #'(lambda (x) (DUL-find-exact-term x goldlist)) mylist))
  )

(defun DUL-find-exact-term (myterm goldlist)
  (if (null goldlist) 'FAIL
	  (let ((match (DUL-check-exact-LF-match (get-keyword-arg myterm :LF) (get-keyword-arg (car goldlist) :LF)))
			)
		(if (equal match 'FAIL)
			(DUL-find-exact-term myterm (cdr goldlist))
		  match)
		)
	)
  )

(defun DUL-check-exact-LF-match (myLFterm goldLFterm)
  (if (not (equal (length myLFterm) (length goldLFterm))) 'FAIL
	(DUL-promote-FAIL (mapcar #'DUL-check-LF-element-recursive myLFterm goldLFterm))
  )
  )

(defun DUL-promote-FAIL (l)
  (cond
   ((null l) nil)
   ((equal (car l) 'FAIL) 'FAIL)
   (t
	(let ((r (DUL-promote-FAIL (cdr l)))
		  )
	  (if (equal r 'FAIL) 'FAIL
		(cons (car l) r)
		)
	  )
	)
   )
  )

(defun DUL-check-LF-element-recursive (myLFelement goldLFelement)
  (cond
   ;; If both null, OK, otherwise, they don't match
   ((and (null myLFelement) (null goldLFelement)) nil)
   ((null myLFelement) 'FAIL)
   ((null goldLFelement) 'FAIL)
   ;; If both listp, call this on recursive lists, otherwise, 'FAIL
   ((and (listp myLFelement) (listp goldLFelement))
	(DUL-check-exact-LF-match myLFelement goldLFelement)
	)
   ((listp myLFelement) 'FAIL)
   ((listp goldLFelement) 'FAIL)
   ;; If both variable, good, otherwise, 'FAIL
   ((and (DUL-is-Var myLFelement) (DUL-is-Var goldLFelement) myLFelement))
   ((DUL-is-Var myLFelement) 'FAIL)
   ((DUL-is-Var goldLFelement) 'FAIL)
   ((equal myLFelement goldLFelement) myLFelement)
   (t 'FAIL)
   )
  )

(defun DUL-is-Var (someAtom)
  (if (not (symbolp someAtom)) nil
	(let ((charlist (coerce (symbol-name someAtom) 'list))
		  )
	  (and (equal (car charlist) #\V)
		   (notany #'null (mapcar #'DUL-is-numeric-char (cdr charlist)))
		   )
	  )
	)
  )

(defun DUL-is-numeric-char (character)
  (member character '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
  )


(defun DUL-terms-found (mylist goldlist)
  (remove-if #'null (mapcar #'(lambda (x)
								   (DUL-find-term x
												  (mapcar #'build-assoc-pair mylist)
												  (mapcar #'build-assoc-pair goldlist)
												  ))
							   mylist
							   )
				)
  )

(defun DUL-find-term (term a-mylist a-goldlist)
  (reset-var-table)
  (if (print (reduce #'(lambda (a b) (or a b))
			  (print
			   (mapcar #'(lambda (term2) (match-term-feats (cdr term) (cdr term2) a-mylist a-goldlist t))
					   (find-lf-terms-by-input (get-keyword-arg term :input) (get-gold-termlist)))
			   )
			  :initial-value nil
			  ))
	  term
	nil)
  )

(defun match-lfs-by-input (input)
  (match-lf-pair (find-lf-term-by-input input (get-gold-termlist))
				 (find-lf-term-by-input input (get-my-termlist))
				 )
  )

(defun find-lf-term-by-input (input termlist)
  (find input termlist :key #'(lambda (x) (get-keyword-arg x :input)) :test #'equal)
  )

(defun find-lf-terms-by-input (input termlist)
  (remove-if-not #'(lambda (x) (equal input (get-keyword-arg x :input))) termlist)
  )

(defun get-gold-termlist ()
  (get-keyword-arg (dialogue-utterance-goldlf *current-dialogue-utterance*) :terms)
  )

(defun get-my-termlist ()
  (get-keyword-arg (dialogue-utterance-mylf *current-dialogue-utterance*) :terms)
  )

(defun match-lf-pair (lf1 lf2)
  (print lf1)
  (print lf2)
  )

;; This seems to work, but there is a match-sems in the lf-testing.lisp file.  
  
;;(defun DUL-match-sems (lf1 lf2)
;;  (ident-sem-array (build-sem-array (get-keyword-arg lf1 :sem) nil)
;;				   (build-sem-array (get-keyword-arg lf2 :sem) nil)
;;				   )
;;  )

;; This is the gateway variable which determines whether or not
;; we're going to use the dialogue-utterance-storage method.

(defvar *use-dialogue-utterance-storage* nil)

;; This variable will determine whether the reference-feedback field of the
;; dialogue-utterance is filled in or not.

(defvar *save-reference-feedback* nil)

(defvar *print-progress-to-stderr* nil)

;; This variable will hold information about the current utterance
;; being parsed.

(defvar *current-dialogue-utterance* nil)

;; This is a list of a number of utterances.  It probably shouldn't
;; be accessed directly, as a number of methods for manipulating
;; it and its contents are provided below.

(defvar *dialogue-utterance-list* nil)

;; This clears the DUL to its original empty state.

(defun DUL-clear ()
  (setf *dialogue-utterance-list* nil)
)

;; This dumps the DUL to the specified file

(defun DUL-dump (fname)
  (write-out-list (reverse (DUL-prune)) fname)
  )

(defun DUL-prune ()
  (remove-if #'(lambda (x) (null (dialogue-utterance-myparse x))) *dialogue-utterance-list*)
  )

;; This loads in a dialogue in the format of the parser-tests directory
;; All of its utterances will be placed in the DUL.  Probably only want
;; to do this for -final, since the :parse field will be taken as a gold
;; standard for comparison.

(defun DUL-load-dialogue (fname)
  (with-open-file (stream fname :direction :input)
				  (setf *dialogue-utterance-list*
						(mapcar #'(lambda (u) (extract-DUL-info fname u)) (read-in-list stream)))
				  )
  fname
  )

(defun DUL-read-file (fname)
  (with-open-file (stream fname :direction :input)
				  (read-in-list stream)
				  )
  )

;; This just takes an utterance in the format from PARSERTESTS and
;; extracs the id, the sentence, and the actual parse. 

(defun extract-DUL-info (fname utterance)
  (make-dialogue-utterance :corpus fname
						   :sentence (get-keyword-arg utterance :sent)
						   :id (get-keyword-arg utterance :id)
						   :goldparse (get-keyword-arg utterance :parse) ;; weird.
						   :goldlf (get-keyword-arg utterance :lfform)
						   )
  )

(defun DUL-get-id-list ()
  (reverse (mapcar #'DU-get-id *dialogue-utterance-list*))
  )

(defun DU-get-id (DU)
  (dialogue-utterance-id du)
  )

(defun DUL-get-id-list-FT (from to)
  (let* ((id-list (DUL-get-id-list))
		(from-fixed (if (> from (length id-list)) (length id-list) from))
		(to-fixed (if (> to (length id-list)) (length id-list) to))
		)
	(butlast (nthcdr (- from-fixed 1) id-list) (- (length id-list) to-fixed))
	)
  )

(defun DUL-parse-FT (from to)
  (dolist (du (DUL-get-id-list-FT from to))
	(progn (if *print-progress-to-stderr*
			   (format *error-output* "Parsing ~S ~%" du)
			 )
	       (setf (end-seen *chart*) nil)
		   (DUL-id-parse du)
		   (DUL-wait-til-end)
		   )
	)
)
;; This parses a sentence that should already be in the DUL

(defun DUL-get-utterance (id)
  (find id *dialogue-utterance-list* :test #'DU-check-id)
  )

(defun DUL-id-parse (id)
  (let ((CDU (find id *dialogue-utterance-list*
				   :test #'DU-check-id)
			 )
		)
	(setf *current-dialogue-utterance* CDU)
	(pref (dialogue-utterance-sentence *current-dialogue-utterance*))
	)
  )

(defun DU-check-id (id du)
  (equal id (dialogue-utterance-id du))
  )

;; For one-off sentences which don't have gold-standard parses.

(defun DUL-parse (sentence)
  (setf *current-dialogue-utterance*
		(make-dialogue-utterance :corpus 'ONE-OFF :sentence sentence))
  (pref sentence)
  )


;;(defun DUL-wait-til-end ()
 ;; (loop
  ;; (if (dialogue-utterance-finished-processing *current-dialogue-utterance*) (return nil))
  ;; (sleep 0.1)
  ;; (mp::process-yield)
  ;; )
 ;; )

;;(defvar *results* nil)

(defun read-in-list (&optional (stream *standard-input*) (res nil))
  (let ((next (read stream nil 'eof)))
    (if (eq next 'eof)
	res
	(read-in-list stream (cons next res)))))

;; already defined in attachments.lisp. I am commenting this out to eliminate this warning:

;; loading #P"/Users/ferguson/Work/TRIPS/defsystem/src/config/lisp/../../../src/Parser/Trips-parser/parserStats.fasl"
; STYLE-WARNING: redefining WRITE-OUT-LIST in DEFUN
; (defun write-out-list (l fname)
;  (with-open-file (stream fname 
;                   :direction :output
;                   :if-exists :supersede
;                   :if-does-not-exist :create)
;    (mapcar #'(lambda (e)
;                (format stream "~S~%" e))
;            l)))
;; Accessors for PCFG information

(defun get-pcfg-db-rule-prob (cat rule)
  (let ((record (find cat *pcfg-db* :test #'check-category)))
	(if record
		(get-pcfg-db-rule-prob-aux (pcfg-lhs-rules record) rule)
	  nil)
  )
  )

(defun get-pcfg-db-rule-prob-aux (record rule)
  (let ((rule-record (find rule record :test #'check-rule-name)))
	(if rule-record
		(pcfg-rule-prob rule-record)
	  nil)
	)
  )

  
;; Let's pull PCFG information from a file.

(defun read-in-pcfg-db (fname)
  (with-open-file (stream fname :direction :input)
				  (setf *pcfg-db* (read-in-list stream))
				  )
  )

;; Let's pull PCFG information from the parses.

(setf *final-file-list* '("../ParserTests/monroe/s2-sent-entry-final.lisp"
						  "../ParserTests/monroe/s4-sent-entry-final.lisp"
						  ;;"../ParserTests/monroe/s6a-sent-entry-final.lisp"
						  ;;"../ParserTests/monroe/s6b-sent-entry-final.lisp"
						  "../ParserTests/monroe/s12-sent-entry-final.lisp"
						  "../ParserTests/monroe/s16-sent-entry-final.lisp"
						  ;;"../ParserTests/monroe/s17-sent-entry-final.lisp"
						  )
	  )


                                                                                
;;(defpackage :f
;;  (:use)
;;  (:import-from "COMMON-LISP-USER" "+" "-")
;;  )
                                                                                
;;(defpackage :ont
;;  (:use)
;;  (:import-from "COMMON-LISP-USER" "+" "-")
;;  )


;; Build probability database
(defun build-pcfg-db ()
  (setf *pcfg-db* nil)
  (let ((pairlist (extract-all-pairs-from-file-list)))
	(print (length pairlist))
	(counter-reset)
	(mapcar #'process-pair-with-count pairlist)
	(mapcar #'calc-probabilities *pcfg-db*)
	)
  )

(defun process-pair-with-count (pair)
  (if (equal (mod (counter-value) 100) 0)
	  (print (counter-value))
	)
  (counter-next)
  (process-pair pair)
  )

(defun run-through-pairlist (list-remaining count-so-far)
  (if list-remaining
	  (progn
		(if (equal (mod count-so-far 100) 0)
			(print count-so-far)
		  )
		(progn (process-pair (car list-remaining))
			   (run-through-pairlist (cdr list-remaining) (+ 1 count-so-far))
			   )
		)
	)
)

;; Uses *final-file-list* above to pull out the proper data pairs.

(defun extract-all-pairs-from-file-list ()
  (mapcan #'extract-all-rule-pairs *final-file-list*)
  )

;; Extracts rule expansion pairings from the *standard* variable.

(defun extract-all-rule-pairs-from-standard ()
  (mapcan #'extract-rule-pairs *standard*)
  )

;; Extracts all rule pairs from a file.

(defun extract-all-rule-pairs (fname)
  (print fname)
  (with-open-file (stream fname :direction :input)
	(mapcan #'extract-rule-pairs (read-in-list stream)))
  )

;; Extracts the rule pairs from a single sentence.

(defun extract-rule-pairs (sentence)
  (process-parse-list (extract-parse sentence) nil)
  )

;; Pulls the parse out of a sentence data-structure
(defun extract-parse (data-structure)
  (get-keyword-arg data-structure :parse)
  )


;; Given a parse tree and some partial results, constructs a list
;; containing the partial results and the new pair representing the
;; expansions contained in that parse tree list

(defun process-parse-list (parse-subtree-list results-so-far)
  (if results-so-far
	  (cons results-so-far (mapcan #'process-parse parse-subtree-list))
	(mapcan #'process-parse parse-subtree-list))
  )

;; This recursively processes the subtrees of the parse
(defun process-parse (parse-subtree)
  (process-parse-list
   (extract-subtree parse-subtree)  ;; pull out the subtree list
   (cons (extract-cat parse-subtree) ;; get the category
		 (extract-rule parse-subtree)) ;; get the rule.
   )
  )

;; pull the category

(defun extract-cat (parse-subtree)
  (car parse-subtree)
  )

;; pull the rule

(defun extract-rule (parse-subtree)
  (get-keyword-arg (get-keyword-arg parse-subtree :stats) :rule)
  )

;; pull the subtree.

(defun extract-subtree (parse-subtree)
  (get-keyword-arg parse-subtree :sub)
  )


;; Now that we have the entire list of things, let's process them.

;; PCFG-LHS is a record which contains the category, the number of times
;; that that category has appeared (count), and a list of the rules that
;; expand it.

;; PCFG-RULE is an element of the list of rules inside a PCFG-LHS, keeping
;; track of the name of the rule, the number of times that the rule appears (count)
;; and, eventually, the probability of that rule.
(defun calc-probabilities (record)
  (mapcar #'(lambda (x) (update-rule-prob (pcfg-lhs-count record) x)) (pcfg-lhs-rules record))
  )

(defun update-rule-prob (lhs-count rule-record)
  (setf (pcfg-rule-prob rule-record) (/ (pcfg-rule-count rule-record) lhs-count))
  )
  

(let ((counter 0))
    (defun counter-next ()
      (incf counter))
	(defun counter-value ()
	  counter)
    (defun counter-reset ()
      (setq counter 0)))

(defun process-pair (pair)
  (let ((record (find (car pair) *pcfg-db* :test #'check-category)))
	(if record
		(update-record record pair)
	  (setf *pcfg-db* (cons (new-record pair) *pcfg-db*))
	  )
	)
  )

(defun check-category (cat record)
  (equal cat (pcfg-lhs-cat record))
  )

(defun new-record (pair)
  (make-pcfg-lhs :cat (car pair)
				 :count 1
				 :rules (list
						 (make-pcfg-rule :name (cdr pair)
										 :count 1
										 :prob 0
										 )
						 )
				 )
  )

(defun update-record (record pair)
  ;; Increment the count of this lhs being seen by 1
  (setf (pcfg-lhs-count record) (+ (pcfg-lhs-count record) 1))
  ;; Find the rule-record inside the rules list.
  (let ((rule-record (find (cdr pair) (pcfg-lhs-rules record) :test #'check-rule-name)))
	(if rule-record
		(setf (pcfg-rule-count rule-record) (+ (pcfg-rule-count rule-record) 1))
	  (setf (pcfg-lhs-rules record)
			(cons (make-pcfg-rule :name (cdr pair) :count 1 :prob 0)
				  (pcfg-lhs-rules record)
				  )
			)
	  )
	)
  )

(defun check-rule-name (rule-name rule-record)
  (equal rule-name (pcfg-rule-name rule-record))
  )


;; Stolen from messages.lisp inside the parser

;;(defun get-keyword-arg (l key)
 ;; "Returns element following KEY in L, or NIL."
 ;; (cond ((and (consp l) (consp (cdr l)))
;;	 (if (eq key (car l))
;;	     (cadr l)
;;	   (get-keyword-arg (cdr l) key)))
;;	(t
;;	 nil)))


