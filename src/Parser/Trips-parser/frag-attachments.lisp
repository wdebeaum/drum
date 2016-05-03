;;; Newattachments .lisp
;;  This basically passes on KR forms to IM/semantic interpreter
;;    and incorporates the score adjustment that is returned.
;;

;; Time-stamp: <Sat Jan 24 06:39:11 EST 2015 jallen>

;;
;;
;; This file contains functions that are semantically attached to rules in the grammar
;;

(in-package parser)


;; for an overview of SYNCHRONOUS INCREMENTAL PARSING, see the comments in and
;; surrounding the VP-advisor code, beginning with vpfound




;; Evaluating the whole buffer reintializes and recreates attachments.
;; Very handy for FI C-c C-b .

;; gf: 23 May 2000: removed :eval from this spec since not CLtL2
(eval-when (:load-toplevel)
  (init-attachments)) 

(defvar *reference-feedback-already-this-sentence* nil)
(defvar *reference-modify-entry-function* 'RF-noop)  

;;  Managing Unknown Words in fragments

(let ((unknown-words nil)
      )

  (defun get-unknown-words ()
    unknown-words)
  
  (defun set-unknown-words (val)
    (setq unknown-words val))
  
  (defun add-to-unknown-words (word start end)
    (setq unknown-words 
      (cons (make-indexedObject :object word :start start :end end)
	    unknown-words ))
    )
  ;;  used when backing up - remove all entries with end
  ;;    position after n
  ;;  This replaces the function defined in onlineParser.lisp
  
  (defun Clear-context-after (n)
    (set-unknown-words  
      (Remove-entries-after unknown-words  n)))
 
	  
)   ;; end scope of variables

;;  unknown words

(defun recordUnknownWord (e)
  (add-to-unknown-words (get-value (entry-constit e) 'w::input) 
	(entry-start e)
        (entry-end e))
  e)

(announce '(w::unknown) #'recordUnknownWord)


;;================
;;   TRAPPING NPS

(defvar *reference-filter-enabled* nil)
(defvar *incremental-enabled* nil)

;;(defvar *skeleton* nil) ;; if t use Collins parse skeletons as filter

(defun p-skeleton (x skeleton)
"Parse an input string with the Collins parse skeletons as filter"
  (setf (skeleton *chart*) skeleton)
  (p x)
  )


(defun npfound (e)
  (trace-msg 2 "~%-NP found: ~S~%" e)
  (when (skeleton *chart*)
    (skeleton-boost e))
  (when *incremental-enabled*
    (call-IM e))
  e)

(defvar *semantic-filter-weight* .2)
(defvar *remainder-weight* nil)
(setq *remainder-weight* (- 1 (/ *semantic-filter-weight* 2)))

(defun call-IM (e)
  "generic function for calling the IM and incorporating its feedback"
  (when (null (entry-prob-aux e))
    ;; do this only if we haven't seen this entry before
    (let* ((tree (build-constit-tree e nil))
	   (root (get-value (car tree) 'w::var))
	   (terms (find-terms-in-tree tree))
	   (lfs (mapcar #'(lambda (x) (find-arg-in-act x :lf)) terms))
	   (score
	    (im::build-semantic-fragment root lfs (constit-cat (entry-constit e)) (entry-name e) (entry-start e) (entry-end e))))
      (setf (entry-prob-aux e) t)
      (when score
	(trace-msg 1 "~%Semantic Filtering score: ~S. Adjusting probability ..." score)
	(setf (entry-prob e)
	      (+ (* (entry-prob e) *remainder-weight*)
		 (* score *semantic-filter-weight*)))
	))
    ))

  

;; experiment using skeletons
(defun skeleton-boost (e)
  (let ((start (entry-start e))
	(end (entry-end e))
	(cat (constit-cat (entry-constit e))))
    ;;(trace-msg 1 "~%~%CHECKING cat ~s from ~S to ~S" cat start end)
      (when (member (list cat start end) (skeleton *chart*) :test #'equal)
	(trace-msg 1 "~% Applying Skeleton Boost to ~S" (entry-name e))
	  (setf (entry-prob e)
		(* (entry-prob e) 1.03))
	)
      e))

;; =====================
;; Call-Reference-Filter
;; =====================

;; Calls the IM with a question about the referential plausibility
;; of this noun phrase.  If *Incremental-Parsing-Enabled*, this will
;; make a synchronous call, which will, as a side effect, halt the
;; parser until a message is received.  Otherwise, it will call a
;; different response function.  Eventually, there will be somethign
;; to do if Asynchronous Incremental Parsing is Enabled, which would
;; not have the effect of halting the parser until an answer comes back
;; ScS Apr 18 2005

(defvar *notification-list* nil)

(defun examination-dump (e)
  (let* ( (tree (build-constit-tree e nil))
		  (terms (find-terms-in-tree tree))
		  (parse (build-tree e '(w::var w::input) nil nil))
		  (msg  `(request :sender Parser
						  :content (NOTIFICATION :PARSE ,parse :TERMS ,terms)))
		  )
	(push msg *notification-list*)
	e)
  )

(defun write-out-list (l fname)
  (with-open-file (stream fname 
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (mapcar #'(lambda (e)
		(format stream "~S~%" e))
	    l)))
	

(defun call-reference-filter (e)
  "handles domain filtering of potential noun phrases"
  (let* ( (tree (build-constit-tree e nil))
          (root (get-value (car tree) 'w::var))
          (terms (find-terms-in-tree tree))
          )
	(if (member (entry-name e) *reference-feedback-already-this-sentence*)
		e
	  ;; Only do this if terms could be found.
	  (if terms
		  ;; The message sent is the same, incrementally or not.
		  (let ( (msg `(request
				:receiver IM
				:sender Parser
				:content (eval-reference-plausibility :ROOT ,root :TERMS ,terms)))
			 )
			;; Call the right function based on whether incremental parsing is enabled or not,
			;; and make sure to send it with a synchronous continuation (e.g. halt parsing
			;; until we get the continuation) if we are in incremental mode.  Note that
			;; if incremental parsing is enabled in synchronous mode, then we want to pass
			;; back 'DELAY-ENTRY so that the second half of add-entry-to-chart won't be called.
		    (if *Incremental-Parsing-Enabled*
			(progn
			  (send-with-synchronous-continuation msg
							      #'(lambda (msg)
								  (get-incremental-reference-feedback msg e))
							      )
			  'DELAY-ENTRY
			  )
			  ;; If we are not in incremental mode, then we'll call the standard function and return
			  ;; e as the entry, so that it will not be delayed.
		      (progn
			(send-with-continuation msg
						#'(lambda (newmsg) (get-reference-feedback msg e)))
			e)
		      )
		    )
	    )
	  )
	)
  )

(defun find-terms-in-tree (tree)
  (let* ((*additional-modifiers* nil)
         (objects (find-objects-in-constit-tree tree))
	 (terms (mapcar #'(lambda (x) (build-lf-term (constit-to-list (Cdr x)) nil)) objects))
	 )
    (append terms *additional-modifiers* )))

(defun get-reference-feedback (msg e)
  (declare (ignore msg e))
  nil)

(defun get-incremental-reference-feedback (msg e)
  (let (
	;;	(lf-tag (determine-lf-tag (get-keyword-arg msg :lf)))
		(e-name (entry-name e))
		(old-prob (entry-prob e))
		(old-score (calculate-entry-score e))
		)

	;;(format t "~&============~&~S~&============~&" e)
	
	(push e-name *reference-feedback-already-this-sentence*)

	(let* ((keyword (RF-message-keyword msg e))
	       (refprob (RF-keyword-to-refprob keyword))
	       (newe (funcall *reference-modify-entry-function* e refprob))
	       )
	  
	  (format t "~&Keyword:  ~S   Refprob:  ~S  Old Prob:  ~S  New Prob ~S ~&~&"
		  keyword refprob old-prob (entry-prob newe))

	  (add-entry-to-chart-continued e old-prob old-score)
	  )
	
	;; Now we put the entry back on the chart.
;;	(cond
;;	 ((eq (car msg) 'RECEIVE-KB-REPLY)
;;	  (let* ((article (get-keyword-arg msg :article))
;;			 (ref-list (get-keyword-arg msg :ref-list))
;;			 (keyword (RF-get-keyword (determine-lf-tag article) ref-list))
;;			 (refprob (RF-keyword-to-refprob keyword))
;;			 (newe (funcall *reference-modify-entry-function* e refprob))
;;			 )
		
;;		(format t "~&Keyword:  ~S   Refprob:  ~S  Old Prob:  ~S  New Prob ~S ~&~&"
;;				keyword refprob old-prob (entry-prob newe))
		;;	  (if (entry-p newe)
	;;	  (add-entry-to-chart-continued newe old-prob old-score)
;;		(add-entry-to-chart-continued e old-prob old-score)
		
		;;)
;;		)
;;	  )
;;	  ((eq (car msg) 'REF-CHECK-RESULT)
;;	   (progn
;;		 (funcall *reference-modify-entry-function* e
;;				  (RF-keyword-to-refprob 'TERRIBLE)
;;				  )
;;		 (add-entry-to-chart-continued e old-prob old-score)
;;		 )
;;	   )
;;	  )
	;;(add-entry-to-chart-continued e old-prob old-score)
	 ;; call the specified function to modify the entry in response to the message.
	 ;; all such functions should take the answer, msg, and e as their arguments,
	 ;; and return the modified entry
	 ;;	 (funcall *reference-modify-entry-function* (list varnum 'PA lf-tag tag) msg e)
	
	)
  )

;; ScS May 16 2005

;;This is the response to RESOLVE-NAME messages.  Need to handle these
;;as well.

;;(TELL :RECEIVER PARSER :IN-REPLY-TO PA18514 :CONTENT
 ;;   (ANSWER :REFERENTS
  ;;   ((AKRL-EXPRESSION :CONTEXT NIL :CONTENT ONT::MORNINGSIDE)))
  ;;  :SENDER IM)


(defun RF-message-keyword (msg entry)
  (case (car msg)
   ;; Handle (REF-CHECK-RESULT BAD :REASON X)
   (REF-CHECK-RESULT 'TERRIBLE)
   
   ;; Handle RECEIVE-KB-REPLY
   (RECEIVE-KB-REPLY (RF-receive-kb-reply-message-keyword msg entry))

   ;; Handle ANSWER
   (ANSWER (RF-answer-message-keyword (get-keyword-arg msg :referents)))
   
   ;; Everything else is unexpected, so let's make it 'NEUTRAL
   (otherwise 'NEUTRAL)
   )
		
  )

;; ScS May 16 2005
(defun RF-answer-message-keyword (referents)
  (let ((content (get-keyword-arg (car referents) :content)))
	;; If we have a match, then GOOD, otherwise, 'TERRIBLE- not an appropriate name
	(if content 'GOOD 'TERRIBLE)
	)
  )


;; ScS May 16 2005
  
(defun RF-receive-kb-reply-message-keyword (msg entry)
  (let (
		(article (get-keyword-arg msg :article))
		(ref-list (get-keyword-arg msg :ref-list))
		)
	(cond
	 ;; :REF-LIST (NON-REFERENTIAL GOOD), :REF-LIST (NON-REFERENTIAL BAD)
	 ((equal (car ref-list) 'NON-REFERENTIAL)  (second ref-list))
	 (t
	  (RF-get-keyword (determine-lf-tag article) ref-list)
	  )
	 )
	)
  )
	 

(defun RF-get-keyword (lf-tag answer)
  (case lf-tag
	(DEF-SING
	  (RF-get-def-sing-keyword answer))
	(INDEF-SING
	 (RF-get-indef-sing-keyword answer))
	(otherwise
	 (let ((set-mag (- (length answer) 1)))
		   (case set-mag
			 (0 'TERRIBLE)
			 (otherwise 'NEUTRAL)
			 )
		   )
	 )
;;	('INDEF-PLUR 'NEUTRAL)
;;	('DEF-PLUR 'NEUTRAL)
;;	('PRO 'NEUTRAL)
;;	('OTHER 'NEUTRAL)
	)
  )

(defun RF-get-def-sing-keyword (answer)
  (cond
    ((equal (car answer) 'SELECTED) 'GREAT)
	((equal (car answer) 'SALIENT) 'GOOD)
	(t
	 ;; We've got 'SET
	 (let (
		   (set-mag (- (length answer) 1))
		   )
	   (case set-mag
		 (0 'TERRIBLE)
		 (1 'GOOD)
		 (otherwise 'BAD)
		 )
	   )
	 )
	)
  )

(defun RF-get-indef-sing-keyword (answer)
  (let ( (set-mag (- (length (RF-remove-selected answer)) 1)))
	(case set-mag
	  (0 'TERRIBLE)
	  (1 'BAD)
	  (otherwise 'GOOD)
	  )
	)
  )

(defun RF-remove-selected (answer)
  (if (equal (car answer) 'SELECTED)
	  (cddr answer)
	answer
	)
  )

(defun RF-keyword-to-refprob (keyword)
  (case keyword
	(TERRIBLE 0)
	(BAD 0.3)
	(NEUTRAL 0.5)
	(GOOD 0.9)
	(GREAT 1.0)
	(otherwise 0.5)
	)
  )


(defvar *reference-interpolation-value* 0.1)
	
(defun RIV () *reference-interpolation-value*)
(defun 1-RIV () (- 1 *reference-interpolation-value*))


(defun RF-linear (e refprob)
  (setf (entry-prob e)
		(+ (* (1-RIV) (entry-prob e))
		   (* (RIV) refprob))
		)
  e
  )

(defun RF-noop (e refprob)
  e
  )

(defun RF-linear-w-boost (e refprob)
  (setf (entry-prob e)
		(+ (* (+ (1-RIV) (/ (RIV) 2))
			  (entry-prob e))
		   (* (RIV) refprob)
		   )
		)
  e
  )

(defun determine-lf-tag (article)
  (case article
	(ONT::A 'INDEF-SING)
	(ONT::THE 'DEF-SING)
	(otherwise 'OTHER)
	)
  )



(announce '(w::np (w::gap -)) #'npfound)



(defun definite (x)
  (member x '(w::THE w::THIS w::THAT)))

(defun findval (val vals)
  (cond ((null vals) nil)
	((eq (car vals) val) (cadr vals))
	(t (findval val (cddr vals)))))

;;  This flattens out conjunctions and removes constraints with reln -
(defun simplifyConstraint (conj)
 (cond
  ((or (null conj) (eq conj '-)) nil)
  ((and (constit-p conj) (eq (constit-cat conj) '&))
   (make-constit :cat '& :feats (cleanupconstraints (constit-feats conj))))
 #|| ((and (consp conj) (not (eq (car conj) '&))) ;; error check for old format
   (list conj))||#
  (t (parser-warn "Bad constraint: not a constituent: ~S" conj) nil)))

(defun cleanupconstraints (cs)
  (remove-if #'(lambda (c) (or (null c)
                               (and (consp c) (or (eq (car c) '-) (eq (third c) '-)))))
             cs))

(defun append-constraints (c1 c2)
  "appends two constraint structures"
  (if (and (constit-p c1) (constit-p c2) (eq (constit-cat c1) '&) (eq (constit-cat c2) '&))
    (make-constit :cat '&
                  :feats (remove-duplicates (append (constit-feats c1) (constit-feats c2)) :test #'equal))))
  
;;================
;;  HANDLING EVENT/STATES

;;================
;;  TRAPPING VPS  
;;  used to build event structures
;;  we trap VPs for use in partial interpretations

(defvar *vp-filter-enabled* nil)

(defvar *lf-prefs*
  '(()))

;;   we ignore constituents that have non-filled gaps
(defun vpfound (e)
  (let* ((newe (if (skeleton *chart*) (skeleton-boost e) e))
	 (new2 (if *lf-prefs*
		   (LF-preferer newe)
		 newe)))
    (when *incremental-enabled*
      (call-im new2))
    new2
    ))

(defun call-vp-filter (e)
  "handles domain filtering of potential verb phrases"

  ;; snag the important parts of the VP that need to be sent up to the IM
  ;; tree is the full constituent tree of e
  ;; root is the variable at the root of the tree that was found for e
  ;; terms are the LF terms from the tree, recursively expanded beginning
  ;;   with the root.  This is what actually gets sent.
  (let* ( (tree (build-constit-tree e nil))
          (root (get-value (car tree) 'w::var))
          (terms (find-terms-in-tree tree))
          )

	;; The first check is to figure out whether this entry has already been 
	;; processed.  If so, then we don't need to get more feedback, since it 
	;; has already been given feedback.
	;; ScS Nov 29 2005 Implementation Note:
	;; This particular scheme adds entry-names to a list, and clears the list
	;; at the beginning of any new sentence.  We'll need to improve this via an
	;; implementation change as soon as any two advisors might both give advice 
	;; for a sentence;  in this implementation the second one would not in fact
	;; be allowed to provide advice.  I'd suggest storing (advisor entry-name) 
	;; two element lists in *reference-feedback-already-this-sentence*.  That 
	;; should probably be combined with a formal way of registering an adivsor, 
	;; which constituents it wants to be sent, etc.

	;; If we've already provided feedback, then just return the constituent e,
	;; since we don't want more feedback and its already been changed, so it 
	;; can be added to the chart immemdiately.

	(if (member (entry-name e) *reference-feedback-already-this-sentence*)
		e

	  ;; Second check is to make sure that there were in fact LF-terms on the tree...
	  ;; if there are no terms, then we just return e
	  (if terms
		  
		  ;; Next, we'll build the message.  The same message gets sent 
		  ;; regardless of whether we are operating incrementally or not.

		  (let ( (msg `(request
						:content (eval-vp-plausibility :ROOT ,root :TERMS ,terms))
					  )
				 )
			
			;; If incremental parsing is not on, then we might never get a message in
			;; if the parser goes into its continue-BU-parsing loop.  So, in that 
			;; case, we send the message, because the vp-filter has been requested,
			;; but we don't change the entry, and we don't wait for the message to be 
			;; returned.

			;; If incremental parsing is on, then we want to send the message with 
			;; syncrhonous continuation.  This is the same as send-message-with-continuation
			;; in that the message-passing expects a response and calls a particular function
			;; when the reply is received, but differs in that it stops the normal operation 
			;; of the parser;  typically, at each time step, the parser does one step of 
			;; parser operation, then checks to see if a message has been received in the 
			;; meantime;  in incremental mode, when send-message-with-synchronous-continuation
			;; has been called, it will ONLY respond to messages until the synchronous-continuation
			;; queue is empty (or a new sentence starts).

			(if *Incremental-Parsing-Enabled*

				;; This sends off the message, and returns 'DELAY-ENTRY from this function,
				;; which will have the effect of causing the procedural entry code in 
				;; attachment.lisp to NOT add this constituent to the chart;  we want to 
				;; wait until we've incorporated the feedback.
				(progn
				  (send-with-synchronous-continuation msg
								#'(lambda (newmsg)
									(handle-vp-feedback (find-arg-in-act newmsg :content) e))
                                   )
				  'DELAY-ENTRY
				  )
			  ;; If we are not in incremental mode, then we'll call the standard function and return
			  ;; e as the entry, so that it will not be delayed.  Note that get-reference-feedback
			  ;; is a function that does nothing;  that is, the reply will be received, but nothing
			  ;; will be done with it.
		      (progn
				(send-with-continuation msg
										#'(lambda (newmsg) (get-reference-feedback newmsg e)))
				e)
		      )
			) ;; close let msg
		e  ;; return the original constituent...
		) ;; close if terms.
	  ) ;; close if member
	) ;; close let* tree,root,terms
  ) ;; close defun.

(defvar *vp-filter-interpolation-value* 0.1)

;; here's a very simple version for affecting the score
;;  of VP constituents based on the score returned by the VP advisor

;; We should probably modify this at some point to go through the same 
;; system as NP-feedback.  Ideally, we'd like to be able to specify a feedback
;; incorporation regime and any parameters and just have the score modification 
;; run through the same code no matter what we are doing.

(defun handle-vp-feedback (msg e)
  (format t "~%~%RECEIVED MSG ~S" msg)

  ;; We need several pieces of information to get the entry to incorporate 
  ;; properly into the chart.  First, we need the name of the entry so that 
  ;; we can add it to the set of constituents that have already received feedback.
  ;; Second, we need the old-prob and the old-score so that we can get the new entry
  ;; added back into the chart properly (I think that add-entry-to-chart-continued
  ;; uses the probability change to affect the old score or something, but it needs to 
  ;; get the original values from the constituent).

  ;; score pulls out the score coming back from the vp-advisor.

  (let ((e-name (entry-name e))
		(old-prob (entry-prob e))
		(old-score (calculate-entry-score e))
		(score (find-arg  msg  :vp-stat))
		)
	
	;; stick this constituent on the stack of constits we've processed so that 
	;; we don't get infinite VPadvisor feedback loops (if we lower the probability, it'll
	;; go back on the stack instead of getting added to the chart, and if we then 
	;; lower the probability again, it may go back on the stack, and we end up lowering its
	;; probability many many many times).

	(push e-name *reference-feedback-already-this-sentence*)

	;; Change the probability.  After this if block executes, e has a new probability
	
	(if (and score (numberp score))
	    (let ((newprob (+ (* *vp-filter-interpolation-value* score)
						  (* (- 1 *vp-filter-interpolation-value*) (entry-prob e)))))
		  (trace-msg 1 "Resetting prob of constit ~S from ~S to ~S" (entry-name e) (entry-prob e) newprob)
		  (setf (entry-prob e) newprob))
	  )
	
	;; stick the modified entry on the chart.
	(add-entry-to-chart-continued e old-prob old-score)
    ) ;; end let
  
  ) ;; end defun

(announce '(w::vp) #'vpfound)

(announce '(W::ADVP) #'skeleton-boost)



;;  this removes constraints with null values, replaces
;;  full constituents with their var, and then flattens the ANDS

;;  tranforms BE + PRED arg -> (BE pred) plus LSUBJ, LOBJ
;;   to enable, call from simplify-pred
;;   currently disabled
(defun transform-be (op lf)
  (if (or (eq op 'w::BE) (equal op '(w::NOT w::BE)))
	  (let* ((var (get-value lf 'w::var))
		 (constraints (cdr (get-value lf 'w::constraint)))
		 (subjc (assoc 'w::lsubj constraints))
		 (subj (third subjc))
		 (lcomp (assoc 'w::lcomp constraints))
		 (pred (third lcomp)))
	    
	    (if (or (and (constit-p pred) (eq (constit-cat pred) 'w::pred))
		    (and (consp pred) (eq (car pred) 'w::pred)))
		(let* ((predargs (if (constit-p pred) (constit-feats pred)  (cdr pred)))
		       (arg (cadr (assoc 'w::arg predargs)))
		       (constraint (cadr (assoc 'w::constraint predargs)))
		       (reduced-constraint (if (listp constraint)
					       (substitute subj arg constraint)))
		       (cpred (car reduced-constraint))
		       (firstarg (second reduced-constraint))
		       (secondarg (third reduced-constraint))
		       (newconstraint (if secondarg   
					  (list 'w::and (list 'w::lsubj var firstarg) (list 'w::lobj var secondarg))
					(list 'w::and (list 'w::lsubj var firstarg) )))
		       (newclass (if (eq op 'w::BE) (list 'w::BE cpred) (list 'w::NOT (list 'w::BE cpred)))))
		  (replace-feature-value (replace-feature-value lf 'w::class newclass)
					 'w::constraint
					 newconstraint))
	    (Parser-Warn "BE verb without PRED found: ~a" lf)))
          ))

(defun fix-args (cs)
  (if (consp cs)
    (let ((firstc (car cs)))
      (cond
       ((eq firstc '-) (fix-args (cdr cs)))
       ((not (and (consp firstc) (eql (length firstc) 3)))
	(cons firstc (fix-args (cdr cs))))
       (t ;; we have a binary constraint - ignore if thrid arg is
	           ;;      -, a variable, or null constit
	(let ((val (third firstc)))
	  (if (or (eq val '-)
		  (and (constit-p val) 
		       (eq (get-value val 'cat) '-)))
	      (fix-args (cdr cs))
	    (cons firstc (fix-args (cdr cs))))))))
    cs))
   	   
;;================
;;  Checking for early termination when we find an UTT that spans the input

;; If you want to actually use incremental parsing, you'd better have
;; defined and set the value of *incremental-parsing-enabled* to T,
;; else this will define it and set it to nil.

(defvar *incremental-parsing-enabled* nil)

;; =============
;; bestFirstTest
;; =============

;; If the appropriate number of solutions are found, then this will
;; suspend the parse by calling the (suspendParse) function, which
;; sets a local variable in the onlineParser.lisp code to make
;; the continue-BU-parse-loop code stop.  ScS Apr 12 2005

;; If the parse-end-conditions-check-out, then suspend the parse.
;; In either case, return the entry unmodified.

;; We suspend the parse differently depending on whether
;; incremental parsing is enabled or not.

;; Unfortunately, this check will not quite work in incremental
;; mode, because it is possible to find the utterance before getting
;; the *end-seen* message, and this will then never catch that utterance.
;; This is why we were using an *uttlist* before, and maybe should again.

(defun bestFirstTest (entry)
  "This checks if we have a complete interpretation"
  (if (parse-end-conditions-check-out entry)
	  (progn (if *incremental-parsing-enabled*
				 (setf *utterance-parsing-complete* T)     ;; Incremental
			   (suspendParse))   ;; Non-Incremental
			 (if *notification-list*
				 (progn
				   (write-out-list (reverse *notification-list*) "sausage.txt")
				   (setf *notification-list* nil))
				   )
			 entry)
	entry)
  )

(announce '(w::utt) #'bestFirstTest)

;; ==============================
;; Parse-end-conditions-check-out
;; ==============================

;; Whether we're in incremental mode or not, we determine
;; whether we've found a spanning utterance in the same way.

(defun parse-end-conditions-check-out (entry)
  ;; Note that this explicitly relies on the shortcutting of the and macro
  ;;(declare (special *end-seen*))
  ;;(declare (special *stopwhensolutionsfound*))
  
  (and (stopwhensolutionsfound *chart*)   ;; control variable that can be disable to force the parser to find all parses
	   (end-seen *chart*)                    ;;  end of utterance received from SR
	   (= (entry-start entry) (get-min-position)  )   ;;
	   (= (entry-end entry) (get-max-position) )
	   (>= (incr-number-found) (number-parses-to-find *chart*))
	   )
  )


;; ====================
;; print-out-utterances
;; ====================

;; Crappy attachment hack to print out utterances so that they
;; can be looked at as they are generated.
;;get-parse :syntax :feats '(w::var w::input)))))

(defun print-out-utterances (entry)
  ;;(format t "~S" (build-tree entry '(w::var w::input) nil nil))
  ;;(format t "~&~S" '(a b (c d)) )
  (format t "~&=========~&UTTERANCE~&=========" )
  (format t "~&~S~&=========~&" (build-tree entry '(w::var w::input) nil nil))
  entry
)

;;(announce '(w::utt) #'print-out-utterances)



#||
;;  Basic numbers, do the Math to compute simplified LF and compute new NTYPE

(defun process-number (e)
  (let* ((c (entry-constit e))
	 (val (get-value c 'w::val))
	)
    (if (not (numberp val))
	(if (consp val)
	    (let ((newVal (add-em-up val)))
	      (when (numberp newVal)
		(setFvalue e 'w::val newVal)
		;; Myrosia 05/14/00 fixed NTYPE to become a variable
		(setFvalue e 'w::NTYPE 
			   (make-var
			    :name 'n
			    :values (classify-n newVal)
			    :non-empty nil
			    ))
		))
	  (parser-warn "Bad number Val: ~S in ~s" val e)))
    )
  e)

;; Myrosia 05/13/00
;; changed that to allow multiplication and nested addition
(defun add-em-up (x)
  (cond
   ((numberp x) x)
   ((eq (car x) '+)
    (+ (add-em-up (second x)) (add-em-up (third x))))
   ((eq (car x) 'w::times*)
    (* (add-em-up (second x)) (add-em-up (third x))))
   ))

(announce '(w::number) #'process-number)

||#
;;======================
;;  Very simple LF Preference code
;;  Allows us to define desriable LF patterns for boosting (or penalizing)

(defun LF-preferer (e)
  (if (not-seen-before e)
      (let* ((lf (get-value (entry-constit e) 'W::LF))
	     (score-factor (check-lf-preferences lf)))
	(if score-factor
	    (setf (entry-prob e) (* (entry-prob e) score-factor)))))
    e)

(defvar *constits-seen* nil)

(defun not-seen-before (e)
  (let ((id (entry-name  e)))
    (if (member id *constits-seen*)
	nil
      (setq *constits-seen* (cons id *constits-seen*)))))

(defun check-lf-preferences (lf)
  (if (constit-p lf)
      (second (find-if #'(lambda (x) (lf-match (car x) lf))
		       *lf-prefs*))
    ))

(defun lf-match (lfpat lf)
  "Check for identical type and argument roles"
  ;;(format t "~% Checking ~S" lf)
  (if (match-with-subtyping (car lfpat) (get-value lf 'w::class))
      ;; we have a pattern for this predicate, check for exact matching of the roles
      (exact-role-match (cdr lfpat) (extract-semantic-roles (get-value lf 'w::constraint)))))

(defun extract-semantic-roles (constraint-list)
  (when (constit-p constraint-list)
    (extract-each-semantic-role (constit-feats constraint-list))))

(defun extract-each-semantic-role (roles)
  (when roles
    (if (member (caar roles) '(W::LSUBJ W::LOBJ W::LIOBJ W::LCOMP W::-))
	(extract-each-semantic-role (cddr roles))
      (cons (keywordify (caar roles))
	    (extract-each-semantic-role (cdr roles))))))

(defun exact-role-match (required-roles actual-role-values)
  (cond
   (actual-role-values
    (let ((r (first actual-role-values)))
      (if (member r required-roles)
	  ;; we have a match here, so check the others
	  (exact-role-match (remove-if #'(lambda (x) (eq x r)) required-roles)
			    (remove-if #'(lambda (x) (eq x r)) actual-role-values)))))
   ((null required-roles)
    ;; there are no required-roles left, so we succeed
    t)
   ))
  
(defun define-lf-preferences (prefs)
  (if (every #'(lambda (x)
		 (if (and (consp x) (consp (first x)) (numberp (second x)))
		     t
		   (progn
		     (format t "WARN: Bad LF preference form: ~S" x)
		     nil)))
	     prefs)
      (setq *lf-prefs* prefs)))
