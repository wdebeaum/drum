(in-package "IM")

(defvar *im-rule-table* nil) ;; an assoc list indexing rules by ID
(defvar *initial-match-trees* nil)  ;; a list of start nodes for rules
(defvar *active-rules* nil)  ;; a list of active nodes in current matching process
(defvar *im-trace* nil)   ;; controlling tracing
;;(defvar *qud* nil) ;; question under discussion for ellipsis procesing in answers
;;(defvar *qudsatisfied* nil)   ;;  QUD has been satisfied, but we may get an over ANSWER next (e.g., yes, 160 pounds)
(defvar *compute-force-from-tmas* t)  ;; remove TMA info and replace with force (default SA interp mode)
(defvar *reset-utt-count-each-paragraph* nil)

;;  CONTROL PARAMETERS - you may set these oin your system.lisp file

(defvar *suppress-context-in-extractions* nil)  ;; if T we supress the context information in outgoing messages

(defstruct IM-rule 
  id       ;; a uniquue identifer for the rule
  pattern  ;; a list of terms that must match input
  context-match  ;; a list of terms that must match input but are not covered by the output
  exclude  ;; a list of terms that must not match the input
  call ;; a list of procedure calls to make that must succeed
  output  ;; the output terms
  )

(defstruct mnode
  pattern     ;; the pattern that must match
  output      ;; the output expression (with variable bindings to far)
  bndgs       ;;  bindings
  children    ;;   index tree to succeeding matches
  covers      ;;  record of what input is covered
  rule-id    ;; the rule ID
  vars       ;; the list of vars in the pattern (cached to speed up copying)
  test       ;; the test from the rule
  )

;; THE DISCOURSE MANAGER DEFINITIONS

(defstruct utt-record
  uttnum
  speaker
  channel
  addressee
  start
  end
  speechactid   ;; the id of the speech act characteristizing this part of th record
  lf-ids   ;; the LFs in the act
  referring-expressions ;; A list of objects referred to for reference resolution
  focus ;; the focussed object from reference resolution (sort-of center)
  status ;; the core status needed by the dialogue manager: PENDING, FAILED, SUCCESS
  processing-status ;; a slot for use by different interpretation algorithms to keep track of intermediate states
  response    ;; a list of things that we will do once we have the turn
  input        ;; the utterance itself
  index       ;; backpointer into the UTTRECORD  
  extractions  ;; results of applying extraction patterns
  )

(defun make-lf-list (rec)
  "extracts the full LFs for the specified IDS in the REC"
  (mapcar #'(lambda (e)
	      (let ((rlf (referent-lf e)))
		(if (member ':start rlf)
		    rlf		
		    (append 
		     rlf
		     (list :start (referent-start e) :end (referent-end e)))
		    )
		)
	      )
	  
	  (utt-record-referring-expressions rec)))

;; IM-record maintains a global record of every speech act processed and what is happening about it
(defvar *max-SA-count* 10000)
(defvar *IM-record* (make-array *max-SA-count*))
(defvar *IM-utt-count* -1)
(defvar *active-SA-index* 0)

(defun get-im-record (i)
  (aref *im-record* i))

(defun get-next-active-im-record nil
  (aref *im-record* *active-SA-index*))

(defun clear-im-record nil
  (setq *im-record* (make-array *max-sa-count*))
  (setq *IM-utt-count* -1)
  (values))


(defstruct dstate
  type   ;; the type of the discourse process
  parent
  content
  I-rules
  ;; R-rules
  protocols
  context)


(defstruct ipattern
  act    ;; the CPS act representing the input analysis
  condition  ;; a condition to be verified from the DB
  then   ;; the conclusion to add to the DB
  )

(defstruct guicontrol
  live?
  combine?
  active?)

(defvar *gui-control* (make-guicontrol :live? t  :combine? t :active? t))
  
;; Channels
;;    Each channel has a unique ID, a status (closed, open) and a specific speaker the chennel connects to,
;;    and the greeting status (nil, greeted, confirmed, queried)

(defstruct channel id status conversant greeted)

;;  An assoc list of possible channel defaults
(defvar *available-channels*
  (list (cons 'phone (make-channel :ID 'phone :status 'closed :conversant *user*))
	(cons 'desktop (make-channel :ID 'desktop :status 'open :conversant *user*))))

(defun get-channel (id)
  (cdr (assoc id *available-channels*)))

(defun get-or-make-channel (id)
  (or (get-channel id)
      (progn
	(setq *available-channels* 
	      (cons (cons id (make-channel :id id)) *available-channels*))
	(car *available-channels*))))


      
;;  REFERENT INFO

(defstruct referent
  id         ;; the ID (indexing into the LF-store)
  name       ;; the :name-of slot in  the LF if it has one (for proper names)
  role       ;; the grammatical role of this expression if it exists
  lf-type    ;; The LF type
  num        ;; individual or set
  exclusions ;; IDs of object that this could not refer to (generally reflexivity contraints)
  ref-hyps   ;; a list of ref-hyp structures enumerating the likely referents
  coref      ;; final coreferential information
  refers-to  ;; final KR ID
  kb-assoc-with  ;; the associated external KB reference ID
  accessibility  ;; info on whether referent is available for different forms of reference
  implicit  ;; a flag set to T for objects introduced into the discourse context but not mentioned (typically via the GUI)
  lf        ;; the LF just for ease of processing since its identical to whats in the LFstore
  start
  end
  sem)

(defstruct ref-hyp
  id        ;; the ID of the object that this is a hypothesis for
  lf-type   ;; the LF-type for this antecedent
  sem       ;; the SEM for this antecedent
  refers-to ;; the KR object ID it refers to (if any)
  coref     ;; the LF var that it refers to
  kr-context   ;; new context for this hypotheis (adds to or replaces stuff in referent context
  score     ;; TBA - some indication of likelihood among the hypotheses
  )
  
(defstruct pattern-info
  pattern  ;; the pattern
  binding-lists ;; a list of all possible bindings
  optional  ;;  if the variable for the LF could match '- then the pattern is optional
)

(defun copy-mnode+ (node)
  (let ((bndgs (make-copy-bndgs (mnode-vars node))))
    (multiple-value-bind (output vars-seen)
	(subst-in1 (mnode-output node) bndgs nil)
      (multiple-value-bind (pattern new-vars-seen)
	  (copy-pattern-info+ (mnode-pattern node) bndgs vars-seen)
	(multiple-value-bind (vars newer-vars-seen)
	    (subst-in1 (mnode-vars node) bndgs new-vars-seen)
	  (values (make-mnode :pattern pattern
		    :output output
		    :rule-id (mnode-rule-id node)
		    :vars vars
		    :test (subst-in1 (mnode-test node) bndgs nil))
		  new-vars-seen))))))


(defun copy-pattern-info+ (pats bndgs vars-seen)
  (if pats
      (multiple-value-bind (pattern new-vars-seen)
	  (subst-in1 (pattern-info-pattern (car pats)) bndgs vars-seen)
	(multiple-value-bind (newbndgs newer-vars-seen)
	    (subst-in-list (pattern-info-binding-lists (car pats)) bndgs new-vars-seen)
	  (multiple-value-bind (rest newest-vars-seen)
	      (copy-pattern-info+ (cdr pats) bndgs newer-vars-seen)
	
	    (values (cons (make-pattern-info :pattern pattern
					     :binding-lists newbndgs
					     :optional (subst-in-list (pattern-info-optional (car pats)) bndgs newest-vars-seen))
			  rest)
		    newest-vars-seen))))
      (values nil vars-seen)))

(defstruct binding-result
  bndgs  ;; the bindings
  covers ;; what LF match produced the bindings
)

;;  Utilities

;; converting structures to list representation

#||(defun referent-to-list (s)
  (cond
   ((referent-p s)
    (list 'REFERENT :id (referent-id s) :sem (referent-sem s)
	  :lf (referent-lf s) :kr-info (referent-kr-info s)
	  :exclusions (referent-exclusions s) :ref-hyps (struct-to-list (referent-ref-hyps s))
	  :coref (referent-coref s) :refers-to (referent-refers-to s)
	  :kr-context (referent-kr-context s)))
   ((ref-hyp-p s)
    (list 'REF-HYP :lf-type (ref-hyp-lf-type s) :sem (ref-hyp-sem s)
	  :refers-to (ref-hyp-refers-to s) :coref (ref-hyp-coref s)
	  :kr-context (ref-hyp-kr-context s) :soce (ref-hyp-score s)))
   ((consp s)
    (mapcar #'struct-to-list s))
   (t s)))||#
   
    
