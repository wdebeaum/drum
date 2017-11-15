;; The Top level code for managing interpretation of input
;; it basically gathers all the information from the queues one utterance at a time

(in-package :IM)

(defvar *current-dialog-manager* )  ;; set to #'textIM, #'simpleIM in system.lisp
(defvar *output-format* 'LF)   ;; set to LF or AKRL in System.lisp


;; ============
;;   DOING THE NEXT STEP IN PROCESSING SPEECH ACTS

(defun run-IM-manager nil
  (let ((act (get-next-from-inputQ)))
    (when act
      (trace-msg 4  "~%Processing ~S " act)
      (if (eq (car act) 'utterance-end)
	  ;; new utterance may have come in while we were processing the last one -- so we recheck
	  (if (empty-inputQ) 
	      (apply *current-dialog-manager* (list 'utt-end act))
	      (run-IM-manager))
	  (let ((index (add-to-im-record act)))
	    (apply *current-dialog-manager* (list 'process index)))))))


(defun set-processing-status (index value)
  (setf (utt-record-processing-status (get-im-record index)) value))

(defun identify-dialog-act (speaker lfs conditional)
  "This gets the current interpretation rules from the discourse stack
     and tries to interpret the input. This also includes processing of conditionals"
  ;; currently we also allow matching down the stack without really accounting for
  ;;   the consequences of this
  (trace-msg 1 "~%==============================~%  IDENTIFYING INTENDED SPEECH ACT FROM ~% ~S ~%=================================="
	     lfs)
  (let* ((rules (get-active-interpretation-rules))
	 (interpretation nil)
	 (reduced-lfs (cons (car lfs) (remove-unused-context (car lfs) (cdr lfs))))
	 (encoded-lfs (read-lfs reduced-lfs)))
    (loop while (and rules (null interpretation))
	  do
	  (setq interpretation (interp encoded-lfs (car rules)))
	  (setq rules (cdr rules)))
    ;; add the conditional if its present
    ;;   as per the CPS modificaiton spec, conditional go to either :condition or :relation depending on whether its an action or relation
    (let* ((result
	   (convert-vars-to-kr-package
	    (subst (list *me* speaker) '*US*
		   (subst *me* '*ME*
			  (subst speaker '*USER* interpretation)))))
	  (final (if conditional
		     (mapcar #'(lambda (r)
				 (insert-conditional r conditional))
			     result)
		     result)))
      (trace-msg 1 "~%==============================~%  INTENDED SPEECH ACT IS~% ~S ~%=================================="
	     final)
      final
      )))

(defun insert-conditional (cps-act conditional)
  "inserts a conditional into the context of a cps-act"
  (if conditional
      (let* (;;(relevant-id (find-arg-in-act cps-act :what))
	     ;;(context (find-arg-in-act cps-act :context))
	     ;;(relevant-def (get-def-from-akrl-context relevant-id context))
	     (conditional-def (car conditional))
	     (conditional-type (third conditional-def))
	     (op (third conditional-type))
	     (cond-var (find-arg-in-act conditional-def :val))
	     )
	(append cps-act (list :condition (list (case op (W::IF 'ONT::IF)
						     (W::WHEN 'ONT::WHEN)
						     (W::UNLESS 'ONT::unless))
					       cond-var)))
	)))
    

(defun find-arg-in-cps-act (cps-act)
  "finds the ID of the element in the cps-act that indicates the content"
  (let ((content (find-arg-in-act cps-act :what)))
    (if (symbolp content)
	content
      (case (car content)   ;; should be obsolete
	(ONT::goal (second (third content)))
	(ONT::bag (third content))
	(otherwise content)))))
    
#||
(defun process-CPS-act (act lfs index channel)
  "we interpret the CPS act produced, invoking the BA when necessary"
  (when act
    (let* ((act-id (gen-symbol 'CPSA))
	   (oldcontext (append (elim-minus-and-vars (find-arg-in-act act :context))
			       (elim-minus-and-vars (find-arg-in-act act :discoursecontext)))) ;;   some acts bring in context from discourse
	   (new-akrl-context (if *old-version* (interpret-SA-arguments act (append oldcontext lfs) index)
			     (map-SA-arguments  act (append oldcontext lfs))))
	   (CPS-act (remove-arg-in-act (remove-arg-in-act act :context) :discoursecontext))
	   (akrl-context (remove-unused-context CPS-act new-akrl-context)))
      (record-in-discourse-context index akrl-context 1)
      (trace-msg 1 "~%Processing CPS-act ~S with context ~S" cps-act akrl-context)
      (if (reasonable-interpretation CPS-act akrl-context)
	  (progn
	    (log-msg `(LOG-CPS-HYPOTHESIS :STATUS PROPOSED  :act ,CPS-act :context ,akrl-context) index)
	    (case (car CPS-act)
	      ((ONT::request ONT::propose ONT::ask-what-is ONT::ask-if ONT::report ONT::undo ONT::accept
			    ONT::close ONT::confirm ONT::reject ONT::ACK ONT::greet ONT::thank)
	       (set-processing-status index  'waiting)
	       (call-for-BA-evaluation act-id index CPS-act akrl-context channel)
	       T)
	      
	      (ONT::answer
	       (process-answer act-id index CPS-act akrl-context channel))

	      (ONT::ellipsis
	       ;; the ellipsis act will already have identified the implied CPS act
	       (let* ((e-cps-act (find-arg-in-act cps-act :act))
		      )
		 ;; now change the IM record with the inferred full CPS act
		 (set-processing-status index  'waiting)
		 (call-for-BA-evaluation act-id index e-CPS-act akrl-context channel))
	       )
	      ))
		       
	(progn
	  (set-processing-status index  'mapping-failure)
	  (log-msg `(LOG-CPS-HYPOTHESIS :STATUS ONT::FAILED-TO-FIND  :act ,CPS-act :content ,akrl-context) index)
	  (values))
	)
      )
    )
  )
||#
(defun elim-minus-and-vars (x)
  "Eliminate unintepretable values, including -, variables, and anything that is not a symbol or list"
  (if (symbolp x)
      (if (or (eq x '-)
	       (eql (char (symbol-name x) 0) #\?))
	  x)
    (if (consp x)
	x)))
  #||    
(defun process-answer (act-id index orig-CPS-act context channel)
  "answers only allowed if prior SA was a question!"
  (setq orig-CPS-act (remove-spurious-thing orig-cps-act context))
  (if (or *QUD* *allow-no-qud*)
      (progn
      	;; if debugging, we set up a fake *QUD*
	(when (and *allow-no-qud* (null *QUD*))
	  (format t "~%Setting fake QUD!")
	  (setq *QUD* '(fakequd :content (ont::ask-if))))
	(let* ((prior-act (find-arg-in-act *QUD* :content))
	       (condition (find-arg-in-act orig-cps-act :condition))
	       (frequency (find-arg-in-act orig-cps-act :frequency))
	       (what (find-arg-in-act orig-cps-act :what))
	       (cps-act (if (symbolp what) (replace-arg-in-act orig-cps-act :what (convert-to-package what :ont))
			    orig-cps-act)))

	  (case (car prior-act) 
	    ((ONT::ask-what-is ONT::ask-if ONT::CHECK)
	     (when (and (not (find-arg-in-act cps-act :what)) (find-arg-in-act cps-act :degree))
	     ;; we have an elliptical answer like "very" or "a bit"
	     (trace-msg 4 "adding :object role to answer from prior act ~S" cps-act)
	     (let ((object (find-arg-in-act prior-act :what))
		   (degree (find-arg (get-def-from-akrl-context (find-arg-in-act cps-act :degree) context) :instance-of)))
	       (if object
		   (setq cps-act (append (if degree (replace-arg-in-act cps-act :degree degree) cps-act)
					 (list :about object))))))
	   (when condition
	     (let* (;; condition type is found only for adverbial modifiers
		    (condition-type (if (consp condition) 
				       (case (third condition)
					 (W::WHEN 'ONT::WHEN) (W::UNLESS 'ONT::UNLESS))))
		    (final-condition (if condition-type (list condition-type (find-arg-in-act cps-act :what))
					 condition)))
	       (setq cps-act (remove-arg-in-act (replace-arg-in-act cps-act :condition final-condition)
						:what))))
	   (when frequency
	     (let* ((def (get-def-from-akrl-context frequency context))
		    (freq-type (or (find-arg def :frequency) (find-arg def :instance-of))))
	       (if freq-type
		   (setq cps-act (replace-arg-in-act cps-act :frequency freq-type)))))
				    
	   (set-processing-status index 'waiting)
	   (call-for-BA-evaluation act-id index CPS-act (clear-out-impro-junk context) channel)
	   t)
	  )))
      (progn
	(trace-msg 4 "Rejecting ANSWER ~S since there is no prior question" orig-cps-act)
	nil))
  )


(defun remove-spurious-thing (cps-act context)
  " this promotes things like frequency from the empty ONT::THING object that is the WHAT argument"
  (let ((what (find-arg-in-act cps-act :what)))
    (if what
	(let* ((obj-def (get-def-from-akrl-context what context))
	       (type (find-arg obj-def :instance-of)))
	  (if (eq type 'ONT::THING)
	      (let ((freq (find-arg obj-def :frequency))
		    (degree (find-arg obj-def :degree)))
		(if (or freq degree)
		    (progn
		      (if freq (append (remove-arg-in-act cps-act :what) (list :frequency freq))
			  (append (remove-arg-in-act cps-act :what) (list :degree degree))))
		    cps-act))
	      cps-act))
	cps-act)))
		  
(defun clear-out-impro-junk (context)
  "Eliminates terms of form (THE x THING :EQUALS y) and replaces x with y elsewhere"
  (multiple-value-bind
	(things rest)
	(split-list #'(lambda (x) (and (eq (find-arg x :instance-of) 'ONT::THING) (find-arg x :equals)))
		    context)
    (if (null things)
	rest
	(let ((ids (mapcar #'second things))
	      (eqs (mapcar #'(lambda (x) (find-arg x :equals)) things)))
	  (subst-all ids eqs rest)))))
||#

(defun subst-all (ids eqs rest)
  (if (null ids)
      rest
      (subst-all (cdr ids) (cdr eqs) (subst (car eqs) (car ids) rest))))

(defun reasonable-interpretation (cps-act context)
  "An interpretation is reasonable if it produces a CPS-act and the V objects in the content
       are defined"
  (if cps-act
      (let ((what (find-arg-in-act cps-act :what)))
	(if (or (symbolp what) (consp what))
	  (let* ((whatobjects (extract-constants what))
		 (ans
		  (or (null whatobjects)    ;; it has no content (e.g., a greet)
		      ;;  or we have a valid CPS-ACT and all extracted constants are defined
		      (and (member (car cps-act) '(ONT::request ONT::propose ONT::ask ONT::report ONT::accept ONT::reject ONT::answer ONT::ellipsis ONT::confirm ONT::ack ONT::enumerate ONT::ask-what-is ONT::ask-if))
			   (every #'(lambda (x)
				      (or (and (symbolp x) (some #'(lambda (lf) (eq (second lf) x)) context))
					  (and (consp x) (member (car x) '(if unless))
					       (let ((xx (second x)))
						 (some #'(lambda (lf) (eq (second lf) xx)) context)))))
				  whatobjects)))))
	    (or ans (trace-msg 2 "CPS-act not well-formed: ~S with context ~S" cps-act context)))
	  (trace-msg 2 "CPS-act not well-formed: :WHAT value ~S is not a symbol or list" what))
    )))

(defun extract-constants (lf)
  "Currently, we know somethings a constant if it begins with V (sorry!)"
  (cond
   ((symbolp lf)
    (if (and (not (keywordp lf)) (eql (char (symbol-name lf) 0) #\V))
             (list lf)))
   ((consp lf)
    (mapcan #'extract-constants lf))
   (t (list lf))   ;; its not a symbol or list - we return it so that checking will detect this and fail
   ))
	   
(defun call-for-BA-evaluation (act-id index CPS-act context channel)
  "calls the BA to evaluate a hypothesis"
  (call-BA
   `(request :CONTENT 
	     (EVALUATE :CONTENT
		       (CPS-ACT :ID ,act-id
				:CONTENT ,(insert-BA-vars CPS-act)
				:CONTEXT ,(remove-unused-context CPS-act context)
				:CHANNEL ,(Channel-id channel))))
   #'(lambda (m)
       (process-BA-interp index m channel))))

;; Here's where we process the BA's response

(defun process-BA-interp (index content channel)
  (trace-msg 1 "Received message from BA: ~S" content)
  (if content
      (let* ((ps-interp (if (consp content) (find-arg-in-act content :content)))
	     (successp (and ps-interp (consp ps-interp) (eq (car ps-interp) 'CPS-interpretation)))
	     (act-info (get-im-record index)))
	(log-msg (list* 'log-cps-interpretation
			:STATUS (if successp 'success 'FAILED)
			(if (consp ps-interp) (cdr ps-interp) ps-interp)) index)
	
	(record-processing-status index 'CPS-interpretation ps-interp)
            	
      ;; If the BA gave us a successful interpretation
	(if successp
	    (progn
	      ;; Commit it back to the BA
	      (record-call-to-BA `(request :content (commit :content
							    ,(append ps-interp
								     (list :gestures (get-recent-gui-acts))))) nil index)
	      (set-processing-status index 'processed)
	      (setf (utt-record-status index)  'success)
	      ;;(reset-qud ps-interp)
	      )
	 ;; Otherwise, see if we had any alternate interpretations to try by looking at the MApped field.
	  (let* ((SA (get-im-record index))
		 (interps (utt-record-Mapped SA))
		 (lfs (utt-record-lfs SA)))
	    (trace-msg 1 "im: BA call failed: returned ~S" content)
	       
	    (if (cdr interps)
		(process-another-CPS-act (cdr interps) SA lfs index channel)
	      ;; there are no more, we fail
	      (handle-failure index 'CPSA-failure channel)))))
    ;; Otherwise really screwed up
    (progn
      (trace-msg 1 "im: BA call returned no content")
      (handle-failure index 'CPSA-failure channel)))
  ;; Now we continue processing on the IM-RECORD
  (loop while (do-next channel))
  )

(defun get-w (x)
  (if (consp x) (third x) nil))

(defun add-lex (lf w)
  (if w 
      (append lf (list :lex w))
      lf))

(defun convert-lf-to-akrl (lf)
  (let* ((var (second lf))
	 (type (get-ont-type (third lf)))
	 (w (get-w (third lf)))
	;; (force (find-arg (cdddr lf) :force))
	;; (frequency (find-arg (cdddr lf) :frequency))
	 (akrl 
   	  (cond ((member (car lf) '(ONT::THE-SET ONT::INDEF-SET ONT::PRO-SET))
			 (add-lex (list* (map-to-krspec lf) var :instance-of 'ONT::SET :element-type type 
					 (remove-args (cdddr lf) '(:start :end)) ;'(:proform :start :end))
					 ) w))
		 ((member :operator lf)
		  (add-lex (list* (map-to-krspec lf) var :instance-of 'ONT::SEQUENCE :element-type type 
				  (remove-args (cdddr lf) '(:start :end)) ;'(:proform :start :end))
				  ) w))
		 (t (add-lex (list* (map-to-krspec lf)  var :instance-of type 
				    (remove-args (cdddr lf) '(:start :end)) ;'(:proform :start :end))
				    ) w))))
	   )
    ;; now we set the equals value, either to the REFERS-TO, or the COREF links
    (let ((newakrl (replace-role-name akrl :refers-to :equals)))
      ;; if newakrl is EQ to akrl, we didn't find a :refers-to
      (setq akrl
	    (if (eq newakrl akrl)
		(replace-role-name akrl :coref :equals)
		;; if different, delete any :coref remaining
		(remove-arg newakrl :coref)))
    
      (trace-msg 4 "~%Converting LF ~S to AKRL:~S" lf akrl)
      akrl)
    ))

(defun replace-role-name (sv-list oldname newname)
  (when sv-list
    (if (eq (car sv-list) oldname)
	(cons newname (cdr sv-list))
	(reuse-cons (car sv-list)
		    (reuse-cons (cadr sv-list)
				(replace-role-name (cddr sv-list) oldname newname)
				(cdr sv-list))
		    sv-list))))

(defun get-ont-type (x)
  (if (consp x) (second x) x))

(defun map-to-krspec (spec)
  (case (car spec)
    ((ONT::THE ONT::QUANTITY-TERM ONT::THE-SET) 'ONT::the)
    ((ONT::PRO ONT::PRO-SET)
     (let ((crel (find-arg-in-act spec :context-rel)))
       (if (member crel '(W::SOMEONE W::SOMETHING))
	   'ONT::A
	 'ONT::THE)))
    ((LAMBDA ONT::A) 'ONT::a)
    ((ONT::TERM ONT::EVENT ONT::EPI ONT::CC ONT::MODALITY) (car spec))
    ((ONT::F) 'ONT::RELN)
    (ONT::KIND 'ONT::KIND)
    (otherwise 'ONT::a)))  ; this includes ONT::TERM (2017/04/17: now ONT::TERM is passed through as itself)

(defun map-referent-to-KR (ref)
  "Selects the best referent from the REF-HYPs and sets the values
    in the referent structure. Returns the new KR form"
  ;;(format t "~%~%MAP-referent-to-kr called with ~S" ref)
  ;; currently ignores the fact that there could be alternative refs
  (install-hyp-in-referent ref (car (referent-ref-hyps ref)))
  (let ((new-kr-context (get-def-from-akrl-context (referent-id ref) (referent-kr-context ref))))
    (if new-kr-context
	(list new-kr-context)
      (list (list 'ONT::A (referent-id ref))))
    ))

(defun record-call-to-BA (msg cont index)
  (declare (ignore cont))
  (record-processing-status index 'response msg))
    
(defun call-GM (content)
  (let ((msg `(TELL :RECEIVER GM
		  :SENDER IM
		  :CONTENT ,content)))

    (output msg)))

(defun find-root-act (SA)
  "Given an LF form, this builds a concise representation of the act for generation"
  (let ((root (find-arg-in-act SA :root))
	(terms (find-arg-in-act SA :terms)))
    (extract-act root terms)))

(defun extract-act (root terms)
  (when terms
    (let ((v (find-arg-in-act (car terms) :var)))
      (if (eq v root)
	  (let ((a (find-arg-in-act (car terms) :lf)))
	    (if (eq (car a) 'A-SEQ)
		(list 'SA-SEQ (second a)
		      :ACTS (mapcar #'(lambda (x) (extract-act x terms))
						       (find-arg-in-act a :ACTS)))
	      (mapcar #'(lambda (x) (find-arg-in-act x :lf))
		      terms)))
	(extract-act root (cdr terms))))))

(defun handle-failure (index code channel)
  (declare (ignore channel))
  ;; do we need to send the GM a failure message?
  (let ((rec (get-im-record index)))
    (trace-msg 1 "~%~% Speech Act ~S, ~S, failed at stage ~S" index (utt-record-input rec)  code)
    (set-processing-status index code)
    (setf (utt-record-status rec) 'failed)
    
    ))


;;  Useful utilities


(defun split-list (fn ll)
  "This divides a list into two lists depending on whether FN is true of the element - it retains the original ordering of elements"
  (let ((yes nil)
        (no nil))
    (mapc #'(lambda (x) 
              (if (apply fn (list x))
                (setq yes (cons x yes))
                (setq no (cons x no))))
          ll)
    (values (reverse yes) (reverse no))))


;;  Various Comm functions

(defun call-BA (msg cont)
  (let ((message msg)) ;;(insert-BA-vars msg)))
    (if cont (send-msg-with-continuation  message cont)
      (output message))))

(defun insert-BA-vars (msg)
  "converts atoms of form _x to ?X"
  (cond
   ((symbolp msg)
    (if (equal (char (symbol-name msg) 0) #\_)
	(intern (coerce (cons #\? (cdr (coerce (symbol-name msg) 'list))) 'string))
      msg))
   ((atom msg)
    msg)
   ((consp msg)
    (cons (insert-BA-vars (car msg)) (insert-BA-vars (cdr msg))))
   ))

(defun lambdas-to-keywords (x)
  (mapcar #'lambda-to-keyword x))

(defun lambda-to-keyword (x)
  "translates (X (lambda x (INSTANCE-OF x T) (ROLE x V)*)) to (TERM X :INSTANCE-OF T :ROLE V ...)"
  (let ((content (cddr (second x))))
    (cons (caadr x)
	  (cons (car x)
		(triple-to-keyword  content)))))

(defun triple-to-keyword (triples)
  (when triples
    (if (third (car triples))
	(append (list (util::convert-to-package (caar triples) :keyword)
		      (third (car triples))) ;; (util::convert-to-package (third (car triples)) :IM))
		(triple-to-keyword (cdr triples)))
      (triple-to-keyword (cdr triples)))))
	  


(defun handle-sys-utt (act)
  "Record SYS UTT and update discourse state"
  (case (car act)
      (said
       (let* ((utt (find-arg-in-act act :what))
	      (cps-act (find-arg-in-act act :cps-act)) 
	      (content (find-arg-in-act cps-act :content))
	      (speaker (or (find-arg-in-act act :who) *me*))
	      (refkrs (find-arg-in-act act :refkr))
	      (input (find-arg-in-act act :input))
	      (explicit-context (find-arg-in-act cps-act :context)))
	 (multiple-value-bind
	       (new-cps-act context)
	     (fix-up-terms-in-said content)
	   (let* ((final-cps-act (list 'cps-act :content new-cps-act :context (append context explicit-context)))
		  (index (add-sys-act-to-record speaker utt final-cps-act refkrs input)))
	     (log-msg (cons 'LOG-SAID (cdr act)) index)
	     ;;(update-discourse-state speaker final-cps-act utt index)
	     ))
	 )
       )))

(defvar *terms-found* nil)

(defun fix-up-terms-in-said (expr)
  "GM sometimes returns terms in arg positions rather than context. Until this is fixed, we remove it here"
  (let ((*terms-found* nil))
    (values (remove-terms-in-said expr) *terms-found*)))

(defun remove-terms-in-said (expr)
  (cond ((atom expr) expr)
	((consp expr)
	 (if (eq (car expr) 'ONT::term) ;;'term)
	     (multiple-value-bind
		 (new-term-args new-context)
		 (remove-terms-in-said (cdr expr))
		 (setq *terms-found*
		       (append (cons (cons 'ONT::term
					   ;;'term
					   new-term-args) new-context) *terms-found*))
		 (second expr))
	   (mapcar #'remove-terms-in-said expr)))))




(defun find-lf (id lfs)
  (find-if #'(lambda (x) (eq (second x) id)) lfs))

(defun remove-mod (lf modvar)
  "removes a MOD if it is the modvar"
  (cons (car lf) (remove-mod-arg (cdr lf) modvar)))

(defun remove-mod-arg (args modvar)
  (when args
    ;;(format t "~%args is ~S ~S~%" (car args) (if (consp (cadr args)) (eq (caadr args) modvar)))
    (if (and (eq (car args) :mods) (eq (caadr args) modvar))
	(cddr args)
	(list* (car args) (cadr args) (remove-mod-arg (cddr args) modvar)))))
	 
		
(defun find-def-in-lFS (id LFS)
  (find-if #'(lambda (x) (eq (cadr x) id)) LFS))
	
;; ======
;;  GUI Interpretation
;;

(defun handle-GUI-action (Action-performed)
  (if (equal trips::*scenario* 'user::|obtw|)
      (trace-msg 5 "In OBTW, ignore action-performed messages from the simulator") 
      (let* ((act (find-arg-in-act action-performed :content))
	     (actor (find-arg-in-act action-performed :who))
	     (id (gen-symbol 'GUI))
	     (AKRL-version (list* 'ONT::RELN id :instance-of (convert-to-package (car act) *kr-package*)
				  (cons :actor (cons actor (cdr act))))))
	(add-GUI-act-to-record akrl-version 'webtracker t)  ;; add discourse record for possible reference
	(notify-BA `(action-observed :content (AKRL-EXPRESSION :content ,id :context ,(list AKRL-version)))) ;;:content ,act)
	)
      )
  )

(defun notify-BA (act)
  (send-msg `(TELL :content ,act)))
    
(defun handle-map-select (SA)
  (let* ((map (find-arg-in-act SA :map))
	 (what (find-arg-in-act SA :what))
	 (lonlat (find-arg-in-act SA :lonlat))
	 (region (find-arg-in-act SA :region))
	 (index (add-gui-act-to-record SA map nil))
	 (ref (cond
	       (region (list (make-region-object-ref lonlat region)))
	       (what (make-object-ref what lonlat)))))
    
    ;; add the referents
    (if (or what lonlat region)
      (progn
	(record-processing-status index 'reference
				  (merge-gui-references (utt-record-referring-expressions (aref *im-record* *im-utt-count*))
						       ref))
	(setf (utt-record-status index)  'success))
      (setf (utt-record-status index) 'failure))
    ))
    
    
(defun make-object-ref  (what lonlat)
  (let* ((id (find-arg-in-act what :id))
	 (type (find-arg-in-act what :type))
	 (lftype (case type
		   (robot '(:* ONT::machine W::robot))
		   (mine '(:* ONT::manufactured-object W::Mine))
		   (background '(:* ONT::DISPLAY W::SCREEN))
		   (otherwise 'ONT::PHYS-object)))
	 (one (make-referent :id id
			     :accessibility 'visible-focus
			     :LF `(A ,id ,lftype)
			     :LF-TYPE lftype
			     :KR-context `((ONT::THE ,id :instance-of ,type :equals ,id))
			     :refers-to id))
	 (two (if (eq type 'background)
		  (let ((id0 (gen-symbol 'p))
			(id1 (gen-symbol 'id)))
		  (make-referent :id id0
				 :accessibility 'visible-focus
				 :LF `(A ,id0 ONT::Point)
				 :LF-TYPE 'ONT::point
				 :implicit t
				 :KR-context `((ONT::THE ,id0 :instance-of ONT::PLACE :hasaddress ,id1)
					       (ONT::THE ,id1 :INSTANCE-OF ONT::XYZCOORDINATEADDRESS
							:xcoordinate ,(car lonlat)
							:ycoordinate ,(cadr lonlat)))
				 )))))
    (if two (list one two) (list one))))

(defun make-region-object-ref (lonlat region)
  "create an object corresponding to the selected region"
  (let* ((id (gen-symbol 'loc))
	(v2 (gen-symbol 'L))
	(v3 (gen-symbol 'L))
	(loc-KR (or (mapcar #'cadr
			    (car (OM::transform
				  (cond
				   (lonlat `((ONT::A ,id ONT::COORDINATE :OF ,(car lonlat) :OF2 ,(cadr lonlat))))
				   (region (or `((ONT::A ,id  (:* ONT::LOCATION W::REGION) :COORD1 ,v2 :COORD2 ,v3)
						 (ONT::A ,v2 (:* ONT::LOCATION W::COORDINATE) :X ,(caar region) :Y ,(cadar region))
						 (ONT::A ,v3 (:* ONT::LOCATION W::COORDINATE) :X ,(caadr region) :Y ,(cadadr region)))))))))
		     `((THE ,id (instance-of ,id region) (coords ,id ,region)))))) ;; temporary hack until transform worked out
    (make-referent :id id :accessibility 'gui-focus
		   :LF `(A ,id ONT::GEO-OBJECT) :LF-TYPE 'ONT::GEO-OBJECT :KR-context (mapcar #'(lambda (x) (cons 'ONT::THE (cdr x))) loc-KR)
		   )))


(defun restart-agent (&key &allow-other-keys)
  "Method called in response to RESTART request."
  (setq *root-state* (make-dstate :type 'root :I-rules '(SA-rules)))
  ;;(setq *discourse-Stack* (list *root-state*))
  (setq *utts-in-progress* 0)
  (setq *current-channel* (make-channel :ID 'unknown :conversant *user* :status 'closed))
  ;;(setq *index-count*  (make-array 1000 :initial-element 0))
  (setq *terms-found* nil)
  (setq *referents* nil)
  ;;(setq *defs-found* nil)
  ;;(setq *DAT* nil)
  ;;(setq *QUD* nil)
  ;;(setq *QUDsatisfied* nil)
  (setq *completed-rules* nil)
  (setq *remaining-frontier* nil)
  (setq *IM-record* (make-array *max-SA-count*))
  (setq *IM-utt-count* -1)
  (setq *active-SA-index* 0)
  (setq *initial-match-trees* nil)
  (setq *active-rules* nil)
  (setq *inputQ* nil)
  (resetLFstore)
  )

(defun text-to-akrl (text size)
  (if (> (length text) 20)
      `(ONT::AKRL-EXPRESSION :content nil :context nil)
    (tta text)))

(defun tta (text)
  "function for quick parsing of some text and returning the AKRL - used for applications
     where we're doing additional parsing of data beyond the normal dialogue channels 
     (e.g., parsing content on web pages"
  (let* ((result (parser::parse-text (remove-if-not #'graphic-char-p text)))
	 (lfs (if (eq (car result) 'COMPOUND-COMMUNICATION-ACT)
		  (mapcan #'extract-lfs-from-sa (find-arg-in-act result :acts))
		  (extract-lfs (find-arg-in-act result :terms))))
	 (newlfs (interpret-tmas lfs))
	 (krs (convert-lfs newlfs))
	 (root (find-arg-in-act (car lfs) :content)))  ;; the first should be the speech act
    (values `(ONT::AKRL-EXPRESSION :content ,root :context ,krs) newlfs)))

(defun entail (text ids)
  (let* ((result (car (parser::parse-text (if (stringp text) (remove-if-not #'graphic-char-p text)
					 text))))
	 (lfs (if (eq (car result) 'COMPOUND-COMMUNICATION-ACT)
		  (mapcan #'extract-lfs-from-sa (find-arg-in-act result :acts))
		  (extract-lfs (find-arg-in-act result :terms))))
	 (newlfs (if *compute-force-from-tmas* (interpret-tmas lfs)
		     lfs)))
    (let ((oldvalue (flexible-semantic-matching *chart*)))
      (setf (flexible-semantic-matching *chart*) nil)
      (let ((result 
	     (compute-entailments newlfs ids)))
	(setf (flexible-semantic-matching *chart*) oldvalue)
	(values result lfs)))))

(defun find-in-text (text krtype)
  (let* ((rep (tta text))
	 (context (find-arg-in-act rep :context))
	 (root (Find-if #'(lambda (x) (eq (find-arg x :instance-of)  krtype)) context)))
    (if root
	(list `ONT::AKRL-EXPRESSION
	      :content (second root)
	      :context (remove-unused-context root context)))))
  
(defun extract-lfs-from-sa (sa)
  "this takes an SA and returns the LFs found in the terms"
  (extract-lfs (find-arg-in-act sa :terms)))

(defun receive-referent-info (reply)
  (format t "~%Receieved from DC: ~S" reply)
  nil)
  
(defun add-hyps (hyps rec)
  (mapcar #'(lambda (ref) (install-hyps-in-referent ref hyps))
	  (utt-record-referring-expressions rec)))

(defun install-hyps-in-referent (ref hyps-list)
  (let* ((id (referent-id ref))
	 (hyps (find-if #'(lambda (x) (eq (find-arg-in-act x :id) id)) hyps-list)))
    (setf (referent-ref-hyps ref) hyps)))


(defun install-most-likely-referent (index)
   "This is the most basic reference resolution decision: pick the one most highly ranked by the DC"
  ())

(defun build-term-list (ref)
  "converts archiac referent list into a reasonable format for messages"
  (list 'REF
	:id (referent-id ref)
	:sem (referent-sem ref)
	:lf-context (or (referent-lf-context ref)
			(list (referent-lf ref)))
	:kr-context (referent-kr-context ref)
	:refers-to (referent-refers-to ref)
	:coref (referent-coref ref)))

;;  Paragraph handling, for Text applications 
;;    where we process an entire paragprah at a time and then pro
       
(defun start-paragraph (msg)
  ;;(declare (ignore msg))
  (if *reset-utt-count-each-paragraph* (restart-agent))
  (setq *paragraph-lfs* nil)
  (let* ((content (get-keyword-arg msg :content))
	 (id (get-keyword-arg content :id)))
    (setq *paragraph-id* id)
    )
  )

(defun end-paragraph (msg)
  (declare (ignore msg))
  "end of paragraph, return a message containing all the LFs"
  #||(setq *paragraph-done* t)
  )

  (defun paragraph-completed ()
  "job done for this paragraph, so say so and reset"||#
  (send-msg `(tell :content (paragraph-done :id ,*paragraph-id*)))
  (setq *paragraph-id* nil)
  (setq *paragraph-done* nil))


;;; converting to graph form
;;; (see also lf-to-rdf-stream in ../WebParser/lf-to-rdf.lisp, and old version
;;; of lf-to-rdf in ../LFEvaluator/lf-to-rdf.lisp)

(defun convert-plural-arg-to-dot (source args &key remove-final-s append-symbol append-index)
  "Split a plural argument like :mods or :and into separate singular arguments
   while converting to Graphviz dot format.
   e.g. :mods (v123 v456) => :mod v123 :mod v456"
  (let ((prefix (string (first args))))
    (when remove-final-s
      (setf prefix (subseq prefix 0 (- (length prefix) 1))))
    (convert-args-to-dot
      source
      (append
          (loop for v in (second args)
	        for i upfrom 0
		for suffix =
		  (cond
		    (append-symbol (format nil "-~a" append-symbol))
		    ((and append-index (> i 0)) (format nil "-~a" i))
		    (t "")
		    )
		append
		  (list (intern (format nil "~a~a" prefix suffix) :keyword) v)
		)
	  (cddr args)))))

(defun convert-args-to-dot (source args)
  "Return a string in Graphviz dot format representing the list of LF term
   keyword args for the given source term ID."
  (cond 
   ((null args) nil)
   ;; plural args
   ((member (first args) '(:mods :members))
     (convert-plural-arg-to-dot source args :remove-final-s t))
   ((member (first args) '(:and :or))
     (convert-plural-arg-to-dot source args :append-symbol 'element))
   ((and (eq (first args) :sequence) (listp (second args)))
     (convert-plural-arg-to-dot source args :append-index t))
   ((eq (first args) :acts)
     (convert-plural-arg-to-dot source args :remove-final-s t :append-index t))
   ((eq (first args) :domain-info)
    ;; special case to avoid ridiculously wide labels for :domain-info
    (let ((node-id (car (second args)))
          (items (cdr (second args))))
        (declare (ignore node-id))
      (append
        (mapcan
          (lambda (item)
            (let ((node-id (gentemp "NT"))
	          (verb (car item))
		        ;; DRUM mappings clutter the graph too much
		  (args (util:remove-arg (cdr item) :mappings)))
	      (list (format nil "  \"~s\" -> \"~s\" [label=\":domain-info\"]~%"
                            source node-id)
	            (format nil "  \"~s\" [label=<(~s<br/> ~{~s~^ ~s~^<br/> ~})>]~%"
			    node-id verb args)
		    )))
	  items)
	(convert-args-to-dot source (cddr args))
	)))
   ((is-trips-variable (second args))
    (cons (format nil "  \"~s\" -> \"~s\" [label=\"~s\"]~%"
		  source (second args) (first args))
	  (convert-args-to-dot source (cddr args))))
   (t ; role value isn't a variable, so we must create a node for it
    (let ((node-id (gentemp "NT"))) ; nt for non-term
      `(,(format nil "  \"~s\" -> \"~s\" [label=\"~s\"]~%"
                 source node-id (first args))
	,(format nil "  \"~s\" [label=~s]~%" node-id
		 (format nil "~s" (second args)))
	,@(convert-args-to-dot source (cddr args))
	)))
   ))

(defun convert-terms-to-dot (terms input)
  (let ((*print-pretty* nil) ; avoid newlines embedded in node labels
        (res (list (format nil "digraph Terms {~%  node [shape=none]~%"))))
    (setq res (append res 
	  (cond 
	    ((null input) nil)
	    ((listp input)
	      (list (format nil "  \"~{~s~^ ~}\"~%" (convert-to-package input))))
	    ((stringp input)
	      (list (format nil "  ~s~%" input)))
	    (t (error "invalid type of input passed to convert-terms-to-dot: ~s" (type-of input)))
	    )))
    (dolist (term (convert-to-package terms))
      (let* ((term-type (first term))
	     (term-var (second term))
	     (term-lftype (third term))
	     (term-args (remove-arg (remove-arg (cdddr term) :start) :end)))
	(setq res (append res 
			  (cons (format nil "  \"~s\" [label=\"~s\"]~%" term-var (list term-type term-var term-lftype))
				(convert-args-to-dot term-var term-args))))
	))
    (apply #'concatenate 'string (append res (list (format nil "}~%"))))))


;;  quick code to support reference resolution without full IM
(defun p+ref (x)
  (resolve-refs-in-new-lf (parse-to-lfs x)))

(defun resolve-refs-in-new-lf (lfs)
  (let* ((n (add-lfs-to-im-record lfs))
	 (new-rec (aref *im-record* n))
	 (new-exprs (utt-record-referring-expressions new-rec)))
    (mapcar #'(lambda (r)
		(let ((hyps (find-possible-hyps r n nil nil)))
		  (setf (referent-ref-hyps r) hyps)
		  (if (car hyps)
		      (progn 
			(install-hyp-in-referent r (car hyps))
			(append (referent-lf r) (list :coref (referent-coref r))))
		      (referent-lf r))))
	    new-exprs)))
    

(defun add-lfs-to-im-record (lfs)
  (setq *im-utt-count* (+ *im-utt-count* 1))
  (setf (aref *im-record* *im-utt-count*)
	(im::make-utt-record   :lfs lfs
			       :status T
			       :referring-expressions  (mapcar #'(lambda (lf) (build-referent-structure (list nil :lf lf) lfs))
					      lfs)
			       :refKB (build-refKB lfs nil nil)
			       :index *im-utt-count*
			       ))
  *im-utt-count*)
  
(defun extract-LFs (terms)
  (remove-if #'null
	     (mapcar #'(lambda (x)
			 (let ((lf (find-arg-in-act x :LF)))
			   (if (find-arg-in-act lf :start)
			       lf
			       (append lf
				       (list :start (find-arg-in-act x :start) :end (find-arg-in-act x :end))))))
		     terms)))


(defun convert-vars-to-kr-package (x)
  "This converts any symbols in the IM package to the KR package - these are the unique ids we'll be using"
  (cond ((consp x)
	 (reuse-cons (convert-vars-to-kr-package (car x))
		     (convert-vars-to-kr-package (cdr x))
		     x))
	((symbolp x)
	 (if (eq (symbol-package x) *im-package*)
	     (intern (symbol-name x) *kr-package*)
	   x)
	 )
 	(t x)))

;; Code for converting LF ont types to other symbols as needed
;; Allows  mapping from atomic types, or types+words
;;  if no exact match, it attempt a hierarchical match on the atomic type
;;     (type1 type2) ((:* type1 word1) type2) 

(setq *symbol-map* nil)

(defun symbolmap (a b &optional ruleid)
  "If b is a variable, then it gets the mapping of a, or a if there is no mapping.
   If rule-id is specified, we filter map by that rule id"
  (let ((candidates (remove-if-not #'(lambda (x) (eq (third x) ruleid)) *symbol-map*)))
    (if (var-p b)
	(let ((c (cadr (assoc a candidates :test #'equal))))
	  ;;(format t "~%ReSULWAS ~S"  c)
	  (if c 
	      (list (list b c))
	      (let* ((simplified-a (if (and (consp a) (eq (car a) ':*)) (second a) a))
		     (c1 (cadr (assoc simplified-a candidates :test #'om::subtype ))))
		;;((c1 (cadr (assoc (cadr a) *symbol-map* :test #'equal))))
		;;(format t "~%ReSULWAS ~S"  c1)
		(if c1
		    (list (list b c1))
		    ;(list (list b a))))))
		    (list (list b simplified-a))))))
	;(list (list b a))
	(list (list b simplified-a))
	)))

(defun sort-symbol-map (symbol-map)
  "Return a copy of a simple symbol map (just (ont-type symbol) pairs with no
   lexical items or mapping groups), sorted so that descendant/more specific
   ont types come before their ancestor/more general ont types. This is used in
   rule sets generated from the online ontology builder/mapper tool."
  (stable-sort
      ;; stable-sort is destructive, so copy the list first
      (copy-list symbol-map)
      ;; it suffices to sort in reverse order by the length of the path from
      ;; the ont type to the root of the ontology hierarchy; ancestors will
      ;; have shorter paths than descendants
      #'>
      :key (lambda (pair) (length (om::get-parents (car pair))))
      ))

