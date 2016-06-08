;; The discourse manager for processing text applications - all IM does is reference
;;    resolution and gathering up all the LFs from a single sentence into one output message

(in-package :IM)

(defun textIM (type index)
  "this dialogue manager is used for basic text processing. It calls reference resolution
    and sends the results on"
  (case type
    (process   ;; new speech act
     (Do-discourse-reference index)
     (setf (utt-record-status (get-im-record index)) 'success)
     (save-simplified-lf index)
     (if *inputQ* (run-IM-manager))) ;; simple DM loops until no more input
    (utt-end   ;; end of utterance
     (save-lf-graph nil nil nil))  ;; this forces the graph to be output
    ))

;; Code for storing LFs temporarily until they are output for th entire sentence

(defvar *saved-lfs* nil)
(defvar *saved-input* nil)
(defvar *last-utt-id* nil)
(defvar *paragraph-id* nil)
(defvar *paragraph-lfs* nil)
(defvar *paragraph-done* nil)
(defvar *include-domain-info* nil)
(defvar *LF-output-by-utterance* t)       ;; output LF graphs by utterance rather than paragraph
(defvar *CPS-control* nil)   ;;  set to T if you want the CPS managerto explicitly ask for new input

(defun save-simplified-lf (index)
  (let* ((rec (get-im-record index))
	 (referents (utt-record-referring-expressions rec))
	 (domain-info (if *include-domain-info* (remove-if #'null
							   (mapcar #'(lambda (ref) 
								       (let ((di  (get-domain-info (referent-sem ref))))									 (when di (cons (referent-id ref) di)))) referents))))
	 (lfs (mapcar #'referent-lf (utt-record-referring-expressions rec)))
	 (ref-map (mapcar #'(lambda (x) (list (referent-id x) (or (referent-refers-to x) (referent-coref x)))) referents))
	 (*impro-substitutions* nil)
	 (simplified-LFs (remove-if #'null (mapcan #'(lambda (lf) (construct-lf lf ref-map domain-info)) lfs))))
    
    ;;(format t "~%Substituions are ~S~% lfs are ~S" *impro-substitutions* simplified-lfs)
    (save-lf-graph (substitute-impros simplified-LFS *impro-substitutions*) (utt-record-uttnum rec) (utt-record-input rec))))

(defun construct-lf (lf refmap domainmap)
  (let* ((ref (assoc (second lf) refmap))
	 (domain-info (assoc (second lf) domainmap))
	 (tma (convert-tma (find-arg-in-act lf :tma)))
	 (reduced-form (add-coref-feature ref ;;(convert-to-package 
				       (remove-arg-in-act (remove-arg-in-act lf :tma) :punctype)))
	 (lf-type (third reduced-form)))
    (if domain-info (setq reduced-form (append reduced-form `(:domain-info ,domain-info))))
    (cond 
      ((member LF-type '(SA_IDENTIFY SA_PRED-FRAGMENT SA-CONDITION SA_QUERY REQUEST SA-SEQ));;  W::SA_WH-QUESTION
       nil)  ;; ignore the fragment SA's that are highly unlikely in expository text
      ((eq (car reduced-form) 'IMPRO)   ;; ignore referents for IMPRO as they are very unreliable at present!!
       #||(if (cadr ref)
	   (progn  ;;  we replace the IMPRO with its referent and remove this term
	     ;;(Format t "~% SAving ref for ls ~S" lf)
	     (setq *impro-substitutions* (cons (convert-to-package (list (second lf) (cadr ref))) *impro-substitutions*))
	     nil)||#
	   (if (not (find-arg-in-act reduced-form :proform))
	       (list reduced-form)
	   ))
      ((eq LF-type 'S-CONJOINED)
       (let ((sequence (find-arg-in-act reduced-form :sequence))
	     (rlf (remove-arg-in-act reduced-form :sequence)))
	 (list (case (list-length sequence)
	   (0 rlf)
	   (1 (append rlf (list :item0 (car sequence))))
	   (2 (append rlf (list :item0 (car sequence) :item1 (cadr sequence))))
	   (otherwise (append rlf (list :item0 (car sequence) :item1 (cadr sequence) :item2 (caddr sequence))))))))
	 
      ((eq (car reduced-form) 'PRO)
       ;; remove the proform if its the same as the third element of type
       (let ((cr (find-arg-in-act reduced-form :proform)))
	 (list
	  (if (or (and (consp lf-type) (eq cr (third lf-type)))
		  (eq cr 'UTT-FRAG))
	      (remove-arg-in-act reduced-form :proform)
	      reduced-form))))
      (t ;;  usual case, we add a referent if it was found
       (if tma (list (append reduced-form tma))
	   (list reduced-form)))
      )))

(defvar *tma-suppress-list* nil)

(defun convert-tma (tma)
  "extract out the desired TMA features and returns as a feature list"
  (when tma
    (if (member (caar tma) *tma-suppress-list*)
	(convert-tma (cdr tma))
	(list* (convert-to-package (caar tma) :keyword) (cadar tma)
	   (convert-tma (cdr tma))))
    ))

(defun add-coref-feature (ref lf)
  (if (and (cadr ref) (not (member :coref lf)))
      (append lf (list :coref (convert-to-package (cadr ref))))
		lf))

	     
(defun substitute-impros (lfs substs)
  (if substs
      (substitute-impros (subst (cadar substs) (caar substs) lfs) (cdr substs))
      lfs))

(defun get-domain-info (sem)
  (let* ((sem-array (if (var-p sem) (var-values sem) sem))
	(domain-info (if (arrayp sem-array) (get-sem-feature-value sem-array 'F::kr-type))))
    (when (consp domain-info) domain-info)))


(defun save-lf-graph (lfs utt-id &optional input)
  (format t "~%*last-utt-id*=~S  utt-id+~S" *last-utt-id* utt-id)
  (if (or (null *last-utt-id*) (eq utt-id *last-utt-id*))
      (progn
	(setq *last-utt-id* utt-id)
	(if lfs (setq *saved-lfs* (cons lfs *saved-lfs*)))
	(setq *saved-input* (append *saved-input* input))
	)
      ;; we have a new utt NUM
      (multiple-value-bind 
	    (roots s-lfs)
	  (construct-sentence *saved-lfs*)
	(let ((sentence-lfs
	       (list 'sentence :uttnum *last-utt-id* :roots roots
		     :terms s-lfs)))
	  (when *LF-output-by-utterance*  ;; we are outputing LFs on a utterance basis rather than paragraph
	    (send-msg `(tell :content (sentence-lfs :content ,sentence-lfs))))
	  (when *show-lf-graphs*  ;; we are showing the graph for demonstration/debugging
	    (send-lf-graph s-lfs *last-utt-id* *saved-input*))
	  (setq *paragraph-lfs* (append *paragraph-lfs* (list sentence-lfs)))
	  (setq *last-utt-id* utt-id)
	  (setq *saved-lfs* (if lfs (list lfs)))
	  (setq *saved-input* input)))))

(defun construct-sentence (lfs)
  (values (mapcar #'cadar lfs) (collapse-one-level lfs)))

(defun collapse-one-level (ll)
  (when ll
    (append (car ll) (collapse-one-level (cdr ll)))))

(defun send-lf-graph (lfs utt-id input)
  (when lfs
    (let ((graph (convert-terms-to-dot lfs input)))
      (unless *LF-output-by-utterance* ;; in this case we send SENTENCE-LFS
	;; FIXME: as a matter of fact, this should be moved elsewhere 
	(send-msg `(TELL :content (SENTENCE-LFS :content  ,(list 'sentence :uttnum utt-id :roots (list (cadar lfs))
		     :terms lfs)))))
      (send-msg `(TELL :content (LF-GRAPH :uttnum ,utt-id :content ,graph))))))



;;===========================================
;;   SimpleIM
;; Simple IM is for use with a back end agent - it does reference and
;; sends CPS act messages and allow the CPS agent to synchronize the handling of multiple utterances (if *CPS-control* is set to T)

(defun simpleIM (msgtype arg)   ; arg is either index or end-utt message
  "this dialogue manager is for simple conversational systems - it does reference resolution and
     conventional speech act interpretation, sending the discourse acts  on"
  (trace-msg 3 "SimpleIM called on ~S: ~S" msgtype arg)
  (case msgtype
    (process   ;; new speech act
     (let* ((rec (get-im-record arg))
	    (results (Do-speech-act-processing rec arg)))
       (if (and  results (not (eq results 'failed)))
	   (progn
	     (replace-lfs-if-specified-in-SAprocessing (cdr results) rec)
	     (Do-visual-reference arg)
	     (Do-discourse-reference arg)
	     ;;(DO-KB-reference arg)
	     (setf (utt-record-status rec) 'success)
	     (build-and-send-cps-hyps results rec))
	   (progn
	     (setf (utt-record-status rec) 'fail)
	     (send-msg `(TELL :content (interpretation-failed :words ,(utt-record-input rec) 
							      :uttnum ,(utt-record-uttnum rec)
							      :channel ,(utt-record-channel rec))))
	     ))
       (if (and *inputQ* (not *CPS-control*))
	   (run-IM-manager))));; simple DM loops until no more input
    (segmentation-failure
     (send-msg `(TELL :content (interpretation-failed :words ,(utt-record-input rec) 
						      :uttnum ,(utt-record-uttnum rec)
						      :channel ,(utt-record-channel rec))))
     )
    (utt-end   ;; end of utterance
     (send-msg `(tell :content ,(cons 'end-of-turn (cdr arg))))
     )
    )
  (if (and (not *CPS-control*) 
	   *inputQ*)
      (run-IM-manager))
  )

(defun build-and-send-cps-hyps (acts rec)
  (if (not (eq acts 'failed))
      (let ((extended-context 
	     (add-reference-info-in-lfs
	      (remove-unused-context (car acts) (make-lf-list rec)))
	      ))
	(send-msg 
	 `(tell :content (CPS-act-hyps 
			  :hyps ,(if (member *output-format* '(LF LF-TERM))
				     (list (list* 'ont::SPEECHACT (gentemp "SA") (car acts)))
				     (car acts))
			  :context ,(build-lf-output extended-context *output-format*)
			  :words ,(utt-record-input rec)
			  :uttnum ,(utt-record-uttnum rec)
			  :channel ,(utt-record-channel rec)))))
      ;; utterance failed
      (send-msg
       `(tell :content (Interpretation-failed :words ,(utt-record-input rec) 
					      :uttnum ,(utt-record-uttnum rec)
					      :channel ,(utt-record-channel rec)))
       ))
  )

(defvar *roles-to-emit* nil)
(defvar *roles-to-suppress* nil)

(defun build-and-send-cps-hyps-with-new-lfs (acts lfs rec)
  (if (not (eq acts 'failed))
      (let ((extended-context 
	     (Mapcar #'remove-unwanted-roles
		     (add-reference-info-in-lfs
		      (remove-unused-context (car acts) lfs)
	      ))))
	(send-msg 
	 `(tell :content (CPS-act-hyps 
			  :hyps ,(if (member *output-format* '(LF LF-TERM))
				     (list (list* 'ont::SPEECHACT (gentemp "SA") (car acts)))
				     (car acts))
			  :context ,(build-lf-output extended-context *output-format*)
			  :words ,(utt-record-input rec)
			  :uttnum ,(utt-record-uttnum rec)
			  :channel ,(utt-record-channel rec)))))
      ;; utterance failed
      (send-msg
       `(tell :content (Interpretation-failed :words ,(utt-record-input rec) 
					      :uttnum ,(utt-record-uttnum rec)
					      :channel ,(utt-record-channel rec)))
       ))
  )

(defun remove-unwanted-roles (lf)
  (list* (car lf) (cadr lf) (caddr lf) (remove-unwanted-role (cdddr lf))))

(defun remove-unwanted-role (role-values)
  "Removes the roles not desired, either using a explicit list if desired roles (*roles-to-emit*) or
     suppresses roles (*roles-to-suppress*) if *roles-to-emit* is not specified. Also
     roles with empty values are removed"
  (when role-values
    (if (or (eq (second role-values) '-)
	    (if *roles-to-emit* 
		(not (member (car role-values) *roles-to-emit*))
		(member (car role-values) *roles-to-suppress*)))
	(remove-unwanted-role (cddr role-values))
	
	(list* (car role-values) (cadr role-values) (remove-unwanted-role  (cddr role-values))))))

(defun replace-lfs-if-specified-in-SAprocessing (newlfs rec)
  (when newlfs
    (let* ((lf (car newlfs))
	   (id (second lf)))
      (if (member id (utt-record-lf-ids rec))
	  (progn  
	    (StoreLF lf nil nil :replaceOK T)
	    (replace-lf-in-referent lf (utt-record-referring-expressions rec)))
	  (progn
	    (StoreLF nil nil lf)
	    (push (build-referent-structure (list 'term :lf lf) nil)
		  (utt-record-referring-expressions rec))))
      (replace-lfs-if-specified-in-SAprocessing (cdr newlfs) rec))))

;;;  Simple Extraction IM
;; uses the rules defined by *extractor-rules* in files load from system.lisp

;;===========================================
;;   ExtractSequenceIM
;; This is a version of EtractIM that allows an arbitrary sequence of extraction phases
;;     determined by the variable *extraction-sequence*

(defvar *extraction-sequence* nil)  ;; set to a list of pattern group ids that should be applied in sequence
;; with substitution

(defun ExtractSequenceIM (msgtype arg)   ; arg is either index or end-utt message
  "this dialogue manager is for simple extraction systems - it does reference resolution,
          temporal processing and send the results of the extraction rules"
  (trace-msg 3 "ExtractSequenceIM called on ~S: ~S" msgtype arg)
  (case msgtype
    (process   ;; new speech act
     (let* ((rec (get-im-record arg))
	    )
       (transform-modals rec)
       ;;(flatten-mods rec)    ;; this removes any binary modifiers and adds them as features 
       (Do-discourse-reference arg)
       ;;(DO-KB-reference arg)
       ;; Now we do the series of extractions based on the defined extraction sequence 
       (multiple-value-bind
	     (extractions newlfs)
	   (iterate-extractions rec *extraction-sequence*)
	 (setf (utt-record-status rec) 'success)
	 (build-and-send-extractions-from-lfs newlfs rec)
	 (when *show-lf-graphs*  ;; we are showing the graph for demonstration/debugging
	   (send-lf-graph (make-lf-list rec)  (utt-record-uttnum rec) (utt-record-input rec))))
       (if *inputQ* 
	   (run-IM-manager)))) ;; simple DM loops until no more input
    (utt-end   ;; end of utterance
     (send-msg `(tell :content ,(cons 'end-of-turn (cdr arg)))))
    ))

(defun SequentialLFtransformIM (msgtype arg)   ; arg is either index or end-utt message
  "this dialogue manager is for simple extraction systems - it does reference resolution,
          temporal processing and send the results of the extraction rules"
  (trace-msg 3 "SequentialLFtransformIM called on ~S: ~S" msgtype arg)
  (case msgtype
    (process   ;; new speech act
     (let* ((rec (get-im-record arg))
	    (results (do-speech-act-processing rec arg))
	    )
       (if (and  results (not (eq results 'failed)))
		(progn
		  (replace-lfs-if-specified-in-SAprocessing (cdr results) rec)
		  (Do-visual-reference arg)
		  (Do-discourse-reference arg)
		  ;;(DO-KB-reference arg)
		  (setf (utt-record-status rec) 'success)
		  (trace-msg 1 "~%==========================~%STARTING ITERATIVE EXTRACTIONS with sequence ~S" *extraction-sequence*)
		  (multiple-value-bind
			(extractions newLFS)
		      (iterate-extractions rec *extraction-sequence*)
		    (if newLFS
			(build-and-send-cps-hyps-with-new-lfs results newLFS rec)
			(build-and-send-cps-hyps results rec))))
		(progn
		  (setf (utt-record-status rec) 'fail)
		  (send-msg `(TELL :content (interpretation-failed :words ,(utt-record-input rec) 
								   :uttnum ,(utt-record-uttnum rec)
								   :channel ,(utt-record-channel rec))))
		  ))

       ;;(transform-modals rec)
       ;;(flatten-mods rec)    ;; this removes any binary modifiers and adds them as features 
       ;;(Do-discourse-reference arg)
       ;;(DO-KB-reference arg)
       ;; Now we do the series of extractions based on the defined extraction sequence 
       
	 (setf (utt-record-status rec) 'success)
	 ;;(build-and-send-extractions rec) ;; all the extractions are in the utt record 
	 (when *show-lf-graphs*  ;; we are showing the graph for demonstration/debugging
	   (send-lf-graph (make-lf-list rec)  (utt-record-uttnum rec) (utt-record-input rec))))
       (if (and *inputQ* (not *CPS-control*)) 
	   (run-IM-manager)))
     (utt-end   ;; end of utterance
      (send-msg `(tell :content ,(cons 'end-of-turn (cdr arg)))))
     ))

(defun iterate-extractions (rec seq)
  (when seq
    (trace-msg 1 "~%Performing Extractions with ~S" (car seq))
    (multiple-value-bind
	  (extractions lfs)
	(do-extractions rec (car seq))
      (continue-extraction-sequence extractions lfs rec (cdr seq)))))

(defun continue-extraction-sequence (extractions lfs rec seq)
  (if seq
    (multiple-value-bind 
	  (new-extractions newlfs)
	(continue-extractions extractions lfs rec (car seq))
      (continue-extraction-sequence new-extractions newlfs rec (cdr seq)))
    (values extractions (replace-lfs-with-terms extractions lfs nil))))

;;   ExtractIM

(defun ExtractIM (msgtype arg)   ; arg is either index or end-utt message
  "this dialogue manager is for simple extraction systems - it does reference resolution,
          temporal processing and send the results of the extraction rules"
  (trace-msg 3 "ExtractIM called on ~S: ~S" msgtype arg)
  (case msgtype
    (process   ;; new speech act
     (let* ((rec (get-im-record arg))
	    )
       (transform-modals rec)
       ;;(flatten-mods rec)    ;; this removes any binary modifiers and adds them as features 
       (Do-discourse-reference arg)
       ;;(DO-KB-reference arg)
       ;; Now we do the series of extractions (up to three possible phases)
       (if *term-extraction-rules*
	   (multiple-value-bind
		 (extractions lfs)
	       (do-extractions rec *term-extraction-rules*)
	     ;; now do event extraction phrase
	      (format t "~%Checking EVENT extraction rules: ~S" *extraction-rules*)
	     (multiple-value-bind 
		   (event-extractions newlfs)
		 (continue-extractions extractions lfs rec *extraction-rules*)
	       (format t "~%Checking post extraction rules: ~S" *post-extraction-rules*)
	       (if *post-extraction-rules*
		   (continue-extractions event-extractions newlfs rec *post-extraction-rules*)
		   )))
	   ;; no term extractions, do event extraction
	   (multiple-value-bind
		 (event-extractions newlfs)
	       (do-extractions rec *extraction-rules*)
	     (if *post-extraction-rules*
		 (continue-extractions event-extractions newlfs rec *post-extraction-rules*)
		 )))
       (setf (utt-record-status rec) 'success)
       (build-and-send-extractions-from-rec rec) ;; all the extractions are in the utt record 
       (when *show-lf-graphs*  ;; we are showing the graph for demonstration/debugging
	    (send-lf-graph (make-lf-list rec)  (utt-record-uttnum rec) (utt-record-input rec))))
     (if *inputQ* (run-IM-manager))) ;; simple DM loops until no more input
     (utt-end   ;; end of utterance
      (send-msg `(tell :content ,(cons 'end-of-turn (cdr arg)))))
     ))

(defun build-and-send-extractions-from-rec (rec)
  (if (consp  (utt-record-extractions rec))
      (let* ((context (make-lf-list rec))
	     (extractions (build-extractions (utt-record-extractions rec)  context))
	     (reduced-exs (mapcar #'elim-dups extractions)))
	(mapcar #'(lambda (e)
		    (send-msg 
		     `(tell :content 
			    (extraction-result 
			     :value ,e
			     :words ,(utt-record-input rec)
			     :uttnum ,(utt-record-uttnum rec)
			     :context ,(remove-unused-context e context)
			     :channel ,(utt-record-channel rec)))))
		    reduced-exs))
		     
      (send-msg
       `(tell :content (Interpretation-failed :words ,(utt-record-input rec) 
					      :uttnum ,(utt-record-uttnum rec)
					      :channel ,(utt-record-channel rec)))
       )))

(defun build-and-send-extractions-from-lfs (lfs rec)
  (if (consp lfs)
      (let* ((context lfs)
	     (extraction-ids (remove-duplicates (mapcar #'second (append-lists (utt-record-extractions rec)))))
	     
	     (extractions (build-extractions (mapcar #'list ;; I add an extra set of parens to be backwards compatible with old format!
						     (remove-if-not #'(lambda (x) (member (second x) extraction-ids)) lfs))
					     context)))
	     ;;(reduced-exs (mapcar #'elim-dups extractions)))
	(mapcar #'(lambda (e)
		    (send-msg 
		     `(tell :content 
			    (extraction-result 
			     :value ,e
			     :words ,(utt-record-input rec)
			     :uttnum ,(utt-record-uttnum rec)
			     :context ,(remove-unused-context e context)
			     :channel ,(utt-record-channel rec)))))
		    extractions))
		     
      (send-msg
       `(tell :content (Interpretation-failed :words ,(utt-record-input rec) 
					      :uttnum ,(utt-record-uttnum rec)
					      :channel ,(utt-record-channel rec)))
       )))

(defun elim-dups (elements)
  (when elements
    (if (not (member (car elements) (cdr elements) :test #'equal))
	(cons (car elements)
	      (elim-dups (cdr elements)))
	(elim-dups (cdr elements)))))

(defun build-extractions (acts context)
  (when acts
    (cons
     ;;(build-lf-output (expand-extraction (car acts) context) *output-format*)
     (build-lf-output (car acts) *output-format*)
     (build-extractions (cdr acts) context))))

(defun build-lf-output (lfs format)
  (case format
    (AKRL (mapcar #'convert-lf-to-akrl lfs))
    (LOGIC (convert-to-package (convert-lfs-to-logic lfs) :im))
    (LF (mapcar #'extend-lf lfs))
    (LF-TERM
     (mapcar #'convert-to-term-format
	     (mapcar #'extend-lf lfs)))
    (t lfs)))

(defun convert-to-term-format (lf)
  (if (or ;;(not (eq *current-dialog-manager* #'extractIM))
       (member (car lf) '(ont::f event term modality epi cc ont::event ont::term ont::modality ont::epi ont::cc)))
      lf
      (list* 'term (second lf) (third lf) :spec (first lf) (cdddr lf))))

(defun convert-lfs-to-logic (lfs)
  (when lfs
    (append (convert-lf-to-logic (extend-lf (car lfs)))
	    (convert-lfs-to-logic (cdr lfs)))))

;; add :start and :end to an LF term
(defun extend-lf (lf)
  (if (not (find-arg-in-act lf :start))
      (multiple-value-bind (start end) (get-start-end-for-lf (second lf))
	(if (and start end)
	    (append lf (list :start start :end end))
	    lf))
      lf))

(defun convert-lf-to-logic (lf)
  (let ((spec (car lf))
	(type (simplify-type (third lf)))
	(var (second lf)))
    (multiple-value-bind 
	  (relns modals)
	(remove-args (cdddr lf) '(:negation :tense :modality :perf :progr :conditional))
      (list* (list (classify-object-type var spec modals type) var)
	     (list 'type var type)
	     (append (build-relns var relns) (convert-modals var modals))))))

(defun simplify-type (type)
  (if (consp type) (second type) type))

(defun build-relns (var args)
  (when args
    (cons (list (car args) var (cadr args))
	  (build-relns var (cddr args)))))

(defun convert-modals (var modals)
  (when modals
    (if (member (car modals) (list :negation '-))
	(convert-modals var (cddr modals))
	(Cons (list (car modals) var (simplify-type (cadr modals)))
	      (convert-modals var (cddr modals))))))

(defun classify-object-type (var spec modals type)
  (case spec
    ((ONT::THE ONT::PRO)  `referential)
    ((ONT::THE-SET ONT::PRO-SET) `referential)
    (ONT::A `indefinite)
    (ONT::INDEF-SET `indefinite)
    ((ONT::F EVENT) 
     (if (is-stative type)
	 (if (eq (find-arg modals :negation) '+) 'nonstate 'state))
	 (if (eq (find-arg modals :negation) '+) 'nonevent 'event))
    (TRELN 'TRELN)
    (TIME-RELN 'TIME-RELN)
    (otherwise `nonreferential))
  )
