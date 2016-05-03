;; Given a new utterance, process it into a sequence of speech act, which might include
;;    breaking apart comound acts
;;    combining fragments
;;    resolving structural ellipsis

;; and returns a list of speech acts for subsequent processing

(in-package :IM)


(defvar *inputQ* nil)

(defun empty-inputQ nil
  *inputQ*)

(defun clearQ nil
  (trace-msg 3 "~%Clearing remaining speech acts ...")
  (setq *inputQ* nil))

(defun add-to-inputQ (a)
  (trace-msg 3 "~%Adding SA to queue: ~S" a)
  (setq *inputQ* (add-to-Q a *inputQ*)))

(defun get-next-from-inputQ nil
  (let ((x (pop *inputQ*)))
    (trace-msg 3 "~%Processing SA ~S" x)
    x))

(defun add-to-Q (a queue)
  (if (null queue) 
      (list a)
      (reuse-cons (car queue)
		  (add-to-Q a (cdr queue))
		  queue)))


;;  HANDLER FOR INCOMING MESSAGES

(defun handle-message (SA)	
  "Top level content handling"
  
   (case (car SA)
    (new-speech-act
     (let ((channel (or (find-arg-in-act (second SA) :channel)
			(find-arg-in-act (car (find-arg-in-act (cadr SA) :acts)) :channel))))
       (handle-new-speech-act (second SA) channel)
       (run-IM-manager)
       ))

    ;;  this is typically invoked whent the system takes over and asks a question, or interrupts
    ;;  we ignore the remaining pending speech acts
    (clear-speech-acts
     (clearQ))
     
    ;;  this gets any pending speech act off the inputQ
    (release-pending-speech-act
     (run-IM-manager))

    ;; we don't handle multiple parser hypotheses yet - we just take the first for now
    (new-speech-act-hyps 
     (handle-message `(new-speech-act ,(car (second SA)))))
     
    ;; this takes an LF form and processes it like it was output from the parser
    (new-lfs
     (let ((lfs (find-arg-in-act SA :lfs))
	   (roots (find-arg-in-act SA :root))
	   (stripped-lf (cons (car SA) (remove-args (cdr SA) '(:lfs :root))))
	   (channel (find-arg-in-act SA :channel)))
       (if (symbolp roots) (setq roots (list roots)))
       (mapc #'(lambda (r)
		 (post-speech-act (append stripped-lf
					  (list :root r
						:terms (mapcar #'(lambda (lf) `(term :lf ,lf :var ,(second lf))) 
							       (remove-unused-context-with-root r lfs))))
				  channel))
	     roots)
       (run-IM-manager)
       ))
    ;;  here we update the discourse state based on what SYS said
    (said
     (handle-sys-utt SA))

    ;; new channel 
    (channel-event
     (process-channel-act (find-arg-in-act SA :content)))

    (started-speaking
     (update-turn-info-with-new-speaker SA)
     )
    ;; GUI acts
    ((select ONT::select)
     (handle-map-select SA))
   
    (selection-determination-failed
     (handle-failure (add-gui-act-to-record SA 'GUI nil) 'GUI-failure nil))

    (otherwise
     (im-warn "Uninterpretable message context: ~S" SA)))
  (values))

;; ============
;;  HANDLING NEW INCOMING SPEECH ACT FROM USER
;;   We simply preprocess and record the speech acts for later processing.

(defun handle-new-speech-act (Sact channel)
  (let* ((uttnum (or (find-arg-in-act Sact :uttnum) 
		     (find-arg-in-act (car (find-arg-in-act Sact :acts)) :uttnum)))
	 (words (or (find-arg-in-act Sact :words) 
		    (find-arg-in-act (car (find-arg-in-act Sact :acts)) :words))))
    
    (get-speaker-and-update-current-channel channel) 
    ;; first break this apart into single Speech Acts
    (let ((act-sequence 
	   (segment-utterance Sact)))
      (if (or (and act-sequence (<= (list-length act-sequence) *max-allowed-utts-in-turn*))
	      (sa-is-response (car act-sequence))) ;; allow complex fragment that begin with a response
	  (progn (mapcar #'(lambda (a) (post-speech-act a channel))
		  act-sequence)
		 (post-speech-act (list 'utterance-end :uttnum uttnum
					:words words) channel))
	  ;; complete failure to understand
	  (progn
	    (trace-msg 1 "IM: Not interpreting because parse exceeds the max number of fragments")
	    (post-speech-act (list 'segmentation-failure :utt Sact  :uttnum uttnum
				   :words words) channel))
	))))

(defun find-uttnum (SA)
  (or (find-arg-in-act (cadr SA) :uttnum)
      (find-arg-in-act (car (find-arg-in-act (cadr SA) :acts)) :uttnum)))


(defun sa-is-response (sa)
  "T if the first speech act is a response"
  (let* ((root (find-arg-in-act sa :root))
	(sa (find-if #'(lambda (x) (eq (find-arg-in-act x :var) root))
		     (find-arg-in-act sa :terms))))
    (eq (caddr (find-arg-in-act sa :LF)) 'W::SA_RESPONSE)))
	 

(defun post-speech-act (a channel)
  (add-to-inputQ (append a (list :channel channel))))
  

(defun segment-utterance (Sact)
  (case (car Sact)
    (utt
     (let* ((rootID (find-arg-in-act Sact :root))
	    (terms (find-arg-in-act Sact :terms))
	    (uttnum (find-arg-in-act Sact :uttnum))
	    (rootSA (find-if #'(lambda (a) (eq (find-arg-in-act a :var) rootid)) terms)))
       (if (is-sa-seq rootSA)
	   (let ((acts (find-arg-in-act (find-arg-in-act rootSA :lf) :acts))
		 (context (remove-if #'(lambda (term) (member (car (find-arg-in-act term :lf)) '(ONT::SA-SEQ ONT::SPEECHACT)))
				     terms)))
	     
	      (mapcar #'(lambda (act-id)
				  (let ((act-term (find-if #'(lambda (x) (eq (find-arg-in-act x :var) act-id)) terms)))
				  `(utt :type w::utt :root ,act-id
					:terms ,(cons act-term (remove-unused-terms act-term context))
					:uttnum ,uttnum
					:input ,(find-arg-in-act act-term :input))))
			      acts))
	 (if terms (list Sact))  ;; if no LF terms, we simply ignore the speech act
	 )))
    (compound-communication-act
     (mapcan #'segment-utterance (find-arg-in-act Sact :acts)))))


(defun is-sa-seq (act)
  (eq (car (find-arg-in-act act :lf)) 'ONT::SA-SEQ))


