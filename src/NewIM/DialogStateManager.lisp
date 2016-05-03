(in-package :im)

;; Managing the discourse state

(defvar *root-state* (make-dstate :type 'root :I-rules '(SA-rules)))

(defvar *discourse-Stack* (list *root-state*))

;; acts that introduce content that must be grounded, and is available for ellipsis
(defvar *contentful-acts* '(ONT::PROPOSE ONT::ask ONT::report ONT::answer ONT::ask-if ONT::ask-what-is))

;;  IM record

(defun show-im-record (&optional show-utt)
  "Show the im-records. If optinal arg is given it shows the UTTS"
  (let ((i 0))
    (loop while (<= i *im-utt-count*)
	  do
	  (show-individual-record i show-utt)
	  (setq i (+ i 1)))))

(defun show-referents (&optional verbose)
  (let ((i 0))
    (loop while (<= i *im-utt-count*)
	  do
	  (let ((rec (utt-record-referring-expressions (get-im-record i))))
	    (when rec
	      (format t "~%~%UTT RECORD ~S" i)
	      (mapc #'(lambda (r) (show-referent r verbose)) rec))
	  (setq i (+ i 1))))))

(defun show-referent (r verbose)
  (format t "~% ID: ~S :LF ~S" (referent-id r) (referent-lf r))
  (show-if-non-null :lf-type (referent-lf-type r))
  (format t "~%      ")
  (show-if-non-null :refers-to (referent-refers-to r))
  (show-if-non-null :coref (referent-coref r))
  (when verbose
    (mapc #'(lambda (x) (format t "~%      ~S" x)) (referent-ref-hyps r))
    )
  )

(defun show-if-non-null (tag val)
  (if val
      (format t " ~S ~S" tag val)))

(defun get-im-record (&optional i)
  (let ((index (or i *im-utt-count*)))
    (if (<= index *im-utt-count*)
	(aref *im-record* index))))

(defun show-individual-record (i show-utt)
  (let* ((rec (aref *im-record* i)))
	 ;;(cps-act (or (utt-record-cps-interpretation rec) (utt-record-cps-act rec))))
    (format t "~%~%ID ~S UTTNUM: ~S~%    speaker:~S  channel:~S    processing status:~S input:~S" i
	    (utt-record-uttnum rec) (utt-record-speaker rec) (utt-record-channel rec)
	    (utt-record-processing-status rec) (utt-record-input rec) )
    ))

(defun add-to-im-record (act)
  "add an act to the record"
  (let* ((channel (find-arg-in-act act :channel))
	 (speaker (find-arg-in-act act :speaker))
	 (sa-id (find-arg-in-act act :root))
	 (terms (find-arg-in-act act :terms))
	 (lfs (or (extract-lfs terms) (find-arg-in-act act :lfs)))
	 (sa (find-lf sa-id lfs))
	 (root (find-arg-in-act sa :content))
	 (rootLF (find-lf root lfs))
	 (input (find-arg-in-act act :words))
	 (uttnum (find-arg-in-act act :uttnum))
	 (roles (extract-roles-from-arglist (cdddr rootLF)))
	 )
    (setq *im-utt-count* (+ *im-utt-count* 1))
    (log-msg `(LOG-SPEECHACT :input ,input :lf ,lfs :uttnum ,uttnum) *im-utt-count*)
    (setf (aref *im-record* *im-utt-count*)
	  (make-utt-record
			   :speaker (or speaker (identify-speaker-from-channel channel))
			   :channel channel
			   :status 'pending
			   :speechactId sa-id
			  ;; :root root
			   :lf-ids (mapcar #'second lfs)
			   :referring-expressions (build-referent-structures-from-terms terms roles)
			   :input input
			   :index *im-utt-count*
			   :uttnum uttnum)
			   )
    *im-utt-count*))

(defun extract-roles-from-arglist (lf)
  "finds the grammatical roles LSUBJ, DOBJ, IOBJ in an LF - returns an assoc list"
  (when lf
    (if (member (car lf) '(:LSUBJ :LOBJ :LIOBJ :AGENT :ADDRESSEE :AFFECTED :CAUSE :THEME))
	(list* (list (second lf) (car lf))
	       (extract-roles-from-arglist (cddr lf)))
	(extract-roles-from-arglist (cddr lf)))))

(defun refine-vague-relations (lfs)
  "tries to refine assoc-with relations"
  (mapcar #'(lambda (x) (refine-vague-relation x lfs))
	  lfs))

(defun refine-vague-relation (lf lfs)
  (let ((arg-var (find-arg-in-act lf :assoc-poss)))
    (if arg-var
	(let* ((core-type (third lf))
	       (arg-def (find-if #'(lambda (x) (equal arg-var (second x))) lfs))
	       (arg-type (third arg-def))
	       (refined-rel (lookup-assoc-rel core-type arg-type)))
	  
	      (append (remove-arg-in-act lf :assoc-poss) 
		      (if refined-rel (list refined-rel (second arg-def))
			  (list :assoc-with (second arg-def)))))
	lf)))
    
(defun lookup-assoc-rel (core-type arg-type)
  "tries to refine an :assoc-with relation to something more specified. Currently
     only looks for +reln nouns, btu eventually we could do more"
  (when (and (consp core-type) (third core-type))
    (let ((refined-role (lxm::is-reln (third core-type) (second core-type))))
      (if refined-role
	  (convert-to-package (car refined-role) :keyword)))))

(defun augment-im-record-with-prior-information (index newlfs prior-index)
  "This adds more LFs, and their corresponding terms, to the IM record. Typically this new information
    has been derived from ellipsis processing"
  (let* ((rec (get-im-record index))
	 (prior-rec (get-im-record prior-index))
	 (new-ids (mapcar #'cadr newlfs))
	 (prior-terms (find-arg-in-act (utt-record-speechactid prior-rec) :terms))
	 (relevant-terms (remove-if-not #'(lambda (x) (member (find-arg-in-act x :var) new-ids)) prior-terms)))
    (setf (utt-record-lfs rec) (append (utt-record-lfs rec) newlfs))
    (setf (utt-record-referring-expressions rec) (append (utt-record-referring-expressions rec)
					    (build-referent-structures-from-terms relevant-terms)))))
    
(defun build-referent-structures-from-terms (terms roles)
  (mapcar #'(lambda (term) (build-referent-structure term roles))
		      terms))


(defun add-sys-act-to-record (defaultspeaker utt cps-act input)
  ;;  This probaly needs fixing
  "This adds the sys utt to the record in one swoop, since we don't have to do interpretation on it.
   UTT is a list of LF's at present"
  (declare (ignore defaultspeaker))
  (let* ((rec (get-im-record (add-to-im-record utt)))
	 )
    (setf (utt-record-input rec) input)
    ;(setf (utt-record-discourse-acts rec) (list cps-act))
    (setf (utt-record-status rec) 'success)
    (setf (utt-record-processing-status rec) 'SYS)
    )
  *im-utt-count*)

(defun get-lf (x)
  (when (consp x)
    (if (eq (car x) 'term)
	(find-arg-in-act x :lf)
      x)))

(defun add-gui-act-to-record (act map-id add?)
  "This adds the Gui utt to the record in one swoop, since we don't have to do interpretation on it.
only create a new record if we are following an utterance - otherwise all GUI actions go in same record"
  (let* ((id (gen-symbol 'a))
	(channel-id (or map-id (if (channel-p *current-channel*)
				     (channel-id *current-channel*))))
	(current-record (if (> *im-utt-count* -1)
			    (aref *im-record* *im-utt-count*)
			  (make-utt-record)))
	;;(prev-record (if current-record (utt-record-part-of current-record)))
	)
    (if (not (utt-record-gestures current-record))
	(progn
	  (setq *im-utt-count* (+ *im-utt-count* 1))
	  (setf (aref *im-record* *im-utt-count*)
		(make-utt-record :id id
				 :speaker 'ONT::USER ;; (identify-speaker-from-channel channel-id)
				 :channel channel-id
				 :act (list act)
				 :mapped nil
				 :status 'success
				 :referring-expressions (if add? (build-reference-objects-from-GUI-act act))
				 :processing-status 'dispatched
				 :gestures T
				 )))
      (setf (utt-record-speechactid current-record) (cons act (utt-record-speechactid current-record))))
	
    *im-utt-count*))

(defun merge-gui-references (old new)
  "This augments an existing set of GUI referents with subsequent args"
  (if (null (guicontrol-live? *gui-control*))
      new
    (remove-doubles old new nil)))

(defun remove-doubles (old new tokeep)
  (if old
    (let* ((next (car old)))
      (if (some #'(lambda (x) (eq (referent-id x) (referent-id next))) new)
	  (remove-doubles (cdr old) new tokeep)
	(remove-doubles (cdr old) new (cons next tokeep))))
    (append new (reverse tokeep))))

(defun get-recent-gui-acts nil
  (let ((i (find-last-gui-interaction (- *im-utt-count* 1))))
  (if i
      (utt-record-speechactid (aref *im-record* i)))))

(defun find-last-gui-interaction (index)
  (when (>= index 0)
    (let ((rec (aref *im-record* index)))
      (if (utt-record-gestures rec)
	  index
	;; don't go back over previously successfully interpreted utterances
	(if (not (eq (utt-record-status rec) 'success))
	    (find-last-gui-interaction (- index 1)))))))
    		 
(defun build-reference-objects-from-gui-act (act)
  "Here we try to extract out objects that might be referred to later, on a case by case basis, unfortunately"
  (let ((object (find-arg act :object)))
    (list (make-referent-for-GUI-object
	   (case (find-arg act :instance-of)
	     (ONT::open-uri 'ONT::WEBSITE)
	     (ONT::select 'ONT::TEXT-REPRESENTATION)
	     (otherwise 'ONT::OBJECT))
	   :name-of object))))
      

(defun make-referent-for-GUI-object (lf-type role value)
  (let ((id (gen-symbol 'gui))
	(kr-type (om::lf-atom-transform lf-type)))
  (make-referent :id id
		 :lf `(ONT::A ,id ,lf-type)
		 :lf-type lf-type
		 :implicit T
		 :kr-context `((ONT::A ,id :instance-of ,kr-type ,role ,value))
		 :accessibility 'visible-focus)))
		 

(defun replace-lfs-in-record (lfs rec)
  (let ((exprs (utt-record-referring-expressions rec)))
    (mapcar #'(lambda (lf) 
		(let* ((id (second lf))
		       (expr (find-if #'(lambda (e) (eq (referent-id e) id)) exprs)))
		  (replace-lf-in-store lf)
		  (setf (referent-lf expr)
			(find-lf (referent-id expr) lfs))))
	    lfs)))
 
;;============

;;  This file contains the code to manage the dialogue state.
;;  It can be conceptualized as a set of FSM's running concurrently that maintain
;;  the communication process: tracking the status of communication channels,
;;  the status of proposals and requests, etc

;; One of the key roles is generated dialogue state specific expectations for
;;  interpreting the subsequent utterances

(defun get-active-interpretation-rules nil
  (mapcar #'dstate-I-rules *discourse-stack*)
  )

;; CHANNEL-MAINTENANCE
;;   maintains communication state - which is created by channels opening, agents speaking, etc.

;; The current open channel being used
;; we default it to HELEN for now to help debugging
(defvar *current-channel* nil)

(setq *current-channel*
     (make-channel :ID 'unknown :conversant *user* :status 'closed))

;; gf: This function seems to conflate conversants and channels
(defun get-speaker-and-update-current-channel (id)
  (if (or (null id)   ;; if no channel is specified, it stays at the old value
	  (and *current-channel* (eq id (channel-id *current-channel*))))
      (channel-conversant *current-channel*)
    (let ((new-channel (cdr (assoc id *available-channels*))))
      (if new-channel
	  (progn
	    (im-warn "new channel: ~S" id)
	    (setq *current-channel* new-channel)
	    (send-current-channel-update))
	  (progn 
	    (im-warn "unknown channel id: ~S. I am creating a new channel" id)
	    (setq *current-channel* (make-channel :id id :conversant id))
	    (setq *available-channels* (cons (cons id *current-channel*) *available-channels*))
	    (send-current-channel-update)))
      (channel-conversant *current-channel*))))

(defun identify-speaker-from-channel (id)
  (if id
      (let ((ch (cdr (assoc id *available-channels*))))
	(if (channel-p ch) (channel-conversant ch)))
    (if (channel-p *current-channel*)
	(channel-conversant *current-channel*))))

 
(defun process-channel-act (channel-act)
  (let* ((act-type (car channel-act))
	 (channel-id (find-arg-in-act channel-act :channel))
	 (channel (get-or-make-channel channel-id))
	 )
    (case act-type
      (CHANNEL-OPENED
       (let ((conversant (find-arg-in-act channel-act :id)))
	 (if (eq (channel-status channel) 'open)  (im-warn "Opening a channel that is not closed: ~S" channel-act))
	 (trace-msg 1 "Opening channel ~S with ~S" channel-id conversant)
	 (setf (channel-status channel) 'open)
	 (if conversant (setf (channel-conversant channel) conversant))
	 (setq *current-channel* channel)
	 (send-current-channel-update)
	 ;;(output (build-GM-call-with-obligation nil channel-act nil)))
       ))
     
      (CHANNEL-CLOSED
       (if (eq (channel-status channel) 'open)
	   (progn
	     (setf (channel-status channel) 'closed)
	     (setq *current-channel* nil)
	     (send-current-channel-update))
	 (im-warn "Trying to close a channel that is not open")))

     #||  ;; obsolete???
      (channel-clear
       ;; when the channel is clear we save the LF info away (printing the graph if desired)
       (save-lf-graph nil nil)
       
       (when (and *paragraph-lfs* *paragraph-done*) 
	 (unless *LF-output-by-utterance*
	   (send-msg `(tell :content (paragraph-lfs :content (paragraph :terms ,*paragraph-lfs*)))))
	 (paragraph-completed)
	 )
       )
      ||#
      )
    ))


(defun send-current-channel-update ()
  "Generate event for a channelkb to track, if it exists."
  (output `(tell :content (channel-event
			   :content (current-channel :channel ,(and *current-channel* (channel-id *current-channel*)))))))

(defun update-turn-info-with-new-speaker (msg)
  "Keeps count of acts in progress, so we can know whether the parser is still working on stuff"
  ;; gf: Completely ignore system utts (output direction)
  (when (not (eq (find-arg-in-act msg :direction) 'output))
    (let* ((speaker (channel-conversant *current-channel*))
	   (channel-id (find-arg-in-act msg :channel))
	   (conversant (get-speaker-and-update-current-channel channel-id)))
	(case (car msg)
	  (started-speaking
	   ;;(output `(TELL :content (TAKE-TURN :who ,speaker :channel ,channel-id)))
	   (when (not (eq (channel-status *current-channel*) 'pending))
	     (output `(TELL :content (channel-event :content (CHANNEL-PENDING :channel ,channel-id))))
	     (setf (channel-status *current-channel*) 'pending)
	     )
	   )
	  
	  ;;(stopped-speaking
	  ;; (release-turn-if-nothing-pending *current-channel*))
	   ;;(output `(TELL :content (RELEASE-TURN :who ,speaker :channel channel-id))))
	  ))))


