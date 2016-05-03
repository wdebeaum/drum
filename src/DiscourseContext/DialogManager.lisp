;;  The interface functions to the dialogue management capabilities

(in-package :dc)

(defvar *last-record* nil)

;; IM-record maintains a global record of every speech act processed and what is happening about it
(defvar *max-SA-count* 10000)
(defvar *DC-record* (make-array *max-SA-count*))
(defvar *DC-utt-count* -1)
(defvar *active-SA-index* 0)

(defun initialize nil
  (setq *DC-record* (make-array *max-SA-count*))
  (setq *DC-utt-count* -1)
  (setq *active-SA-index* 0)
  )

(defstruct DC-record id speaker addressee channel lf-root lf-context status akrl-context input syntax term-list focus discourse-act
	   cps-interpretation refKB)

(defstruct term-record id sem lf-context kr-context 
	   ref-hyps exclusions coref refers-to accessibility implicit
	   ;; last ones are for reusing old code (maybe be obsolete in a redesign)
	   lf lf-type)

(defstruct ref-hyp
  id        ;; the ID of the object that this is a hypothesis for
  lf-type   ;; the LF-type for this antecedent
  sem       ;; the SEM for this antecedent
  refers-to ;; the KR object ID it refers to (if any)
  coref     ;; the LF var that it refers to
  kr-context   ;; new context for this hypotheis (adds to or replaces stuff in referent context
  score     ;; TBA - some indicaiton of likelihood among the hypotheses
  )
  
(defun get-next-active-dc-record nil
  (aref *dc-record* *active-SA-index*))

(defun clear-dc-record nil
  (setq *dc-record* (make-array *max-sa-count*))
  (setq *dc-utt-count* -1)
  (values))

;;==========================================
;;==========================================
;;  CREATING NEW UTTERANCE RECORD
;;
                                                                                         
(defun add-new-utterance-record (&key id speaker addressee channel 
				 lf-root akrl-context 
				 status input syntax 
				 term-list discourse-act cps-interpretation 
				 number-referents-desired &allow-other-keys)
  (setq *dc-utt-count* (+ *dc-utt-count* 1))
  (let* ((terms (mapcar #'build-term-records term-list))
	 (head-term (find-if #'(lambda (x) (eq (term-record-id x) lf-root)) terms))
	 (lf-context (if head-term (term-record-lf-context head-term)))
	 (focus (identify-focus akrl-context lf-context))
	 (number-desired (or number-referents-desired 1)))
  
  (setq *last-record*
	(make-dc-record :id id 
		  :speaker speaker 
		  :addressee addressee 
		  :channel channel 
		  :lf-root lf-root 
		  :lf-context lf-context 
		  :status status 
		  :focus focus
		  :akrl-context akrl-context 
		  :input input 
		  :syntax syntax 
		  :cps-interpretation cps-interpretation
		  :discourse-act discourse-act
		  :term-list terms
		  :refKB (build-refKB lf-context speaker addressee)
		  ))
 
  (setf (aref *dc-record* *dc-utt-count*)
	*last-record*)
  (remove-if #'null
	  (mapcar #'(lambda (x) (convert-hyps-to-list (term-record-ref-hyps x)))
		  (find-possible-antecedents-in-act terms speaker addressee *dc-utt-count* number-desired)))
  ))

(defun build-term-records (def)
  "Builds a term structure from the message content"
  (let* ((id (find-arg-in-act def :id))
	 (sem (find-arg-in-act def :sem))
	 (lf-context (find-arg-in-act def :lf-context))
	 (kr-context (find-arg-in-act def :kr-context))
	 (refers-to (find-arg-in-act def :refers-to))
	 (coref (find-arg-in-act def :coref))
	 (head-lf (find-if #'(lambda (x) (eq (second x) id)) lf-context)))
    (make-term-record :id id
		      :sem (cons '$ (cdr sem))
		      :lf head-lf
		      :lf-type (third head-lf)
		      :lf-context lf-context
		      :kr-context kr-context
		      :accessibility (classify-accessibility head-lf)
		      :refers-to refers-to
		      :coref coref)
    ))

(defun convert-hyps-to-list (hyps)
  "convers a list of REF-HYPS to a list format"
  (when (and (consp hyps) (car hyps))  ;; exclude the (NIL) case if it comes up
    (mapcar #'convert-hyp-to-list hyps)))

(defun convert-hyp-to-list (hyp)
  (list 'HYP :id (ref-hyp-id hyp)
	:lf-type (ref-hyp-lf-type hyp)
	:refers-to (ref-hyp-refers-to hyp)
	:coref (ref-hyp-coref hyp)
	:kr-context (ref-hyp-kr-context hyp)
	:score (ref-hyp-score hyp))
  )


;;==========================================
;;==========================================
;;  HANDLING REFERENT RESOLUTION REQUESTS
;;

(defun find-possible-antecedents-in-act (refs speaker addressee index number-desired)
  "performs reference resolution on all terms in the specified discourse record, and 
    returns the number of hypotheses for each specified by NUMBER-DESIRED"
  (mapcar #'(lambda (r)
	      (let ((hyps (find-possible-hyps r index speaker addressee)))
		(setf (term-record-ref-hyps r) (if (and (consp hyps) (car hyps))
						   hyps))
		(first-n-in-list hyps number-desired)
		r))
	  refs))

(defun first-n-in-list (ll n)
  (when (> n 0)
    (cons (car ll) 
	  (first-n-in-list (cdr ll) (- n 1)))))

;;==========================================
;;==========================================
;;  
;;  I/O for DC records

(defun show-dc-record (&optional show-utt)
  "Show the dc-records. If optinal arg is given it shows the UTTS"
  (let ((i 0))
    (loop while (<= i *dc-utt-count*)
	  do
	  (show-individual-dc-record i show-utt)
	  (setq i (+ i 1)))))

(defun show-referents (&optional verbose)
  (let ((i 0))
    (loop while (<= i *dc-utt-count*)
	  do
	  (let ((rec (dc-record-term-list (get-dc-record i))))
	    (when rec
	      (format t "~%~%UTT RECORD ~S" i)
	      (mapc #'(lambda (r) (show-referent r verbose)) rec))
	  (setq i (+ i 1))))))

(defun show-referent (r verbose)
  (format t "~% ID: ~S~% :LF ~S~%" (term-record-id r) (term-record-lf r))
  (show-if-non-null :lf-type (term-record-lf-type r))
  (show-if-non-null :sem (term-record-sem r))
  (show-if-non-null :KR-context (term-record-kr-context r))
  (show-if-non-null :refers-to (term-record-refers-to r))
  (show-if-non-null :coref (term-record-coref r))
  (when verbose
    (mapc #'(lambda (x) (format t "~%      ~S" x)) (term-record-ref-hyps r))
    )
  )

(defun show-if-non-null (tag val)
  (if val
      (format t " ~S ~S~%" tag val)))

(defun get-dc-record (&optional i)
  (let ((index (or i *dc-utt-count*)))
    (if (<= index *dc-utt-count*)
	(aref *dc-record* index))))

(defun show-individual-dc-record (i show-utt)
  (let* ((rec (aref *dc-record* i))
	 (cps-act (dc-record-cps-interpretation rec)))
    (format t "~%~%UTT# ~S ID: ~S~%    speaker:~S  channel:~S  status:~S" i
	    (dc-record-id rec) (dc-record-speaker rec) (dc-record-channel rec)
	    (dc-record-status rec))
    
    (format t "~%    Utterance: ~S~%    LFs: ~S ~%   AKRL: ~S~%   CPS-interpretation: ~S~%    Discourse-Act: ~S~%    Focus: ~S    ~%KB: ~S"
	    (dc-record-input rec)
	    (dc-record-lf-context rec)
	    (dc-record-akrl-context rec)
	    (if (consp cps-act)
		(case (car cps-act)
		  (cps-act (find-arg-in-act cps-act :content))
		  (cps-interpretation (find-arg-in-act cps-act :action))
		  (otherwise cps-act))
		)
	    (dc-record-discourse-act rec)
	    (dc-record-focus rec)
	    (dc-record-refkb rec))
    (if show-utt (format t "~%    ACT:~S" (dc-record-syntax rec)))
    ))
