;; The Top level code for matching logical forms against conventional
;; patterns for Speech acts - stores the results in the discourse record

(in-package :IM)


(defun Do-speech-act-processing (rec index)
  "creates an ordered list of possible interpretations of the speech act at INDEX"
  (let* ((lfs (mapcar #'referent-lf (utt-record-referring-expressions rec)))
	 (results (process-speech-act index rec lfs (utt-record-channel rec))))
    ;;(if results
	;;(setf (utt-record-discourse-acts rec) results))
    results))

(defun process-speech-act (index rec LFS channel)
  (multiple-value-bind 
	(new-lfs conditionals)
      (transform-modals rec)
    (let ((interps (identify-dialog-act (utt-record-speaker rec) (make-lf-list rec) conditionals))
	   )
    ;; if we succeed we have at least produced some sort of mapping
      (trace-msg 1 "Mapped to CPSact: ~S" interps)
      (if (null interps)
	  ;;(setf (utt-record-discourse-acts rec) interps)
	  (handle-failure index 'mapping-failure channel)
	  interps
      )
    )))

(defun transform-modals (rec)
  "This pulls the modals out of the TMA structure and makes them top level features"
  (multiple-value-bind
	(newlfs conditionals)
      (extract-modals-and-conditionals (make-lf-list rec))
    (replace-lfs-in-record (append newlfs conditionals)
			   rec)
    (values newlfs conditionals)))

(defun flatten-mods (rec)
  "This flattens the temporal expressions in the LF into features. Returns only those LFs that are changed"
  (let* ((lfs (make-lf-list rec))
	 (newlfs (remove-if #'null (flatten-mod-expr lfs lfs))))
    (replace-lfs-in-record newlfs rec)))

(defun flatten-mod-expr (lfs context)
  (when lfs
    (let ((lf (car lfs)))
      (if (is-binary-mod lf)
	  (flatten-mod-expr (cdr lfs) context)   ;; temporal preds will be added to other lfs 
	  ;;  non-temporal pred, we check the mods
	  (multiple-value-bind
		(temp-mods other-mods)
	      (split-list #'(lambda (x)
			      (is-binary-mod (find-lf x context)))
			  (find-arg-in-act lf :mods))
	    (cons (add-mod-as-feature lf temp-mods other-mods context) (flatten-mod-expr (cdr lfs) context)))))))

(defun add-mod-as-feature (lf temp-mods other-mods context)
  "removes the temporal preds from the lf and adds them as features"
  (if temp-mods 
      (let ((newlf (if other-mods 
		       (replace-arg-in-act lf :mods other-mods)
		       (remove-arg-in-act lf :mods))))
	(append newlf (generate-mod-features temp-mods context)))))

(defun generate-mod-features (mods context)
  "returns list of features that capture the temporal mods"
  (when mods
    (let* ((lf (find-lf (car mods) context))
	   (type (third lf)))
      (case (simplify-type type)
	 ;; predicate with value as the type restriction
	((ont::sequence-position)
	 (list* (convert-to-package (simplify-type type) :keyword)
		(third type) (generate-mod-features (cdr mods) context)))
	 ;; any predicate with :of and :val
	 (otherwise
	  (list* (convert-to-package (simplify-type type) :keyword) (or (find-arg-in-act lf :val) (find-arg-in-act lf :is))
		 (generate-mod-features (cdr mods) context)))
	 ))
    ))
  	    
(defun is-binary-mod (lf)
  (or (and (find-arg-in-act lf :of)
	   (or (find-arg-in-act lf :val) (find-arg-in-act lf :val)))
      (member (simplify-type (third lf)) '(ont::sequence-position ont::while))))
      
	
			       

		  
;;=======================
;;   Code to transform the LFs Before matching

(defun extract-modals-and-conditionals (LFs)
  "We need to transform modals and extract if-conditionals in order
    to simplify the subsequent matching"
  ;;  extract conditionals if present
  (multiple-value-bind (newLFs conditional)
      (extract-conditionals LFS)
    ;; now interpret the modality and replace with new roles
    ;;(values (interpret-TMAS newLFS) (interpret-TMAS conditional))
    (values (interpret-tmas newLFs) (interpret-TMAs conditional))
    ))


(defun extract-conditionals (LFS)
  "The is handles the new treatment of conditional, which are just additional modifiers"
  (let ((condition 
	 (find-if #'(lambda (lf) 
		      (let ((lf-type (third lf)))
			(and (consp lf-type) (member (second lf-type) '(ONT::POS-CONDITION ONT::NEG-CONDITION)))))
		  lfs)))
    (if condition
	(let* ((sa (car lfs))
	       (main-id (find-arg-in-act condition :of)))
	  (if main-id
	      (let* ((modified-lf (find-lf main-id lfs))
		    (condition-lf (find-lf (find-arg-in-act condition :val) lfs))
		    ;; we remove the conditional MOD from the modified form
		    (new-lf (remove-mod modified-lf (second condition))))
		(values (list* sa new-lf (remove-unused-context new-lf (remove-if #'(lambda (x) (eq (second x) main-id)) lfs)))
			(cons condition (remove-unused-context condition-lf lfs))))
		;; ill-formed condition as there is no :of argument -- just give up
	      (values lfs)))
	  (values lfs))))

(defun interpret-TMAs (LFS)
  (if *compute-force-from-tmas*
      (mapcar #'(lambda (x) (interpret-TMA x lfs)) lfs)
      lfs))

(defun interpret-TMA (lf lfs)
  "This adds a :force feature to an existing LF" 
  (if (not (eq (car lf) 'ont::F))
      lf
      ;; we compute force only on predicates
      (let ((modal (find-arg-in-act lf :modality))
	    (neg (find-arg-in-act lf :negation))
	    (mods (find-all-args-for-slot (cdddr lf) :mod))
	    (frequencylf (find-def-in-lfs (find-arg-in-act lf :frequency) lfs)))
	(multiple-value-bind
	 (negmods othermods)
	 (split-list #'is-negation-modifier
		     (mapcar #'(lambda (x) (find-def-in-lfs x lfs)) mods))
	 
	 (setq neg 
	       (cond ((and (not (null neg)) (not (eq neg '-))) T) ;; if NEG is set (and not -) then negation
		     ;; NEG was not set, check for modifiers
		     (negmods T)))
	 (let* ((frequency-type (third frequencyLF))
		(frequency (if (consp frequency-type) (third frequency-type) frequency-type))
		(force
		 (if modal
		     (case (if (consp modal) (cadr modal) modal)
		       ((ONT::ABILITY ONT::CONDITIONAL ONT::POSSIBILITY)
			(if (or neg (eq frequency 'W::NEVER))
			    'ONT::IMPOSSIBLE 'ONT::POSSIBLE))
		       ((ONT::SHOULD ONT::MUST)
					     (if (or neg (eq frequency 'W::NEVER))
						 'ONT::PROHIBITED 'ONT::REQUIRED))
		       ((ONT::FUTURE ONT::GOING-TO) 
			(if (or (and neg (not (eq neg '-)))
				(eq frequency 'W::NEVER))
			    'ONT::FUTURENOT 'ONT::FUTURE))
		       (otherwise
			(if (or neg (eq frequency 'W::NEVER))
			    'ONT::FALSE 'ONT::TRUE))
		       )
		     ;; no modal
		     (if (or neg (eq frequency 'W::NEVER))
			 'ONT::FALSE 'ONT::TRUE)))
		)
	   ;;(if force
	   ;; replace the TMA and the frequency adverbials with the FORCE
	   #||  (let*
	   ((lf-revised-mods (if (and mods frequency)
	   (if newmods
	   (replace-arg-in-act lf :mods newmods)
	   (remove-arg-in-act lf :mods))
	   lf))
	   )||#
	   (append lf (list :force force)))
      ))))

(defun is-negation-modifier (lf)
  (and (consp lf) 
       (eq (car lf) 'ont::f) 
       (om::subtype (let ((type (third lf)))
		      (if (consp type) (second type) type))
		    'ont::neg)))
  

(defun extract-adverbials-by-type (type mods lfs)
  "Pulls outs any mod of the indicated type"
  (when mods
    (let* ((firstlf (find-def-in-lfs (car mods) lfs))
	   (adv-type (third firstlf)))
      ;; NC - adding the nested lisp to check for single LF atoms, not :* lists
      (if (eq type (if (listp adv-type) (second adv-type) adv-type))
	  (values firstlf (cdr mods))
	(multiple-value-bind
	    (freq-lf others)
	    (extract-adverbials-by-type type (cdr mods) lfs)
	  (values freq-lf (cons (car mods) others)))))))
