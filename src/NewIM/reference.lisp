;;  Basic reference module fill in
;; we keep the referents from the last utterance, then everything in a simple history list 

(in-package :im)

(defun Do-discourse-reference (index)
  (let ((refs (find-possible-antecedents-in-act index)))
    (mapcar #'(lambda (ref) (install-hyp-in-referent ref (car (referent-ref-hyps ref)))) refs)
   ;; (sort-referents-by-level-of-focus index)
    (set-processing-status index 'reference)))

(defun add-reference-info-in-lfs (lfs)
  "adds the REF and COREF slots to the LF based on the REFERENT record"
  (mapcar #'(lambda (lf)
	      (let* ((rec (get-referential-info (cadr lf))))
		(if rec
		    (let ((ref (referent-refers-to rec))
			  (coref (referent-coref rec))
			  (kb-assoc-with (referent-kb-assoc-with rec)))
		      (if ref
			  (if coref 
			      (if kb-assoc-with
				  (append lf (list :REFERS-TO ref :COREF coref :KB-ASSOC-WITH kb-assoc-with))
				  (append lf (list :REFERS-TO ref :COREF coref)))
			      (if kb-assoc-with
				  (append lf (list :REFERS-TO ref :KB-ASSOC-WITH kb-assoc-with))
				  (append lf (list :REFERS-TO ref))))
			  (if coref
			      (if kb-assoc-with
				  (append lf (list :COREF coref :KB-ASSOC-WITH kb-assoc-with))
				  (append lf (list :COREF coref)))
			      (if kb-assoc-with
				  (append lf (list :KB-ASSOC-WITH kb-assoc-with))
				  lf))))
		    lf)))
	  lfs))
			      
;; Functions for manipulating referent structures

(defvar *referents* nil)

(defun init-referents ()
  (setq *referents* nil))

(defun add-to-referent-lookup (ref)
  (setq *referents*
	(cons (cons (referent-id ref) ref)
	      *referents*))
  ref)

(defun get-referential-info (id)
  (cdr (assoc id *referents*)))

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
  (format t "~% ID: ~S accessibility: ~S  ~%LF: ~S~%" 
	  (referent-id r) (referent-accessibility r) (referent-lf r))
  (show-if-non-null :lf-type (referent-lf-type r))
  (show-if-non-null :refers-to (referent-refers-to r))
  (show-if-non-null :coref (referent-coref r))
  (show-if-non-null :role (referent-role r))
  (when verbose
    (mapc #'(lambda (x) (format t "~%      ~S" x)) (referent-ref-hyps r))
    )
  )

(defun show-if-non-null (tag val)
  (if val
      (format t " ~S ~S~%" tag val)))

(defun build-referent-structure (term grammroles)
  "builds the basic referent structure from the information in TERM"
  (let* ((lf (find-arg-in-act term :lf))
	 (id (second lf))
	 (start (find-arg-in-act term :start))
	 (end (find-arg-in-act term :end)))
	   
    (storeLF lf start end)
    (trace-msg 4 "~%Putting LF in LF Store: ~S" lf)
    (add-to-referent-lookup
     (make-referent
      :id id
      :role (cadr (assoc id grammroles))
      :num (classify-num lf)
      :lf LF
      :lf-type (simplify-generic-type (third lf))
      :accessibility (classify-accessibility LF)
      :coref (find-arg-in-act lf :coref)
      :refers-to (find-arg-in-act lf :refers-to)
      :start start
      :end end
      )
     )))

(defun replace-lf-in-referent (lf refs)
  (let* ((id (second lf))
	 (ref (find-if #'(lambda (x) (eq (referent-id x) id)) refs)))
    (setf (referent-lf ref) lf)))

(defun classify-num (lf)
  (case (first lf) 
    ((ont::the-set ont::indef-set ont::pro-set) 'set)
    ((ont::the ont::a ont::pro ont::F) 'individual)
    (otherwise 'other)))

(defun read-sem (x)
  (read-expression (expand-or x))
  )

(defun expand-or (x)
  (when x
    (if (consp x)
	(if (eq (car x) 'ONT::<OR>)
	    (list* '? (gen-symbol 'sem) (cdr x))
	  (reuse-cons (expand-or (car x))
		      (expand-or (cdr x))
		      x))
      x)))

(defun classify-accessibility (lf)
  (case (car lf)
    ((ONT::THE ONT::THE-SET ONT::A ONT::INDEF-SET)
     'concrete)
    ((ONT::PRO ONT::IMPRO ONT::PRO-SET)  ;; Pro's are concrete refs except for the first and second person and demonstrtives, which are handled specially
     (case (find-arg-in-act lf :proform) 
       ((ont::ME ont::I ont::YOU ont::*YOU* ont::MY ont::YOUR ont::OUR)
	'pro-1st-2nd)
       ((ont::here ont::this ont::that ont::those ont::these ont::there ont::today )
	'diectic)
       (t
	(if (member (caddr lf) '(ont::EXPLETIVE ont::expletive))
	    'expletive
	    (case (car lf)
	      (ONT::PRO 'concrete)
	      (ONT::IMPRO 'impro))))))
    (ONT::F
     ;; until the ontology is reorganized, we need to enumerate all (mostly) stative classes
     (let ((lf-type (get-lf-type lf)))
       (if (is-stative lf-type)
	   'state
	   (if (not (eq lf-type 'ont::do))  ;; ont::do does not generate a referent
	       'event))))
    ((ONT::BARE ONT::KIND) 'kind)
    (ONT::WH-TERM 'wh-term)
    (otherwise 'abstract)))

(defun is-stative (lf-type)
  (or (member lf-type '(ont::salience ont::rely ont::state-of-being ont::share ont::accommodate ont::compliance ont::correlation ont::function ont::lifecycle-stage ont::containment ont::necessity ont::regretting ont::associate ont::confuse))
	  (subtype-check lf-type 'ont::event-of-state)
	  ))

(defun install-hyp-in-referent (ref hyp)
  "We have picked a HYP and now update the discourse state to reflect this"
  (if hyp
      (let* ((id (referent-id ref)))	    
	(setf (referent-refers-to ref) (ref-hyp-refers-to hyp))
	
	(if (and (or (ref-hyp-refers-to hyp) ;; if the ID's differ, this was a real anaphoric reference and we make it concrete
		     (ref-hyp-coref hyp))
		 (not (eq (referent-accessibility ref) 'pro-1st-2nd)))
	    (setf (referent-accessibility ref) 'concrete))  
	(setf (referent-coref ref) (ref-hyp-coref hyp))
	(if (ref-hyp-lf-type hyp)
	    (setf (referent-lf-type ref) (simplify-lf-type (ref-hyp-lf-type hyp) (referent-lf ref))))
	;; add to the LF
	(let* ((ref-exprs (if (referent-refers-to ref) (list :refers-to (referent-refers-to ref))
			      (if (referent-coref ref) (list :coref (referent-coref ref)))))
	       (newlf (if ref-exprs (append (referent-lf ref) ref-exprs))))
	  (when newlf
	    (setf (referent-lf ref) newlf)
	    (storeLF newlf (referent-start ref) (referent-end ref) :replace t)))
      
    ;; there was no referent -- if a demonstrative, pro, impro or any other non-semantically specified referent,
    ;;    we prevent it form being accesible as an antecedent
    ;; Not a good strategy with language learning
    #||(progn
      (if (eq (referent-lf-type ref) 'ONT::REFERENTIAL-SEM)
	  (setf (referent-accessibility ref) 'FAILED))||#
      
	))
  ref)


(defun simplify-lf-type (lf-type lf)
  "This simplfies the types of PRO forms as they contains lexical information that's not now needed"
  (if (and (eq (car lf) 'ONT::PRO)
	   (consp lf-type))
      (second lf-type)
    lf-type))

(defun find-possible-antecedents-in-act (n)
  (let* ((rec (get-im-record n))
	 (refs (utt-record-referring-expressions rec))
	 (speaker (utt-record-speaker rec))
	 (addressee (if (eq speaker *me*) (channel-conversant *current-channel*) *me*)))
    (mapcar #'(lambda (r)
		(let ((hyps (find-possible-hyps r n speaker addressee)))
		  (setf (referent-ref-hyps r) hyps)
		  r))
	    refs)))

(defun find-possible-hyps (ref index speaker addressee)
  "Returns a list REF-HYPs with referential information added"
  (let* ((lf (referent-lf ref))
	 (hyps
	  (case (car lf)
	   ((ONT::PRO ONT::PRO-SET)
	    (resolve-personal-pronoun lf index speaker addressee)
	    )
	   (ONT::IMPRO
	    (resolve-impro lf index)
	    )
	   ((ONT::THE ONT::THE-SET)
	    (resolve-definite-reference lf index speaker addressee))
	   (ONT::QUANTIFIER
	    (resolve-quantifier lf index)
	    )
	   (t   ;; generate null ref entries for other terms
	    ;;(list (generate-referent-from-description lf KRinfo nil)))
	   ))))
     hyps
    ))

;;  FOCUS-BASED ordering
;;  here we order the possible antecedents by their level of salience
;; In general, we have the following factors:
;;    -- whether the term successful refers to something earlier
;;    -- the class: with PRO,IMPRO and THE being the key components
;;    -- "contentful" wh-terms - e.g., which truck, but not relaitonal ones such as the location in a "where" Q
;;    -- whether the term plays a direct role in the top eventuality.
;;   things not in this category end up at the end of the list.

(defun sort-referents-by-level-of-focus (index)
  (let* ((rec (get-im-record index))
	 (sorted-scored-refs (sort-referents-by-focus (utt-record-lfs rec) (utt-record-referring-expressions rec))))
    (setf (utt-record-referring-expressions rec) sorted-scored-refs)))

(defun sort-referents-by-focus (lfs referents)
  (let* ((direct-roles (find-top-level-roles lfs))
	(scored-refs (mapcar #'(lambda (x) (score-focus x direct-roles)) referents))
	(sorted-scored-refs (sort scored-refs #'> :key #'car)))
    (trace-msg 4 "Sorted referents by salience: ~%~S" sorted-scored-refs)
    (mapcar #'cdr sorted-scored-refs)))

(defun score-focus (rec top-level-roles)
  (let ((score (if (member (referent-id rec) top-level-roles) 1 0))
	(lf (referent-lf rec)))
    (if (not (eq (Classify-accessibility lf) 'pro-1st-2nd))
	(progn
	  (setq score (+ score (case (car lf)
				 ((ONT::PRO ONT::IMPRO)
				  (if (referent-coref rec) 
				      4)
				  3)
				 (ONT::THE (if (referent-coref rec)
					      3)
					  2)
				 ((ONT::QUANTITY-TERM ONT::BARE ONT::KIND) 1)
				 (otherwise 0))))
	  (setq score (+ score (if (referent-refers-to rec) 1 0))) ;; extra score for domain relevance
	  (cons score rec))
	(cons 0 rec))))
    
(defun find-top-level-roles (lfs)
  (let* ((sa (find-if #'(lambda (x) (eq (car x) 'ONT::SPEECHACT)) lfs))
	 (content-id (find-arg-in-act sa :content))
	 (content (find-if  #'(lambda (x) (eq (second x) content-id)) lfs))
	 (args (get-every-other (cddddr content))))
    (remove-if-not #'symbolp args)))

(defun get-every-other (ll)
    (when ll
      (cons (car ll) (get-every-other (cddr ll)))))
	 	  
(defun separate-by-pro-the (referents)
  (multiple-value-bind (pros nopros)
      (split-list #'(lambda (x) (member (car (referent-lf x)) '(ONT::PRO ONT::IMPRO))) referents)
    (multiple-value-bind (defs nodefs)
	(split-list #'(lambda (x) (member (car (referent-lf x)) '(ONT::THE))) nopros)
      (values pros defs nodefs)
      )))



;; old code -- to be deleted
(defun identify-focus (context lfs)
  "This identifies the focus/center of the utterance -- we only set when it has been
    referred to -- we look for pronouns, Impros, definites that have a referent"
  (let* ((candidates (mapcar #'cadr (remove-if-not #'(lambda (x) (and (eq (car x) 'ONT::THE)
						      (find-arg x :EQUALS)))
						  context)))
	(focus-id (cadr (car (sort-for-focus lfs candidates)))))
    focus-id ;;(find-arg (find-if #'(lambda (x) (eq (cadr x) focus-id)) context) :EQUALS)
    ))

(defun sort-for-focus (lfs possible-ids)
  "Order PROs first, then IMPRO, the Def descriptions"
  (let* ((candidates (remove-if-not #'(lambda (x) (and ;;(member (cadr x) possible-ids)
						       (member (car x) '(ONT::PRO ONT::IMPRO ONT::THE))))
				    lfs))
	 (pros (remove-if-not #'(lambda (x) (and (eq (car x) 'ONT::PRO)
						 (third-person-pro x)))
				  candidates))
	 (impros (remove-if-not #'(lambda (x) (eq (car x) 'ONT::IMPRO)) candidates))
	 (defs (remove-if-not #'(lambda (x) (eq (car x) 'ONT::THE)) candidates)))
    (append pros impros defs)))
    
(defun resolve-personal-pronoun (lf index speaker addressee)
  ;;(format t "~%~%REsolving ~S" lf)
  (let* ((id (second lf))
	 (cr (or (find-arg-in-act lf :proform) 
		 (if (consp (third lf)) (third (third lf)))))
	 (val
	  (case cr
	    ((ont::i ont::me ont::my ont::myself)
	     (list (make-ref-hyp :id id :refers-to speaker)))
	    ((ont::you ont::your ont::yourself) 
	     (list (make-ref-hyp :id id :refers-to addressee)))
	    ((ont::we ont::our ont::us ont::ourself) 
	     (list (make-ref-hyp :id id ;;:refers-to id
				 :lf-type '(ONT::SET-OF ONT::PERSON)
				 :refers-to 'ONT::US)))
	    ((ont::it ont::its)
	     (when (non-expletive lf)
	       (or (resolve-pro-fn lf '(concrete) '(individual) index (fn-no-human 'ONT::REFERENTIAL-SEM) 3)
		 (resolve-pro-fn lf '(event wh-term state) '(individual) index #'(lambda (x) T) 3))))

	    ((ont::itself)
	     (resolve-reflexive lf index))
	    
	    ((ont::this ont::that ont::these ont::those)
	     (when (non-expletive lf)
	       (resolve-this-that lf index)))
	    
	    ((ont::they ont::them ont::their)
	     (resolve-pro-fn lf '(concrete kind) '(set) index 
			     #'(lambda (x)
				 (subtype-check (referent-lf-type x)  'ONT::REFERENTIAL-SEM ))
				 3))

	    ((ont::he ont::him ont::his ont::she ont::her)
	     (or (resolve-pro-fn lf '(concrete) '(individual) index 
				 #'(lambda (x) 
				     x)
				     ;;(subtype-check (referent-lf-type x) (if (member cr '(ont::he ont::him ont::his))
							;;	       'ONT::MALE-PERSON 'ont::female-person)))
			     3)
		 ;;   backoff strategy for gendered pronouns - just look for people
		 (resolve-pro-fn (list* (car lf) (cadr lf) 'ont::person (cdddr lf))
				 '(concrete) '(individual) index 
				 #'(lambda (x) 
				     (subtype-check (referent-lf-type x) 'ont::person))
				 3)))
		 
	    
	    ((ont::here ont::there) (find-location-ref id lf (- index 1)))

	    ((ont::tonight ont::today)
	      (find-temporal-ref cr lf))

	    ((ont::which ont::who)  ;; these occur in relative clause fragments
	     (resolve-relative-pro lf index))
	    )))
	    
    val))

(defun non-expletive (lf)
  (not (eq (third lf) 'w::expletive)))

(defun find-temporal-ref (pro lf)
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (case pro
      ((W::TONIGHT W::TODAY ont::tonight ont::today)
       (build-temporal-ref lf month day year))
      ((W::TOMORROW ont::tomorrow)
       (build-temporal-plus-1 lf month day year))
      ((W::YESTERDAY ont::yesterday)
       (if (> day 1)
	   (build-temporal-ref lf month (- day 1) year)))
      )))

(defun build-temporal-plus-1 (lf month day year)
  (if (or (< day 27) 
	  (and (not (eql month 2)) (< day 29))
	  (and (not (member month '(2 9 4 6 11))) (< day 30))
	  (and (member month '(1 3 5 7 8 10 12)) (eql day 30))
	  )
      (build-temporal-ref lf month (+ day 1) year)
      (if (< month 12)
	  (build-temporal-ref lf (+ month 1) 1 year)
	  (build-temporal-ref lf 1 1 (+ year 1)))))

(defun build-temporal-ref (lf month day year)
  (let ((datestring (format nil "~A ~S, ~S" (find-month month) day year)))
  (list (bind-to-referent lf
			  (make-referent :id (second lf)
					 :implicit t
					 :lf (append lf (list :month month :day day :year year))
					 :lf-type (simplify-lf-type (third lf) lf)
					 :accessibility (classify-accessibility LF)
					 #||:kr-context (list `(ONT::THE ,(second lf) :instance-of ONT::|Cal-Time| 
								     :|time-month| ,month :|time-day| ,day :|time-year| ,year
								     :|the-name| ,(list datestring)))||#
			  )))))

(defun find-month (n)
  (cadr (assoc n
	       '((1 "jan") (2 "feb") (3 "Mar") (4 "Apr") (5 "may") (6 "jun") 
		 (7 "jul") (8 "aug") (9 "sep") (10 "oct") (11 "nov") (12 "dec")))))

(defun fn-no-human (type)
  #'(lambda (x)
      (and (subtype-check (referent-lf-type x) type)
	   (it-exclusion (referent-lf-type x))))
  )

(defun it-exclusion (type)
  (not (om::subtype type 'ONT::PERSON)))

(defun third-person-pro (pro)
  (member (find-arg pro :proform)
	  '(W::IT W::THEM W::HE W::HIM W::SHE W::HER ont::it ont::them ont::he ont::him ont::she ont::her)))

    
(defun resolve-pro-fn (lf access num index fn count)
  "Personal pronouns should refer to a concrete object matching the criteria in the recent history.
    We check in the current utter1ance for possible referents that come before the pro form"
  (let ((ans (or (find-possible-referents-in-current-sentence (get-lf-type lf) (second lf) access num index)
		 (search-for-possible-refs (get-lf-type lf) (second lf) access num (- index 1) count fn))))

	(mapcar #'(lambda (a) (bind-to-referent lf a)) ans)))



(defun resolve-this-that (lf index)
  "this and that allow reference to visually focussed objects as well as discourse concrete and abstract"
  (let ((possibles (progressive-search-for-possible-refs (get-lf-type lf) (second lf)
							 '(visible-focus concrete event abstract)
							 (list (classify-num lf))
							 (- index 1)
							 1 ;;2
							 #'(lambda (x) (it-exclusion (referent-lf-type x))))))
    (mapcar #'(lambda (a) (bind-to-referent lf a)) possibles)))

(defun resolve-relative-pro (lf index)
  "relative pronouns only arise in the case of fragemented sentences, and thus it should refer to the 
    last viable NP in the previous utterance"
  (let ((possibles (last (search-for-possible-refs (get-lf-type lf) (second lf)
							 '(concrete)
							 (list (classify-num lf))
							 (- index 1)
							 1 
							 #'(lambda (x) x)))))
    (mapcar #'(lambda (a) (bind-to-referent lf a)) possibles)))

(defun resolve-reflexive (lf index)
  "reflexives must be in the same sentence!"
  (let* ((id (second lf))
	 (terms (utt-record-referring-expressions (get-im-record index)))
	 ;; first find possible candidates in the same sentence
	 (possibles (remove-if-not #'(lambda (x) (and (referent-p x) 
						      (not (eq (referent-id x) id))
						      (eq (referent-accessibility x) 'concrete)))
				   terms))

	 ;;  now filter by semantic role
	 (results (or (remove-if-not #'(lambda (x) (eq (referent-role x) :agent))
				  terms)
		      (remove-if-not #'(lambda (x) (eq (referent-role x) :affected))
				  terms))))
    (mapcar #'(lambda (a) (bind-to-referent lf a)) results)))
		   
(defun progressive-search-for-possible-refs (lf-type id access num index range fn)
  "This searches discourse history one accessibility reln at a time, stopping at the first one found"
  (when access
    (or (search-for-possible-refs lf-type id (list (car access)) num index range fn)
	 (progressive-search-for-possible-refs lf-type id (cdr access) num index range fn))))

(defun search-for-possible-refs (lf-type id access num index range fn)
  (remove-if-not fn (find-most-salient lf-type id access num index range)))

(defun sort-by-access (results access)
  results)

(defun resolve-impro (lf index)
  (let* ((id (second lf))
	 (lf-type (get-lf-type lf))
	 (cr (find-arg-in-act lf :proform))
	 (sem (find-arg-in-act lf :sem))
	(hyps (case cr
		((W::*YOU* ont::*you*) 
		 (list (make-ref-hyp :id id :refers-to (or *addressee*
							   *me*))))
		((W::TODAY ont::today)
		 (multiple-value-bind (sec min hour date month year dow dstp tz)
		     (get-decoded-time)
		   (declare (ignore hour min sec dow dstp tz))
		   (list (make-ref-hyp :id id ;; :lf-type 'ONT::DATE-OBJECT :lf lf
				       :kr-context `((THE ,id :instance-of  DATE :day ,date :month ,month :year ,year))))
		   ))
		((W::HERE W::THERE ont::here ont::there) (find-location-ref id
						       lf (- index 1)))
		(otherwise
		 (when (not (member lf-type '(ONT::TIME-LOC)))  ;; certain exclusions where we don't try for a referent
		   (resolve-pro-fn lf '(concrete) '(individual) index 
				   #'(lambda (x)
				       (or (null sem) (match-with-subtyping sem (referent-sem x))))
				   3)))
		)))
    hyps

    ))

(defun get-lf-type (lf)
  (simplify-generic-type (third lf)))

(defun simplify-generic-type (type)
  "this simplifies the certain types that have the same lexical rep as the LF form, plus one, to allow general reference (e.g., 'the vehicle' may find 'the truck'"
  (if (consp type)
      (second type)
      type))

(defun resolve-definite-reference (lf index speaker addressee)
  (let* ((id (second lf))
	 (name (find-arg-in-act lf :name-of))
	 (lf-type (get-lf-type lf)))   ;;  should really make this into a variable so we can extract a more specific type
    ;;  need to check for special cases like names
    (cond
     ;; proper names
     (name
       (if *external-name-resolution*
	   (resolve-name id lf name)
	   (standard-definite-reference id index lf lf-type speaker addressee)))
     ;;  references to explicit times do not use the discourse history
     ((subtype-check lf-type 'ONT::TIME-LOC)
      (resolve-temporal-reference lf index))
     ((and (subtype-check lf-type 'ONT::LOCATION)
	   (member (find-arg-in-act lf :proform) '(W::THIS ont::this)))
      (resolve-spatial-reference lf index))
     ((member :members lf)
      (resolve-definite-set-reference lf index))
     ((member :sequence lf)
      (resolve-definite-sequence-reference lf index))
     (t   ;; standard definite description
      (standard-definite-reference id index lf lf-type speaker addressee)
      ))
    ))
    
(defun standard-definite-reference (id index lf lf-type speaker addressee)
  (let* ((proform (find-arg-in-act lf :proform))
	 (possibles (find-most-salient lf-type id (if (and proform (member proform '(ont::this ont::that ont::these ont::those)))
						   '(visible-focus concrete event abstract)
						   '(concrete wh-term event abstract))
				       (list (classify-num lf))
				       (- index 1) (if proform 3 200)))
	 (ans (filter-by-fastmatch-subsumes id (mapcar #'referent-id possibles))))
    (if ans
	(mapcar #'(lambda (a) (bind-to-referent lf a)) 
		(mapcar #'get-referential-info ans))
	)))

(defun remove-null-impro (id lf lfs)
  "If we have a functional referent with a null impro (e.g., the author) - then we remove this
      here and try to match on the specific object type (e.g., author as person). The bridging
      reference case needs to be handled elsewhere"
  (let* ((arg (find-arg-in-act lf :of))
	 (newlf (if arg
		    (let ((argtype (third (get-def-from-akrl-context arg lfs))))
		      (if (and argtype (or (eq argtype 'ONT::REFERENTIAL-SEM) (and (consp argtype) (eq (third argtype) 'ONT::REFERENTIAL-SEM))))
			  (cons (car lf) (remove-arg (cdr lf) :of)))))))
    (if newlf (cons newlf (remove-if #'(lambda (x) (eq (second x) id)) lfs))
	lfs)))
			   
(defun resolve-definite-set-reference (lf index)
  (declare (ignore lf index))
  (format t "Reference resolution to sets not reimplemented yet"))

(defun resolve-definite-sequence-reference (lf index)
  (declare (ignore lf index))
  (format t "Reference resolution to sequences not reimplemented yet"))

(defun resolve-temporal-reference (lf index)
  (declare (ignore lf index))
  (format t "Reference resolution to times not implemented yet"))

(defun resolve-spatial-reference (lf index)
  (declare (ignore lf index))
  (format t "Reference resolution to spatial entities not implemented yet")
  )
  


(defun is-lf-set (type)
  (and (consp type) (eq (car type) 'ONT::SET-OF)))
	

(defun resolve-quantifier (lf index)
  (declare (ignore index))
  nil)

(defun classify-quan (q)
  (if (consp q)     ;;extract out lexical content from complex type
      (setq q (third q)))
  (case q
    ((W::EVERYTHING W::EVERY W::ALL W::UNIVERSAL ont::everything ont::every ont::all ont::universal) 'ONT::EVERY)
    ((W::MORE ont::more) 'ONT::MORE)
    ((W::LESS ont::less) 'ONT::LESS)
    (otherwise 'ONT::SOME)))



(defun build-referent-list (lfs KRs refkrs)
  "Given a list of LFs, this build the referent structures in salience order.
   This is called on SYS utts. Eventually we need KR information"
  (let ((referents
	 (remove-if #'null
		    (mapcar #'(lambda (x) (if (recordable-referent x)
					      (build-referent-from-sys-utt x KRs refkrs lfs))) lfs)))
	)
    (sort-referents-by-focus lfs referents)
    ))
    

(defun build-referent-from-sys-utt (lf KRs refinfo lf-context)
  (let* ((id (second lf))
	 (kr-expr (find-if #'(lambda (x) (eq (second x) id)) KRs))
	 (krs (remove-unused-context kr-expr KRs)))
  (make-referent :id id
		 :lf lf
		 :lf-type (third lf)
		 :num (classify-num lf)
		 :accessibility (classify-accessibility lf)
		 :refers-to (or (find-arg-in-act lf :refers-to) (cadr (assoc id refinfo))))))


;;  HANDLING HERE AND THERE

(defun find-location-ref (id lf index)
  (if (guicontrol-active? *gui-control*)
      (look-for-GUI-object id lf index)
    (look-for-mentioned-object id lf index)))

(defun look-for-GUI-object (id lf index)
  (declare (ignore id lf index)))

(defun look-for-mentioned-object (id lf index)
  (declare (ignore id))
  "Looks for most recent ONT::GEO-OBJECT"
   (let ((ans (find-most-salient 'ont::REFERENTIAL-SEM id '(concrete) '(individual)
				 #'(lambda (x)
				     (or (subtype-check (referent-lf-type x) 'ONT::LINGUISTIC-OBJECT)  ;; for GUI display act
					 (and (subtype-check (referent-lf-type x)  'ONT::GEO-OBJECT )
					      (not (subtype-check  (referent-lf-type x) 'ONT::PERSON))))) index 2)))
    (mapcar #'(lambda (a)
		(bind-to-referent lf a)) ans))
  )

(defun bind-to-referent (lf ante)
  "Build a new coreferent structure that refers to ANTE"
  (let ((lf-type (third lf))
	(id (second lf))
	)
    (make-ref-hyp  :id id
		   :lf-type (or (om::more-specific (referent-lf-type ante) lf-type) lf-type (referent-lf-type ante))
		   :refers-to (referent-refers-to ante)
		   :coref (or (referent-coref ante) (referent-id ante)))
    ))

(defun recordable-referent (lf)
  "we only keep things in the system utterances that we think will be useful"
  (case (car lf)
    ((ONT::THE ONT::A ONT::SPEECHACT ONT::BARE) t)
    ((ONT::PRO ONT::IMPRO ONT::WH-TERM)
     (and (Not (member (find-arg-in-act lf :proform) '(W::ME W::I W::YOU W::*YOU* ont::me ont::i ont::you ont::*you*)))
	  (let ((type (third lf)))   ;; this second condition is a safety because GM does always include the :proform
	    (not (and (consp type) (member (third type) '(W::ME W::I W::YOU W::*YOU* ont::me ont::i ont::we ont::*you*)))))))
    ))
       

(defun extract-referent (expr)
  "extracts the referent from a expr of form (lambda (x) (equals x R))"
  (let ((equality (remove-if-not #'(lambda (x) (eq (car x) 'equals))
				 (cddr expr))))
    (if equality
	(third (car equality)))))

(defun find-most-salient (lf-type id access-requirement num index limit)
  "Gathers up potential referents in preference order based on the parameters"
  (when (and (>= index 0) (>= limit 0))
    (if (eq (utt-record-status (get-im-record index)) 'CPSA-failure)
	;; skip over failed utterances
	(find-most-salient lf-type id access-requirement num (- index 1) limit)
	(or (find-possible-referents lf-Type id Access-requirement num index) 
	    (find-most-salient lf-type id access-requirement num (- index 1) (- limit 1))))
    ))

(defun find-possible-referents (lf-type id access-requirement nums index)
  "returns the objects that meet the access requirements"
  (let* ((r (get-im-record index))
	 (focus (utt-record-focus r))
	 (focus-id (if (referent-p focus) (referent-id focus) focus))
	 )
    (let*
	((accessable-refs (remove-if-not #'(lambda (x) (and (referent-p x) 
							    (not (eq (referent-id x) id)) ;; can't refer to itself
							    (member (referent-accessibility x) access-requirement)
							    (member (referent-num x) nums)
							    (subtype-check (referent-lf-type x) lf-type)))
					 (utt-record-referring-expressions r)))
	 )
      accessable-refs)))

(defun find-possible-referents-in-current-sentence (lf-type id access-requirement nums index)
  "returns the objects that meet the access requirement AND that precede the id in the sentence"
  (let* ((r (get-im-record index))
	 (focus (utt-record-focus r))
	 (focus-id (if (referent-p focus) (referent-id focus) focus))
	 )
    (let*
	((accessable-refs (remove-if-not #'(lambda (x) (and (referent-p x) 
							    (not (eq (referent-id x) id)) ;; can't refer to itself
							    (member (referent-accessibility x) access-requirement)
							    (member (referent-num x) nums)
							    (subtype-check (referent-lf-type x) lf-type)))
					 (truncate-expressions-at-id id (utt-record-referring-expressions r)))
	 ))
      accessable-refs)))

(defun truncate-expressions-at-id (id exprs)
  "this gathers up all the possible referents up to the ID"
  (if (and exprs (not (eq (referent-id (car exprs)) id)))
      (cons (car exprs) (truncate-expressions-at-id id (cdr exprs)))))
	  
	  
#||
(defun sort-refs (refs focus-id)
  "Here we try to order the candidates - currently we prefer referents that have non-null refers-to values since
      we know they are known to the application"
  (let ((focus (find-if #'(lambda (x) (eq (referent-id x) focus-id)) refs)))
    (if focus (setq refs (remove-if #'(lambda (x) (eq (referent-id x) focus-id)) refs)))
    (multiple-value-bind
	(kr-refs others)
	(split-list #'referent-refers-to refs)
      (if focus
	  (cons focus (append kr-refs others))
	(append kr-refs others)))))
||#
;;======
;;  Domain Specific Name Handling

(defun resolve-name (id lf name)
  (let* ((answer (dfc::send-and-wait
		  `(REQUEST :content (RESOLVE-NAME :id ,id :name ,(convert-to-string name)))))
	 (lf-type (third lf))
	 (refs (find-arg-in-act answer :referents)))
    (if refs
	(mapcar #'(lambda (x) (build-ref-hyp-for-name id x lf-type)) refs)
      (list (make-ref-hyp :id id :lf-type lf-type)))))

(defun build-ref-hyp-for-name (id hyp lf-type)
  (let* ((kr-context (find-arg-in-act hyp :context))
	 (kr-id (find-arg-in-act hyp :content))
	;; (root-expr (get-def-from-akrl-context kr-id kr-context))
	;; (kr-type (find-arg root-expr :instance-of))
	 )
      (make-ref-hyp :id id
		  :kr-context kr-context
		  :lf-type lf-type
		  :refers-to kr-id)))

(defun convert-to-string (x)
  (if (consp x)
      (if (eql (list-length x) 1)
	  ;; Just one thing;  handle as number or string...
	  (cond ((symbolp
		  (car x)) (symbol-name (car x)))
		((numberp (car x)) (format nil "~S" x)))
	;; Many things
	(apply #'concatenate 'string (mapcar #'atom-name-plus-space x)))
    ))

(defun atom-name-plus-space (x)
  (cond ((symbolp x)
	 (concatenate 'string (symbol-name x) " "))
	((numberp x)
	 (format nil "~S " x))
	))
;;
;;  Support for Generation

(defun get-reference-info-for-id (id)
  (multiple-value-bind
      (most-recent orig)
      (find-most-recent-reference id) 
	  
    ;; now r is the referent record, and i is the index
    (let* ((ref (cadr most-recent))
	   (msg (list 'REFERENCE-INFO
		      :id id
		      :accessibility (referent-accessibility ref)
		      :most-recent
		      (list :utts-back (- *im-utt-count* (car most-recent))
			    :lf (referent-lf ref))
		      :competitors (find-competitors id *im-utt-count* (car most-recent) (cadr most-recent)))))
      (if orig
	  (append msg
		  (list :original
			(list :utts-back (- *im-utt-count* (car orig))
			      :lf (referent-lf (cadr orig))
			     )))
	msg))
	  
    ))

(defun find-most-recent-reference (id)
 (let ((i (+ *im-utt-count* 1))
	(most-recent-ref nil)
	(original-ref nil))
    (loop while (and (> i 0) (not original-ref))
	  do
	  (setq i (- i 1))
	  (let ((rec (find-if #'(lambda (r)
				 (if (and (null most-recent-ref) (eq (referent-refers-to r) id))  ;; record most recent occurance
				     (setq most-recent-ref (list i r)))
				 (eq (referent-id r) id))  ;; the test for the original occurance
			     (utt-record-referring-expressions (get-im-record i)))))
	      (if rec (setq original-ref (list i  rec)))))
    (values (or most-recent-ref original-ref)
	    (if (and most-recent-ref (not (eq original-ref most-recent-ref)))
		original-ref))))

(defun find-competitors (id end start rec)
  (let* ((lf-type (referent-lf-type rec))
	 (lf (referent-lf rec))
	 (sem (referent-sem rec))
	 (ref-hyps (find-most-salient lf '(concrete event abstract)
				#'(lambda (x) (and (subtype-check  (referent-lf-type x) lf-type)
						   (or (not sem) (not (referent-sem x)) (match-with-subtyping sem (referent-sem x)))))
				end
				(- end start)))
	 (remaining-hyps (remove-if #'(lambda (x) (or (eq id (referent-refers-to x))
						      (eq id (referent-id x)))) ref-hyps)))
    (when remaining-hyps
      ;; unfortunately we didn't remember where this was, so we research!
      (mapcar #'(lambda (ref-hyp)
		  (let ((rec
			 (find-most-recent-reference (referent-id ref-hyp))))
		    (list (- *im-utt-count* (car rec)) (referent-id (cadr rec)))))
	      remaining-hyps))))


(defun receive-kb-reply (msg content)
  (reply-to-msg msg 'tell :content content))

