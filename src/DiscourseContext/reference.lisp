;;  Basic reference module fill in
;; we keep the referents from the last utterance, then everything in a simple history list 

(in-package :dc)

;; Functions for manipulating referent structures

(defvar *referents* nil)

(defun init-referents ()
  (setq *referents* nil))

(defun add-to-term-lookup (ref)
  (setq *referents*
	(cons (cons (term-record-id ref) ref)
	      *referents*))
  ref)

(defun get-referential-info (id)
  (cdr (assoc id *referents*)))

(defun build-term-structure (term lfs)
  "builds the basic referent structure from the information in TERM"
  (let ((lf (find-arg-in-act term :lf)))
    (add-to-term-lookup
     (make-term-record
      :id (second lf)
      :lf LF
      :lf-context (remove-unused-context lf lfs)
      :lf-type (simplify-lf-type (third lf) lf)
      :sem (read-sem (find-arg-in-act term :sem))
      :accessibility (classify-accessibility LF)
      )
     )))

(defun read-sem (x)
  (parser:read-expression (expand-or x))
  )

(defun expand-or (x)
  (when x
    (if (consp x)
	(if (eq (car x) 'ONT::<OR>)
	    (list* 'PARSER::? (gen-symbol 'sem) (cdr x))
	  (reuse-cons (expand-or (car x))
		      (expand-or (cdr x))
		      x))
      x)))

(defun classify-accessibility (lf)
  (case (car lf)
    ((ONT::THE ONT::A)
     'concrete)
    ((ONT::PRO ONT::IMPRO)  ;; Pro's are concrete refs except for the first and second person, which are handled spceially
     (if (member (find-arg-in-act lf :context-rel) '(ONT::ME ONT::I ONT::YOU ONT::*YOU*))
	 'pro-1st-2nd
       (case (car lf)
	 (ONT::PRO 'concrete)
	 (ONT::IMPRO 'impro))))
    (ONT::F 'event)
    (ONT::KIND 'kind)
    (ONT::WH-TERM 'wh-term)
    (otherwise 'abstract)))

(defun install-KR-info-in-record (KR index)
  (let ((refs (dc-record-term-list (get-dc-record index))))
    (mapc #'(lambda (x) (install-KR-info-in-referent x KR)) refs)))

(defun install-KR-info-in-referent (ref KR-info)
  (let* ((id (term-record-id ref))
	 (root (find-if #'(lambda (x) (eq (car x) id)) KR-info)))
    (if root
	(setf (term-record-kr-context ref)
	      (list root)))))

(defun install-hyp-in-referent (ref hyp)
  "We have picked a HYP and now update the discourse state to reflect this"
  (if hyp
    (let* ((id (term-record-id ref))
	   (kr-type (or (om::lf-atom-transform (ref-hyp-lf-type hyp))
			(get-role-from-akrl-context :instance-of (or (ref-hyp-refers-to hyp)
								     (ref-hyp-id hyp)) (ref-hyp-kr-context hyp))))
	   (orig-kr (find-if #'(lambda (x) (eq (second x) id)) (term-record-kr-context ref))))
      (setf (term-record-kr-context ref)
	    (cond
	      ((ref-hyp-refers-to hyp)
	      (let ((start (or orig-kr
			       (if kr-type 
				   `(ONT::THE ,id :INSTANCE-OF  ,kr-type)
				   (list 'ONT::THE id)))))
		(list (append start `(:EQUALS ,(ref-hyp-refers-to hyp))))))
     
	      ((ref-hyp-coref hyp)
	       (let ((start (or orig-kr
				(if kr-type 
				    `(ONT::THE ,id :INSTANCE-OF  ,kr-type)
				    (list 'ONT::THE id)))))
		 (list (append start `(:COREF ,(ref-hyp-coref hyp))))))
	      ;;  no referent, but we set the KR-info as a "new" object
	      (t (or (ref-hyp-kr-context hyp)
		     orig-kr
		     `((ONT::THE ,id :instance-of ,kr-type))))))
      (setf (term-record-refers-to ref) (ref-hyp-refers-to hyp))
	
      (if (and (or (ref-hyp-refers-to hyp) ;; if the ID's differ, this was a real anaphoric reference and we make it concrete
		   (ref-hyp-coref hyp))
	       (not (eq (term-record-accessibility ref) 'pro-1st-2nd)))
	  (setf (term-record-accessibility ref) 'concrete))  
      (setf (term-record-coref ref) (ref-hyp-coref hyp))
      (if (ref-hyp-lf-type hyp)
	  (setf (term-record-lf-type ref) (simplify-lf-type (ref-hyp-lf-type hyp) (term-record-lf ref))))
      (if (ref-hyp-sem hyp)
	  (setf (term-record-sem ref) (ref-hyp-sem hyp))))
	
    ;; there was no referent -- if a demonstrative, pro, impro or any other non-semantically specified referent,
    ;;    we prevent it form being accesible as an antecedent
    (progn
      (if (eq (term-record-lf-type ref) 'ONT::REFERENTIAL-SEM)
	  (setf (term-record-accessibility ref) 'FAILED))
      ))
  ref)


(defun simplify-lf-type (lf-type lf)
  "This simplfies the types of PRO forms as they contains lexical information that's not now needed"
  (if (and (eq (car lf) 'ONT::PRO)
	   (consp lf-type))
      (second lf-type)
    lf-type))

(defun find-possible-hyps (ref index speaker addressee)
  "Returns a list REF-HYPs with referential information added"
  (let* ((lf (term-record-lf ref))
	 (sem (term-record-sem ref))
	 (KRinfo (mapcar #'(lambda (x) (cons (second x) x))  ;; build an ASSOC table
			 (term-record-kr-context ref)))
	 (hyps
	  (case (car lf)
	    (ONT::PRO 
	    (resolve-personal-pronoun lf sem KRinfo index speaker addressee))
	   (ONT::IMPRO
	    (resolve-impro lf sem KRinfo index))
	   (ONT::THE
	    (resolve-definite-reference lf sem KRinfo (term-record-lf-context ref) index speaker addressee))
	   (ONT::QUANTIFIER
	    (resolve-quantifier lf KRinfo index))
	   (t   ;; generate null ref entries for other terms
	    (list (generate-referent-from-description lf KRinfo nil)))
	   )))
     hyps
    ))

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
  (let* ((candidates (remove-if-not #'(lambda (x) (and (member (cadr x) possible-ids)
						       (member (car x) '(ONT::PRO ONT::IMPRO ONT::THE))))
				    lfs))
	 (pros (remove-if-not #'(lambda (x) (and (eq (car x) 'ONT::PRO)
						 (third-person-pro x)))
				  candidates))
	 (impros (remove-if-not #'(lambda (x) (eq (car x) 'ONT::IMPRO)) candidates))
	 (defs (remove-if-not #'(lambda (x) (eq (car x) 'ONT::THE)) candidates)))
    (append pros impros defs)))
    
(defun resolve-personal-pronoun (lf sem KRinfo index speaker addressee)
  ;;(format t "~%~%REsolving ~S" lf)
  (let* ((id (second lf))
	 (cr (or (find-arg-in-act lf :context-rel) (if (consp (third lf)) (third (third lf)))))
	 (val
	  (case cr
	    ((ONT::I ONT::ME ONT::MY) (list (make-ref-hyp :id id :refers-to speaker)))
	    ((ONT::YOU ONT::YOUR) (list (make-ref-hyp :id id :refers-to addressee)))
	    ((ONT::WE ONT::OUR) (list
		    (make-ref-hyp :id id ;;:refers-to id
				  :kr-context `((ONT::THE ,id :instance-of SET
							 :equals (,speaker ,addressee))))))
	    ((ONT::IT ONT::ITS)
	     ;; Resolve with SEM if we have it, otherwise use default non-human phys-object
	     (resolve-pro-fn lf index (fn-no-human 'ONT::REFERENTIAL-SEM sem) 3))
	    ((ONT::THIS ONT::THAT)
	     (resolve-this-that lf index (fn-no-human 'ONT::REFERENTIAL-SEM sem)))  ;; tight restriction for GIU pointing
	    
	    ((ONT::THEM ONT::THEIR)
	     (resolve-pro-fn lf index #'(lambda (x kb)
					  (declare (ignore kb))
					  (and (parser::match-with-subtyping '(SET-OF ONT::PHYS-OBJECT) (term-record-lf-type x))
					       (or (not sem) (not (term-record-sem x)) (parser::match-with-subtyping sem (term-record-sem x)))))
			     3))
	    ((ONT::he ONT::him ONT::his ONT::she ONT::her)
	     (resolve-pro-fn lf index #'(lambda (x kb) 
					   (declare (ignore kb))
					   (parser::match-with-subtyping 'ONT::PERSON (term-record-lf-type x)))
			     3))
	    
	    ((ONT::HERE ONT::THERE) (find-location-ref id
						 lf (- index 1)))

	    ((ONT::TONIGHT ONT::TODAY)
	      (find-temporal-ref cr lf))

	    (otherwise
	     (let ((kr (cadr (assoc id KRinfo))))
	       (if kr
		   (list (make-ref-hyp :id id
				       :kr-context (list kr))))))
	    )))
	val)
    )

(defun find-temporal-ref (pro lf)
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec min hour))
    (case pro
      ((ONT::TONIGHT ONT::TODAY)
       (build-temporal-ref lf month day year))
      (ONT::TOMORROW
       (build-temporal-plus-1 lf month day year))
      (ONT::YESTERDAY
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
			  (make-term-record :id (second lf)
					 :implicit t
					 :lf lf
					 :lf-type (simplify-lf-type (third lf) lf)
					 :accessibility (classify-accessibility LF)
					 :kr-context (list `(ONT::THE ,(second lf) :instance-of ONT::|Cal-Time| 
								     :|time-month| ,month :|time-day| ,day :|time-year| ,year
								     :|the-name| ,(list datestring))))
			  ))))

(defun find-month (n)
  (second (assoc n '(
    (1 "jan") (2 "feb") (3 "Mar") (4 "Apr") (5 "may") (6 "jun") (7 "jul") (8 "aug") (9 "sep") (10 "oct") (11 "nov") (12 "dec")))))

(defun fn-no-human (type sem)
   (if (null sem)
       #'(lambda (x kb)
	   (declare (ignore kb))
	   (and (parser::match-with-subtyping type (term-record-lf-type x))
		(it-exclusion (term-record-lf-type x))))
     #'(lambda (x kb)
	  (declare (ignore kb))
	 (and (if (term-record-sem x)
		(parser::match-with-subtyping sem (term-record-sem x))
	      (parser::match-with-subtyping type (term-record-lf-type x)))
	    (it-exclusion (term-record-lf-type x))))))

(defun it-exclusion (type)
  (not (parser::match-with-subtyping 'ONT::PERSON type)))

(defun third-person-pro (pro)
  (member (find-arg pro :context-rel)
	  '(ONT::IT ONT::THEM ONT::HE ONT::HIM ONT::SHE ONT::HER)))

    
(defun resolve-pro-fn (lf index fn count)
  "Personal pronouns should refer to a concrete object matching the criteria in the recent history"
  (let ((ans (find-most-salient lf '(concrete) fn (- index 1) count)))
    (mapcar #'(lambda (a) (bind-to-referent lf a)) ans)))

(defun resolve-this-that (lf index fn)
  "Currently we only handle this/that to recently focussed objects pointed out in GUI"
  (let ((ans (find-most-salient lf '(visible-focus) fn (- index 1) 4)))
    (mapcar #'(lambda (a) (bind-to-referent lf a)) ans)))

(defun resolve-impro (lf sem KRinfo index)
  (declare (IGNORE KRINFO))
  (let* ((id (second lf))
	 (cr (find-arg-in-act lf :context-rel))
	(hyps (case cr
		(ONT::*YOU* (list (make-ref-hyp :id id :refers-to (DC-record-addressee (get-dc-record index)))))
								   
		(ONT::TODAY
		 (multiple-value-bind (sec min hour date month year dow dstp tz)
		     (get-decoded-time)
		   (declare (ignore hour min sec dow dstp tz))
		   (list (make-ref-hyp :id id ;; :lf-type 'ONT::DATE-OBJECT :lf lf
				       :kr-context `((THE ,id :instance-of  DATE :day ,date :month ,month :year ,year))))
		   ))
		((ONT::HERE ONT::THERE) (find-location-ref id
						       lf (- index 1)))
		(otherwise
		 (if (null sem) (setq sem (parser::make-var :name 'sem)))
		 (resolve-pro-fn lf index #'(lambda (x kb)
					      (declare (ignore kb))
					      (parser::match-with-subtyping (or (third lf) sem) (term-record-sem x))) 30))
		)))
    hyps
    ))

(defun simplify-generic-type (type)
  "this simplifies the certain types that have the same lexical rep as the LF form, plus one, to allow general reference (e.g., 'the vehicle' may find 'the truck'"
  (if (consp type)
      (cond 
	((equal type '(:* ONT::REFERENTIAL-SEM W::ONE))
	 'ONT::REFERENTIAL-SEM)
	((and (symbolp (second type)) (symbolp (third type)) (equal (symbol-name (second type)) (symbol-name (third type))))
	 (second type))
	(t type))
      type))

(defun resolve-definite-reference (lf sem KRS lf-context index speaker addressee)
  (let* ((id (second lf))
	 (kr (cadr (assoc id KRS)));;(build-kr-context-for-id id KRS))
	 (name (find-arg-in-act lf :name-of))
	 (lf-type (simplify-generic-type (third lf))))   ;;  should really make this into a variable so we can extract a more specific type
    ;;  need to check for special cases like names
    (cond
     ;; proper names
     (name
      (resolve-name id lf krs name))
     ;;  references to explicit times do not use the discourse history
     ((parser::match-with-subtyping lf-type 'ONT::TIME-LOC)
      (list
       (generate-referent-from-description lf krs nil)))
     ((and (parser::match-with-subtyping lf-type 'ONT::LOCATION)
	   (eq (find-arg-in-act lf :context-rel) 'ONT::THIS))
      (list (generate-referent-from-description lf krs nil)))
     ((member :members lf)
      (resolve-definite-set-reference lf sem KRS lf-context index))
     ((member :sequence lf)
      (resolve-definite-sequence-reference lf sem KRS lf-context index))
     (t   ;; standard definite description
      (standard-definite-reference id index lf lf-type lf-context sem kr speaker addressee)
      ;;(and (member :of lf)   ;;  try for a bridging reference
      ;;	(resolve-definite-bridging-reference lf sem KRS lf-context index)))

       ))
    ))
    
(defun standard-definite-reference (id index lf lf-type lf-context sem kr speaker addressee)
  (multiple-value-bind 
	(query-expr mappings)
      (build-ref-query (remove-unused-context-with-root  id (remove-null-impro id lf lf-context)) speaker addressee)
    (let* ((context-rel (find-arg-in-act lf :context-rel))
	   (query-var (cadr (assoc id mappings)))
	   (ans (find-most-salient lf (if (and context-rel (eq context-rel 'ONT::THIS))
					  '(visible-focus concrete event abstract)
					  '(concrete event abstract))
				   #'(lambda (x kb) (and (not (eq (term-record-lf-type x) 'ONT::REFERENTIAL-SEM))  ;; can't refer back to a pronoun
							 (parser::match-with-subtyping (term-record-lf-type x) lf-type)  ;; the basic type must match
							 (or (not sem) (not (term-record-sem x)) (parser::match-with-subtyping sem (term-record-sem x)))  ;; sem features must match
							 (match-lf-lists (subst-mappings query-expr (list (list query-var (term-record-id x)))) kb)
							     
							 ))
									    
				   (- index 1)
				   (if context-rel 3 200))  ;; if its indexical, it should be close by
	     ))
      (if ans
	  (mapcar #'(lambda (a) (bind-to-referent lf a)) ans)
	  ;; if no referent, set the KR if it exists
	  (if kr
	      (list (make-ref-hyp :id id
				  :kr-context (list kr)))
	      )))
    ))

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
			   
(defun resolve-definite-set-reference (lf sem KRS lf-context index)
  (declare (ignore lf sem KRS lf-context index))
  (format t "Reference resolution to sets not implemented yet"))

(defun resolve-definite-sequence-reference (lf sem KRS lf-context index)
  (declare (ignore lf sem KRS lf-context index))
  (format t "Reference resolution to sequences not implemented yet"))

(defun is-lf-set (type)
  (and (consp type) (eq (car type) 'ONT::SET-OF)))
	

(defun resolve-quantifier (lf Krs index)
  (declare (ignore index))
  (let* ((q (find-arg-in-act lf :quan))
	(id (second lf))
	(kr (cadr (assoc id KRS)))
	(set (find-arg-in-act lf :subset)))
	;;(mods (find-arg-in-act lf :mods)))
   (list
    (make-ref-hyp
     :id id
     :kr-context (list
		  (if KR (append (cons 'ONT::QUANT (cdr kr)) ;; there was a KR mapping
				 (list :quan (classify-quan q)))
		    (list 'ONT::QUANT id)))
     :coref set
     
     )
    )))

(defun classify-quan (q)
  (if (consp q)     ;;extract out lexical content from complex type
      (setq q (third q)))
  (case q
    ((ONT::EVERYTHING ONT::EVERY ONT::ALL ONT::UNIVERSAL) 'ONT::EVERY)
    (ONT::MORE 'ONT::MORE)
    (ONT::LESS 'ONT::LESS)
    (otherwise 'ONT::SOME)))

(defun generate-referent-from-description (lf KRS refs)
  "Builds a REFERENT structure from an LF and possible KR information.
    We don't expect a referent, but use this to store the KR translation"
  (let* ((id (second lf))
	 (kr (cadr (assoc id KRS)))
	 (krref (cadr (assoc id refs))))

    (when (or kr krref)
      (make-ref-hyp
       :id id
       :kr-context (list kr)
       :refers-to krref
      )
    )
  ))


(defun build-term-list (lfs KRs refkrs)
  "Given a list of LFs, this build the referent structures in salience order.
   This is called on SYS utts. Eventually we'' need KR information"
  (let ((referents
	 (remove-if #'null
		    (mapcar #'(lambda (x) (if (recordable-referent x)
					      (build-term-from-sys-utt x KRs refkrs))) lfs)))
	)
    (multiple-value-bind (pros others)
	(split-list #'(lambda (r)
			(eq (car (term-record-lf r)) 'ONT::PRO))
		    referents)
      ;; order pronouns as most salience
      (append pros others))))

(defun build-term-from-sys-utt (lf KRs refinfo)
  (let* ((id (second lf))
	 (kr-expr (find-if #'(lambda (x) (eq (second x) id)) KRs))
	 (krs (remove-unused-context kr-expr KRs)))
  (make-term-record :id id
		 :lf lf
		 :lf-type (third lf)
		 :kr-context krs
		 :accessibility (classify-accessibility lf)
		 :refers-to (or (find-arg-in-act lf :refers-to) (cadr (assoc id refinfo))))))


;;  HANDLING HERE AND THERE

(defun find-location-ref (id lf index)
  (look-for-mentioned-object id lf index))

(defun look-for-mentioned-object (id lf index)
  (declare (ignore id))
  "Looks for most recent ONT::GEO-OBJECT"
   (let ((ans (find-most-salient lf '(concrete) #'(lambda (x)
				       (or (parser::match-with-subtyping 'ONT::LINGUISTIC-OBJECT (term-record-lf-type x))  ;; for GUI display act
					   (and (parser::match-with-subtyping 'ONT::GEO-OBJECT (term-record-lf-type x))
						(not (parser::match-with-subtyping 'ONT::PERSON (term-record-lf-type x)))))) index 2)))
    (mapcar #'(lambda (a)
		(bind-to-referent lf a)) ans))
  )

(defun bind-to-referent (lf ante)
  "Build a new referent structure that refers to ANTE"
  (let ((lf-type (third lf))
	(id (second lf))
	(first-mention (term-record-implicit ante))   ;; check if antecedent has been explicitly used or not
	)
    (make-ref-hyp  :id id
		   :lf-type (or (om::more-specific (term-record-lf-type ante) lf-type) lf-type (term-record-lf-type ante))
		   :sem (term-record-sem ante)
		   :kr-context (if first-mention
				   ;; this is the first use, change the variable name in the KR context
				   (subst id (term-record-id ante) (term-record-kr-context ante))
				 (term-record-kr-context ante))
		   :refers-to (if (not first-mention) (or (term-record-refers-to ante) (term-record-id ante)))
		   :coref (if (not first-mention) (or (term-record-coref ante)
						      (term-record-id ante)))
    )))

(defun recordable-referent (lf)
  (case (car lf)
    ((ONT::THE ONT::A) t)
    ((ONT::PRO ONT::IMPRO ONT::WH-TERM)
     (and (Not (member (find-arg-in-act lf :context-rel) '(ONT::ME ONT::I ONT::YOU ONT::*YOU*)))
	  (let ((type (third lf)))   ;; this second condition is a safety because GM does always include the :context-rel
	    (not (and (consp type) (member (third type) '(W::ME W::I W::YOU W::*YOU*)))))))
    ))
       

(defun extract-referent (expr)
  "extracts the referent from a expr of form (lambda (x) (equals x R))"
  (let ((equality (remove-if-not #'(lambda (x) (eq (car x) 'equals))
				 (cddr expr))))
    (if equality
	(third (car equality)))))

;; Finding referents

 (defun find-most-salient (lf access-requirement fn index limit)
  "Searches back until a salient referent (with property FN) is found. LF is only used for tracing purposes"
  (when (and (>= index 0) (>= limit 0))
    (or (find-matching-referent access-requirement fn index) 
	(find-most-salient lf access-requirement fn (- index 1) (- limit 1))))
  )

(defun find-matching-referent (access-requirement fn index)
  "Checks if any objects in REFERENCE list with property FN"
  (let* ((r (get-dc-record index))
	 (focus (dc-record-focus r))
	 (focus-id (if focus (term-record-id focus)))
	 (kb (dc-record-refKB r)))
    ;;(format t "~% KB = ~%~S record = ~S~%" kb r)
    (when (member (dc-record-status r) '(im::success T im::pre-reference))
      (let*
	  ((accessable-refs (remove-if-not #'(lambda (x) (member (term-record-accessibility x) access-requirement))
							 (dc-record-term-list r)))
	   (potential-refs (remove-if-not #'(lambda (x) (apply fn (list x kb))) accessable-refs))
	   (refs (sort-refs potential-refs focus-id)))
    ;; return the most salient information
	refs))
    ))

(defun sort-refs (refs focus-id)
  "Here we try to order the candidates - currently we prefer referents that have non-null refers-to values since
      we know they are known to the application"
  (let ((focus (find-if #'(lambda (x) (eq (term-record-id x) focus-id)) refs)))
    (if focus (setq refs (remove-if #'(lambda (x) (eq (term-record-id x) focus-id)) refs)))
    (multiple-value-bind
	(kr-refs others)
	(split-list #'term-record-refers-to refs)
      (if focus
	  (cons focus (append kr-refs others))
	(append kr-refs others)))))

;;======
;;  Domain Specific Name Handling

(defun resolve-name (id lf krs name)
  (let* ((answer (dfc::send-and-wait
		  `(REQUEST :content (RESOLVE-NAME :id ,id :context ,krs :name ,(convert-to-string name)))))
	 (lf-type (third lf))
	 (refs (find-arg-in-act answer :referents)))
    (if refs
	(mapcar #'(lambda (x) (build-ref-hyp-for-name id x lf-type)) refs)
      (list (make-ref-hyp :id id :lf-type lf-type :kr-context krs)))))

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
		      :accessibility (term-record-accessibility ref)
		      :most-recent
		      (list :utts-back (- *dc-utt-count* (car most-recent))
			    :lf (term-record-lf ref)
			    :kr (term-record-kr-context ref))
		      :competitors (find-competitors id *dc-utt-count* (car most-recent) (cadr most-recent)))))
      (if orig
	  (append msg
		  (list :original
			(list :utts-back (- *dc-utt-count* (car orig))
			      :lf (term-record-lf (cadr orig))
			      :kr (term-record-kr-context (cadr orig)))))
	msg))
	  
    ))

(defun find-most-recent-reference (id)
 (let ((i (+ *dc-utt-count* 1))
	(most-recent-ref nil)
	(original-ref nil))
    (loop while (and (> i 0) (not original-ref))
	  do
	  (setq i (- i 1))
	  (let ((rec (find-if #'(lambda (r)
				 (if (and (null most-recent-ref) (eq (term-record-refers-to r) id))  ;; record most recent occurance
				     (setq most-recent-ref (list i r)))
				 (eq (term-record-id r) id))  ;; the test for the original occurance
			     (dc-record-term-list (get-dc-record i)))))
	      (if rec (setq original-ref (list i  rec)))))
    (values (or most-recent-ref original-ref)
	    (if (and most-recent-ref (not (eq original-ref most-recent-ref)))
		original-ref))))

(defun find-competitors (id end start rec)
  (let* ((lf-type (term-record-lf-type rec))
	 (lf (term-record-lf rec))
	 (sem (term-record-sem rec))
	 (ref-hyps (find-most-salient lf '(concrete event abstract)
				#'(lambda (x) (and (parser::match-with-subtyping lf-type (term-record-lf-type x))
						   (or (not sem) (not (term-record-sem x)) (parser::match-with-subtyping sem (term-record-sem x)))))
				end
				(- end start)))
	 (remaining-hyps (remove-if #'(lambda (x) (or (eq id (term-record-refers-to x))
						      (eq id (term-record-id x)))) ref-hyps)))
    (when remaining-hyps
      ;; unfortunately we didn't remember where this was, so we research!
      (mapcar #'(lambda (ref-hyp)
		  (let ((rec
			 (find-most-recent-reference (term-record-id ref-hyp))))
		    (list (- *dc-utt-count* (car rec)) (term-record-id (cadr rec)))))
	      remaining-hyps))))

(defvar *defs-found* nil)

(defun build-kr-context-for-id (id KRS)
  (let ((root-def (cadr (assoc id KRS))))
    (when root-def
      (setq *defs-found* (list id))
      (cons root-def (find-required-krs root-def)))))

(defun find-required-krs (kr)
  "This traverse an AKRL form and finds the definitions of all the objects referenced"
  (let ((roles (remove-arg (cddr kr) :instance-of)))
    (mapcan #'(lambda (x)
		(track-down-defs (cadr x)))
	    roles)))

(defun track-down-defs (id)
  (when (not (member id *defs-found*))
    (let* ((rec (get-referential-info id))
	  (kr (if rec (term-record-kr-context rec))))
      (list (list* kr (find-required-krs kr))))))
  
