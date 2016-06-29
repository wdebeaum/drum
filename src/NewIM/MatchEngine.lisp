(in-package "IM")

;;  General LF graph Matching facility to perform extraction and/or inference

;;  Rules consist of
;;      Input  - LF terms that match the input
;;      Context Patterns - which match the input but do not "consume" it. There are three
;;         types of contextual patterns
;;              (- X) ---  pattern X  must NOT match the input
;;              CALL procedure calls to arbitrary lisp functions, which return a binding list if they succeed
;;     Output - the new transformed expressions that correspond to the content in the input


(defvar *im-rule-table* nil)  ;; the lookup table for rules in their original format
(defvar *match-trees* nil)    ;; the lookup table for rules in their match-tree form
(defvar *rule-groups* nil)    ;; the lookup table for groups, group-id -> ruleid*
(defvar *initial-match-trees* nil)   ;; the set of rule trees at the start of matching
(defvar *active-rules* nil)   ;; the current set of rule trees during the match 
(defvar *pending-evals* nil)  ;; varibale for storing evals found in first pass, to actually eval in second pass
(defvar *max-cover-with-strict-disjoint-extractions* t) ;; max covers with strict disjoint extractions
(defvar *match-max-two-mods* t)   ;; in pepare-LF, we generate one order of MODs (complete only for two or less mods)
(defvar *extractor-oblig-features* nil) ;; don't put :start and :end in this as they are done separately
(defvar *ruleweights* nil)
(defvar *eliminate-subevents* nil) ;; set to t if we should suppress events that occur in other events.

(defun reset-im-rules (&optional flag)
  (setq *im-rule-table* nil)
  (if (and flag *rule-groups*)
      ;; selective reset
      (setf (gethash flag *rule-groups*) nil)
      ;; global reset
      (setq *rule-groups* (make-hash-table)))
  ;;(setq *ruleweights* nil)  can't do this it we are loading multiple files!!!
  (setq *initial-match-trees* nil)
  (setq *active-rules* nil)
  (if (not flag) (setq *match-trees* nil))
  ;;(setq *discourse-stack* (list *root-state*))
  )

;; access rules
(defun find-rule (id)
  (cdr (assoc id *im-rule-table*)))

(defun find-match-nodes (id)
  (cdr (assoc id *match-trees*)))

(defun find-group (id)
  (gethash id *rule-groups*))

(defun add-rule (rule-id rule group-id)
  (let ((group (gethash group-id *rule-groups*)))
    (setq *im-rule-table* (cons (cons rule-id rule) *im-rule-table*))
    (setq *match-trees*
	  (cons (cons rule-id (gen-mnode rule)) *match-trees*))
    (setf (gethash group-id *rule-groups*)
	  (append group (list rule-id)))) ;; Not using CONS since we want to preserve order of definition in file
  )


(defun gen-mnode (rule)
  (let ((*Seen-already* nil))
    (make-mnode  :pattern (mapcar #'(lambda (pat)
				      (make-pattern-info :pattern pat :binding-lists nil :optional (compute-optional-pattern pat)))
				  (im-rule-pattern rule))
		 :output (im-rule-output rule)
		 :rule-id (im-rule-id rule)
		 :vars (gather-vars (im-rule-pattern rule))
		 :test (im-rule-call rule)
		 )))

(defun compute-optional-pattern (pat)
  "If the pattern only contains free variables, we generate a binding list where all bind to -
    which is the result used if the LF term doesn't exist in the LF being matched"
  (let ((res (every #'free-if-var pat)))
    (if res (trace-msg 2 "~% WARNING: ~S: ALL VARS are FREE ~S:" pat res))
    (if res
	(gen-bind-to-none-list pat)
    )))

(defun free-if-var (x)
  (if (var-p x)
      (not (var-non-empty x))
      (if (consp x)
	  (every #'free-if-var x)
	  T)
     ))

(defun gen-bind-to-none-list (pat)
  (mapcar #'(lambda (x) (list x '-))
	  (remove-if-not #'var-p (flatten pat))))

(defvar *seen-already* nil)

(defun gather-vars (expr)
  (cond ((consp expr)
	 (flatten (remove-if #'null
		    (mapcar #'gather-vars expr))))
	((var-p  expr) 
	 (when (not (member expr *seen-already*))
	     (push expr *seen-already*)
	     expr))
	))
      

;; gathering all the rules in a list of groups together. Also accepts a single group name.

(defun assemble-rules (group-ids)
 (when group-ids
   (if (symbolp group-ids)
       (find-rules (find-group group-ids))
    (let ((grp (find-group (car group-ids))))
      (append (find-rules grp)
	      (assemble-rules (cdr group-ids)))))))

(defun find-rules (rs)
  "given a list of rule names, return a new (later ops will be destructive) MNODE structure for each"
  (when rs
    (let ((node (find-match-nodes (car rs))))
      (if node
	  (cons (if *allow-optional-lfs*    
		    (insert-optional-result (copy-mnode+ node))
		    (copy-mnode+ node))
		(find-rules (cdr rs)))
	  (find-rules (cdr rs))))))


(defun insert-optional-result (mnode)
  "if we allow optional LF forms -- we add a possible binding list where all variables go to -"
  (mapc #'(lambda (pat)
	    (if (pattern-info-optional pat)
		(setf (pattern-info-binding-lists pat)
		      (list (make-binding-result :bndgs (pattern-info-optional pat))))))
	(mnode-pattern mnode))
  mnode)

;;==========================================================================   
;;     I N P U T   
;; reading a rule and adding to the rule database

(defun add-im-rule (r group-id)
  (mapcar #'(lambda (rule)
	      (if (im-rule-p rule)
		  (let ((id (im-rule-id rule)))
		    (if id
			(progn
			  (when (assoc id *im-rule-table*)
			    (setf (im-rule-id rule) (gentemp "X"))
			    (im-warn "duplicate id found. Rule ~S renamed to ~S" id (im-rule-id rule)))
			  (add-rule (im-rule-id rule) rule group-id))
			(im-warn "No ID in rule ~S. It is ignored" r)))))
	  (read-LF-rule r)))

(defun read-LF-rule (r)
  "Example rule:
      ((SPEECHACT ?x SA_TELL :CONTENT ?c)     ;; input pattern 
       (F ?c ONT::NECESSITY :THEME ?a)          ;; input pattern
              --->  2    ;; prob is optional
      (A *1 PROPOSE :THEME ?a))              ;;  output"
  
  (if (verify-rule-format r)
      (mapcar #'(lambda (rule)
		  (let ((im-rule (make-im-rule)))
		    (init-var-table)  ;; clear VAR table for reading the rule
		    (read-next-term rule im-rule)
		    im-rule))
	      (explode-rule r))
    (im-warn "Bad rule detected: ~S" r)))

;;  The following code expands out the MODS, ASSOC-WITH, or SEQUENCE feature if it contains multiple
;;   values, giving each a unique name - MOD, MOD1, etc and generating multiple rules to
;;   account for any possible ordering in the LF

(defvar *expanded-rules* nil)
(defvar *expanded-lfs* nil)

(defun explode-rule (r)
  (setq *expanded-rules* nil)
  (explode-next-part nil r)
  *expanded-rules*)

(defun explode-next-part (prev r)
  (if r
      (if (symbolp (car r))
	  ;; once we hit the rule name, we are done expanding
	  (push (append prev (rename r)) *expanded-rules*)
	  ;; else its an LF
	  (mapcar #'(lambda (p)
		      (explode-next-part (append prev (list p)) (cdr r)))
		  (explode-LF (car r))))
      ;; really shouldn't ever get here - it means there was no rule id!
      (push prev *expanded-rules*)))

(defun rename (r)
  "This gets the rest of a rule starting at the ID. If there is a previous version of this rule,
      it renumbers it"
  (if *expanded-rules*
      (cons (add-alt (car r)) (cdr r))
      r))

(defun add-alt (name)
  (intern (concatenate 'string (symbol-name name) "ALT>")))
  
(defun explode-LF (r)
  "explodes rules out if they contain features with multiple values (e.g., has 2 MODS)"
  (setq *expanded-lfs* nil)
  (explode-roles (list (car r) (cadr r) (caddr r))   ;; specifier var type
		(cdddr r))
  *expanded-lfs*)

(defun explode-roles (prev roles)
  (if roles
      (case (car roles)
	(:mods
	 (explode-sequence-role roles prev :mods :mod :mod1))
	;;(:sequence 
	;;  (explode-sequence-role roles prev :sequence :sequence-0 :sequence-1))
	(:assoc-withs 
	 (explode-sequence-role roles prev :assocwiths :assoc-with :assoc-with1))
	(otherwise 
	 (explode-roles (append prev (list (car roles) (cadr roles))) (cddr roles))))
      ;; we're done, store the result
      (push prev *expanded-lfs*)
      ))
	      
(defun explode-sequence-role (roles prev sequencerolename rolename1 rolename2)
  (case (list-length  (cadr roles))
    ;; length one -- the pattern only has a single MOD, but this might occur in either
    ;;   the :mod or :mod1 position in the input
    (1 (explode-roles (append prev (list rolename1 (caadr roles))) (cddr roles))
       (explode-roles (append prev (list rolename2 (caadr roles))) (cddr roles)))
    (2 (explode-roles (append prev (list rolename1 (caadr roles) rolename2 (cadadr roles)))
		      (cddr roles))
       (explode-roles (append prev (list rolename1 (cadadr roles) rolename2 (caadr roles)))
		      (cddr roles)))
    (otherwise (im-warn "Only support two ~S at most ~S" sequencerolename roles))
  ))

(defun  read-next-term (r im-rule)
  "Reads one term and recurses to read other terms. This assumes we're reading the INPUT side.
   NB: Does not initialize variable table"
  (when r
    (let ((first (car r)))
      (cond
       ((symbolp first)
	(setf (im-rule-id im-rule) first)
	(if (numberp (cadr r)) 
	    (progn  ;; we have an optional rule weight
	      (push (list first (cadr r)) *ruleweights*)
	      (read-output-terms (cddr r) im-rule))
	    (read-output-terms (cdr r) im-rule)))
       ((consp first)
	(case (car first)
	  (- (setf (im-rule-exclude im-rule)
		   (append (im-read-value (second first)) (im-rule-exclude im-rule))))
	  (ONT::EVAL (setf (im-rule-call im-rule)
			   (append (im-rule-call im-rule) (list (im-read-value (second first))))))
	  (otherwise (setf (im-rule-pattern im-rule)
			   (append (im-rule-pattern im-rule) (list (im-read-value first)))))
	  )
	(read-next-term (cdr r) im-rule))
       (t (im-warn "Bad term ~s in rule" first))))))

(defun read-output-terms (terms im-rule)
  (setf (im-rule-output im-rule)
	(append (mapcar #'(lambda (x) (if (consp x)
					  (im-read-value x)
					(im-warn "Illegal term ~S in rule" x)))
			terms)
		(im-rule-output im-rule))))


(defun im-read-value (x)
  "This invokes the parser reader - but we need to convert special symbols first"
  (read-value x nil))

(defun read-LFs (lfs)
  (init-var-table)
  (mapcan #'read-LF-term lfs)
  )

(defun read-lf-term (lf)
  (list (im-read-value lf)))

(defun verify-rule-format (r)
  (every #'verify-term r))

(defun verify-term (term)
  (cond ((symbolp term) t)
	((numberp term) t)
	((consp term)
	 (or (verify-args (cdddr term)) (verify-args (cddr term))))
	))

(defun verify-args (args)
  (if (null args)
      T
    (and (keywordp (car args)) (verify-args (cddr args)))))

;; Handling the input side, readying an LF GRAPH for matching
;;    Rather than having to explode the MODS rule to all combinations when its in a pattern,
;;    We just need to generate one ordering at matching time (as the right pattern will be there!)
;;      Note: Currently this is only true for two modifiers!!


(defun prepare-lf (lf)
  "this looks for roles that have multiple occurances and gives them all unique names (e.g., MOD ... MOD..) 
    becomes MOD ... MOD1..)"
  (list (list* (car lf) (cadr lf) (caddr lf) (renumber-roles (cdddr lf) nil))))

(defun renumber-roles (roles seen)
  (when roles
    (if (member (car roles) seen)
	(let ((new-role (add-number (car roles))))
	  (list* new-role (cadr roles) (renumber-roles (subst new-role (car roles) (cddr roles))
						       (cons new-role seen))))
	(list* (car roles) (cadr roles) (renumber-roles (cddr roles) (cons (car roles) seen)))
	)))
  
(defun add-number (atom)
  (case atom
    (:mod :mod1)
    (:mod1 :mod2)
    (:member :member1)
    (:member1 :member2)
    (:assoc-with :assoc-with1)
    (:assoc-with1 :assoc-with2)))

;;===============================================================================
;;
;;    M A T C H I N G
;;

(defvar *completed-rules* nil)
(defvar *mnode-record* nil)  ;; used for debugging - can inspect what matched in a rule

(defun compute-entailments (lfs rules)
  (let ((extractions  (add-oblig-features (interpret-lf lfs rules) lfs)))
    (if *eliminate-subevents*
	(eliminate-subevents extractions)
	extractions)))

(defun interpret-LF (LFs group-ids)
  "This interprets the LF of a speech act. LFS is alist of LF terms, and returns a list of
     entailments using a best set cover heuristic"
  (trace-msg 4 "~%Interpreting: ~S" LFs)
  (let* ((candidates (generate-rule-matches (mapcan #'prepare-LF LFs) group-ids)))
    (let* ((filtered-candidates (reverse candidates)) ;; put them in the order they are in the file
	   (max-covers
	    (find-max-covers filtered-candidates nil)))
      (trace-msg 1 "Rules selected: ~S" (mapcar #'match-result-rule-id max-covers))
      max-covers)
    ))

(defun eliminate-subevents (events)
  "checks to see if one event is a subpart (e.g., a purpose) of another"
  (when events
    (if (included-in (second (caar events)) (cdr events))
	(eliminate-subevents (cdr events))
	(reuse-cons (car events) (eliminate-subevents (cdr events)) events))))

(defun included-in (x y)
  "t is x appears somewher ein y"
  (when y
    (cond ((atom y) (eq x y))
	  ((consp y)
	   (or (included-in x (car y)) (included-in x (cdr y))))
	  )))

(defun add-oblig-features (results lfs)
  (mapcar #'(lambda (result) 
	      (mapcar #'(lambda (output)
			  (add-oblig-feature output result lfs))
		      (match-result-output result)))
	  results))

(defun add-oblig-feature (output result lfs)
  (let ((feats-to-add (remove-features-already-present output *extractor-oblig-features*)))
    (multiple-value-bind (start end)
	(find-start-end (match-result-cover result) lfs 10000 0)
    (append output
	    (extract-oblig-features (cdr (find-lf (cadr output) lfs)) feats-to-add)
	    (list :start start :end end))
    )))

(defun extract-oblig-features (lf features)
  (when lf
    (if (member (car lf) features)
	(list* (car lf) (cadr lf) (extract-oblig-features (cddr lf) features))
	(extract-oblig-features (cddr lf) features))))

(defun remove-features-already-present (event features)
  (remove-if #'(lambda (x) (find-arg event x)) features))
    
(defun add-start-and-end-times (result lf)
  (multiple-value-bind (start end)
      (find-start-end (match-result-cover result) lf 10000 0)
    (append (car (match-result-output result)) (if (> end start)
						   (list :start start :end end)
						   (list :start NIL :end NIL)))))
  
;; This code is used in Salle

(defun add-start-and-end-for-result (cover lfs)
  (let* ((lfs (mapcar #'(lambda (x) (nth (- x 1) lfs)) cover))
	 (minmaxs (remove-if #'null
			     (mapcar #'(lambda (x) 
					 (multiple-value-bind (start end)
					     (get-start-end-for-lf (second x))
					   (if (and start end)
					       (list start end)))) lfs))))
    (multiple-value-bind (min max)
	(find-min-max-position minmaxs)
      (list :start min :end max))))
    
(defun find-min-max-position (values)
  " given set of pairs of form (start end) - return (min-start max-end)"
  (if (cdr values)
      (multiple-value-bind (min max)
	  (find-min-max-position (cdr values))
	(values (min (caar values) min) (max (cadar values) max)))
      (values (caar values) (cadar values))))

(defun find-start-end (ids lfs min max)
  (if ids
      (let* ((lf (nth (- (car ids) 1) lfs))
	     (start (find-arg-in-act lf :start))
	     (end (find-arg-in-act lf :end)))
	(find-start-end (cdr ids) lfs (if start (min min start)
					  min)
			(if end (max max end) max)))
      (values min max)))

(defun generate-rule-matches (LFs group-ids)
  "this uses a two pass algorithm: the first pass finds matches between individual LF terms.
    The second sees if there's a consistent set of bindings that allows all terms to match"
  (let ((*completed-rules* nil)
	(mnodes (possibly-filter-for-debugging (assemble-rules group-ids))))
    (trace-msg 3 "Active rule groups are ~S:" group-ids)
    (if (null mnodes)
	(trace-msg 2 "No rules were found in specified groups"))
    ;; first pass
    (setq *pending-evals* nil)
    (match-lfs-to-rules LFS 1 mnodes)
    (setq *mnode-record* mnodes)  ;; record for debugging purposes
    (let ((remaining-mnodes (find-possible-completed-rules mnodes)))
      (if remaining-mnodes
	  (progn
	    (trace-msg 3 "Rules that may match: ~S" (mapcar #'(lambda (x) (mnode-rule-id x)) remaining-mnodes))
	    (let ((extractions (construct-rule-matches remaining-mnodes lfs)))
	      (trace-msg 4 "Extractions before filtering: ~S" extractions)
	      extractions
	    ))
	  (trace-msg 1 "No rules matched completely.")
	  ))))

(defvar *rule-matches* nil)

(defun construct-rule-matches (mnodes lfs)
  (setq *rule-matches* nil)
  (mapcar #'(lambda (mn) (construct-rule-match mn lfs)) mnodes)
  *rule-matches*)

(defvar *complete-bndgs* nil)

(defstruct match-result
  output
  rule-id
  bndgs
  cover
  patterns
  )
 
(defun construct-rule-match (mnode lfs)
  "checks for possible consistent assignment of variables across the patterns to create a matched instance"
  (let ((*complete-bndgs* nil))
    (find-bndg-set (mnode-pattern mnode) (mnode-test mnode) nil nil lfs)
    (mapcar #'(lambda (res)
		(trace-msg 3 "Found a complete match with rule ~S, covering ~S" (mnode-rule-id mnode)  (cadr res))
		(trace-msg 4 " with bindings ~S" (car res))
		(let ((reduced-res (remove-if #'null (cadr res))))
		  (if reduced-res
		      (push (make-match-result :output (substitute-stars (subst-in (mnode-output mnode) (car res))) :rule-id (mnode-rule-id mnode)
							      :bndgs (car res) :cover reduced-res :patterns (construct-patterns mnode (car res)))
			    *rule-matches*)
		      (trace-msg 1 "~% Rule ~S eliminated as it matched no input" (mnode-rule-id mnode)))))
	    *complete-bndgs*)
    (values)))
  
(defun construct-patterns (mnode bndgs)
  "instantiates the patterns with the successful bindings for use later"
  (remove-if #'(lambda (y)
		 (or (null (second y)) (eq (second y) '-)))
	     (mapcar #'(lambda (x)
			 (subst-in (pattern-info-pattern x) bndgs))
		     (mnode-pattern mnode))))

(defun find-bndg-set (patterns test oldbndgs cover lfs)
  "This searches the set of individual binding results for an overall consistent set across all patterns"
  (if patterns
      (mapcar #'(lambda (binding-result)
		  (let ((newbndgs (combine-bndgs (binding-result-bndgs binding-result) oldbndgs)))
		    (if newbndgs 
			(find-bndg-set (cdr patterns) test newbndgs (remove-if #'null (cons (binding-result-covers binding-result) cover)) lfs))))
	      (pattern-info-binding-lists (car patterns)))
      ;; when nil, we are done
      
      (progn
	(trace-msg 3 "Bindings found are ~S" *complete-bndgs*)
	(if test
	    (setq oldbndgs (apply-test test oldbndgs lfs)))   ;; if test fails the bdgs are set to nil
	(when (and oldbndgs (not (duplicate-in-cover cover)))
	  (push (list oldbndgs cover) *complete-bndgs*)))
      )
  )

;; some test want access to entire LF but its not an argument 
;;  so we provide access using a special variable

(defvar *lfs-for-test-functions* nil)

(defun apply-test (tests bndgs lfs)
  (let* ((boundtest (subst-in (car tests) bndgs))
	 (*lfs-for-test-functions* lfs))
    (if (fboundp (car boundtest))
	(let ((res (apply (car boundtest) (cdr boundtest))))
	  (if res
	      (let ((newbndgs (add-to-binding-list bndgs res)))
		(trace-msg 3 "~%Test ~S succeeded with bndgs ~S" boundtest res)
		(if (null (cdr tests))
		    newbndgs
		    (apply-test (cdr tests) newbndgs lfs)))
	      (trace-msg 3 "~%Test ~S failed" boundtest)))
	(im-warn "~%Undefined test found in rules: ~S" (car boundtest))
    )))


(defun combine-bndgs (bndgs1 bndgs2)
  "checks that any bindings in both are the same"
  (if bndgs1
      (let ((samevar (assoc (caar bndgs1) bndgs2)))
	(if samevar
	    (if (eq (cadar bndgs1) (cadr samevar))
		(combine-bndgs (cdr bndgs1) bndgs2)
		;; otherwise fail
		)
	    ;; var in bndg1 is not in bndgs2
	    (combine-bndgs (cdr bndgs1) (cons (car bndgs1) bndgs2)))
	)
      ;; we're done, and all the vars are in bndgs2
      bndgs2))
		

(defun match-lfs-to-rules (lfs lf-index rules)
  (when lfs
    (trace-msg 2 "Matching LF ~S ... " (car lfs))
    (mapcar #'(lambda (mnode)
		(mapcar #'(lambda (info)
			    (let ((bndgs (LF-match (pattern-info-pattern info) (car lfs))))
			      (trace-msg 5 "Matching MNODE Pattern ~S from rule ~S with ~S ... Result=~S"
					    (pattern-info-pattern info) (mnode-rule-id mnode) (car lfs) bndgs)
			      (when bndgs 
				(trace-msg 2 "        Matched rule ~S," (mnode-rule-id mnode))
				(setf (pattern-info-binding-lists info)
				      (cons (make-binding-result :bndgs bndgs :covers lf-index) (pattern-info-binding-lists info)))
									
				  )))
			(mnode-pattern mnode)))
	    rules)
    (match-lfs-to-rules (cdr lfs) (+ lf-index 1) rules)))
	  
(defun find-possible-completed-rules (mnodes)
  "returns only those mnodes that have at least one match for each pattern"
  (remove-if #'(lambda (mnode)
		     (some #'(lambda (pat) (null (pattern-info-binding-lists pat))) (mnode-pattern mnode))
		     )
	     mnodes))

(defun duplicate-in-cover (xs)
  "returns T if the list of elements contains a duplicate"
  (when xs
    (if (member (car xs) (cdr xs))
	T
	(duplicate-in-cover (cdr xs)))))
           
(defun lf-match (pattern value)
  "Matches an LF pattern against an LF value"
  (when (and (match-vals 'w::sem (car pattern) (car value))
	     (match-vals 'w::sem (cadr pattern) (cadr value))
	     (check-type-match (caddr value)(caddr pattern)))
    (match-with-subtyping (make-into-constit pattern) (make-into-constit value))))

(defun check-type-match (val pat)
  "check-type-match imposes stronger constraints than the parser unification, as we wnat strict
     inclusion of the val in the pattern, and not just non-null intersection"
  (let ((pattypes (find-typelist pat))
	(valtypes (find-typelist val)))
    (if (or (null pattypes)
	    (null valtypes)
	    (intersection pattypes valtypes))
	    
	T
	(find-subtype-match valtypes pattypes))))

(defun find-typelist (term)
  (cond ((symbolp term) (list term))
	((consp term) 
	 (if (eq (car term) ':*)
	     (find-typelist (cadr term))
	     term))
	((var-p term)
	 (if (var-values term)
	     (find-typelist (var-values term))
	     nil))))

(defun find-subtype-match (vals pats)
  (some #'(lambda (v)
	    (some #'(lambda (p) (subtype-check v p))
		  pats))
	vals))

(defun make-into-constit (lf)
  (make-constit :cat (car lf)
			:feats (list* (list :var (cadr lf)) (list :type (caddr lf))
				      (gen-role-features (cdddr lf)))))
(defun gen-role-features (roles)
  (when roles
    (cons (list (car roles) (cadr roles))
	  (gen-role-features (cddr roles)))))

(defun record-rule (n covers bndgs)
  (trace-msg 4 "    rule ~S completed with bndgs ~S" (mnode-rule-id n) bndgs)
  (setq *completed-rules*
	(cons
	 (make-mnode :rule-id (mnode-rule-id n)
		     :output (subst-in (mnode-output n) bndgs)
		     :covers (remove-if #'null covers)
		     :bndgs bndgs)
	 *completed-rules*))
  (values))

(defvar *debug-rule-id* nil)

(defun possibly-filter-for-debugging (rules)
  "removes all but the desired rule if we're debugging"
  (if *debug-rule-id*
      (progn
	(format t "~% Debugging with rule ~S" *debug-rule-id*)
	(remove-if-not #'(lambda (x) 
			   (eq (mnode-rule-id x) *debug-rule-id*))
		       rules))
      rules))

;;  FIND BEST COVERS

;;  FINDING MAXIMAL COVERING OF INPUT

(defun find-max-covers (results alreadycovered)
  (if *max-cover-with-strict-disjoint-extractions* 
      (find-max-covers1 results alreadycovered)
      (find-max-covers2 results alreadycovered)))

(defun find-max-covers1 (results alreadycovered)
  "Finds the best matches based on how much of the input they covered (results are strictly disjoint)"
  (when results
      ;; we first sort by amount of LF/pattern coverage
      (let* ((sorted-results (sort results #'> :key #'(lambda (r) (list-length
								   (remove-if #'(lambda (x) (member x alreadycovered))
									      (match-result-cover r))))))
	     
	     (best (car sorted-results))
	     (other-results (find-equiv-interps (cdr sorted-results) (list-length (match-result-cover best)))))
	;; continue only if we have best rule covers entirely new material
	(when (not (intersection (match-result-cover best) alreadycovered))
	  ;; *note*: this is checked only for the best. there can be other extractions that do not have intersection with alreadycovered (e.g., alreadycovered (6 5 2), best (2 3 4 5 2), another (7)). but this seems to be a rare case
	  (if (null other-results)
	      ;; a unique best, now try to cover the remainder of the LF
	      (cons best (find-max-covers1 (remove-subsumed-results (cdr sorted-results) (match-result-cover best)) (remove-duplicates (append (match-result-cover best) alreadycovered)))) 
	      ;;(cons best (find-max-covers1 (cdr sorted-results) (remove-duplicates (append (match-result-cover best) alreadycovered)))) --> *note* (HJ) without removing subsumbed result here, an extraction that the best subsumes may get returned in the final result
	      ;; competing alternatives, try to sort by number of variables matched
	      (let* ((varsort (sort (cons best other-results) #'> :key #'(lambda (x) (list-length (match-result-bndgs x)))))
		     (newcover  (remove-duplicates (append (match-result-cover (car varsort)) alreadycovered))))
		(cons (car varsort)
		      (find-max-covers1 (remove-subsumed-results sorted-results newcover) newcover)
		      )))))))

(defun find-equiv-interps (mns n)
  "returns all MN nodes that also cover N items"
  (if (and mns (eql (list-length (match-result-cover (car mns))) N))
      (cons (car mns) (find-equiv-interps (cdr mns) n))))

(defun remove-subsumed-results (results cover)
  (remove-if #'(lambda (r) (eql (list-length (match-result-cover r))
				(list-length (intersection (match-result-cover r) cover))))
	     results))

;; 
;; Given a match result, return a list with two elements, ("the size of LF/pattern coverage" "amount of new LF/pattern coverage")
;;
(defun compute-lf-pattern-coverage (r alreadycovered)
  (list (list-length (match-result-cover r)) (set-difference (match-result-cover r) alreadycovered)))

;;
;; Each input parameter is a result from "compute-lf-pattern-coverage"
;; - First, compare the first element (size) and then compare the second element (any new discovery over all)
;; - The new discovery is based on the collective coverage of all extractions observed so far (not new ones compared with each existing cover)
;; If strictly disjoint (with alreadycovered) covering with more new LFs is preferred, the sorting function can be changed (e.g., check the 2nd element first)
;;
(defun sort-for-max-covers2 (x y)
  (if (null (eq (first x) (first y))) ;; if the first elements are not equal
      (> (first x) (first y))         ;; compare the lengths of the first elements
      (> (list-length (second x)) (list-length (second y)))))     ;; otherwise, compare the lengths of the second elements

;;
;; Remove results the covering of which is a subset of the covering of the given result (e.g., best covering)
;; The covering is only for LFs and bindings are not taken into account
;;
(defun remove-subsumed-results-for-max-covers2 (results b)
  (let* ((B-output  (car (match-result-output b)))
	 (B-var (cadr B-output))
	 (B-args (remove-if #'(lambda (x) (eq x B-var)) (cddr B-output))))  ;; this is the list of args in the B term
    (remove-if #'(lambda (r) (and (null (set-difference (match-result-cover r) (match-result-cover b)))   ;; r is whooly subsumed by current selection
				  (let ((r-var (second (car (match-result-output r)))))                   ;; and is not an argument to B
				    (not (member r-var B-args))))) 
	       results)))

;;
;; Find covers that are not subsumed pair-wise (just in case: nothing to do with min set covering or its likes in the set covering theory)
;; It also includes whooly subsumed terms that are arguments of other extracted terms
;;
(defun find-max-covers2 (results alreadycovered)
  (if results
      ;; we first sort by (1) amount of LF/pattern coverage and (2) amount of new (non-disjoint with alreadycovered) LF/pattern coverage
      (let* ((sorted-results (sort results #'sort-for-max-covers2 :key #'(lambda (r) (compute-lf-pattern-coverage r alreadycovered))))
	     (best (select-best-result (car sorted-results) (cdr sorted-results))))
	(cons best (find-max-covers2 (remove-subsumed-results-for-max-covers2 sorted-results best)
				     (remove-duplicates (append (match-result-cover best) alreadycovered)))))))

(defun select-best-result (best results)
  (if (null results)
      best
      (if (eq (list-length (match-result-cover best)) (list-length (match-result-cover (car results))))
	  ;; have two equal coverage results, check their weights
	  (if (> (match-weight best) (match-weight (car results)))
	      (select-best-result best (cdr results))
	      (select-best-result (car results) (cdr results)))
	  best)))

(defun match-weight (match-result)
  (or (cadr (assoc (match-result-rule-id match-result) *ruleweights*))
      1))

(defun interp (lfs group-ids)
  (let ((remainder (read-lfs lfs))
	)
    (trace-msg 3 "Interpreting ~S" remainder)
    (let ((output 
	   (add-oblig-features (interpret-LF remainder group-ids) lfs)))
	 
      (trace-msg 3 "Produced ~S" output)
      (if output
	  (apply #'values (mapcar #'fixup-output output))))))

(defun fixup-output (terms)
 ;; this separates out any :context LFs so they can be integrated into the
  ;;  rest of the context. I've deleted the reassemble-frames for now as I suspect its obsolete
 #|| (multiple-value-bind
	(args context)
      (remove-arg (cdr lf) :context)
    (values (if (eq args (cdr lf)) lf
		(list* (first lf) args))
	    context)))||#
  (substitute-ids ;;(reassemble-frames
		   (substitute-stars terms)))

(defvar *stars* nil)

(defun substitute-stars (expr)
  (setq *stars* nil)
  (substitute-stars1 expr))

(defun substitute-stars1 (expr)
  "This replaces terms like *1 and *2 with unique constants"
  (cond
     ((and (symbolp expr) (not (member expr '(*me* *user*))))
      (let ((name (symbol-name expr)))

	(if (and (equal (char name 0) #\*)
		 (> (length name) 1)
		 (member (char name 1) '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
	     
	    (or (cdr (assoc expr *stars*))
		(let ((v (convert-to-package (gen-symbol 'x) *ontology-package*)))
		  (setq *stars* (cons (cons expr v) *stars*))
		  v))
	  expr)))
     ((consp expr)
    (cons (substitute-stars1 (car expr))
	  (substitute-stars1 (cdr expr))))
   (t expr)))

(defun reassemble-frames (terms)
  "This simplifies the output by inserting ROLEs back into the frame notation"
  (multiple-value-bind
      (roles frames)
      (separate #'(lambda (x) (eq (car x) 'ROLE)) terms)
    (insert-roles roles frames)))

(defun separate (fn terms)
  (when terms
    (multiple-value-bind
	(ins outs)
	(separate fn (cdr terms))
      (if (apply fn (list (car terms)))
	  (values (cons (car terms) ins) outs)
	(values ins (cons (car terms) outs))))))

(defun insert-roles (roles frames)
  "inserts ROLE assertions back into the frame in which they belong"
  (if (null roles)
      frames
    (let* ((r (car roles))
	   (rolename (second r))
	   (var (third r))
	   (val (fourth r))
	   (new-frames (mapcar #'(lambda (ref) (add-to-frame (if (keywordp rolename) rolename (keywordify rolename))
							   var val ref)) frames)))
      (insert-roles (cdr roles) new-frames))))

(defun add-to-frame (rolename var val frame)
  (if (eq var (second frame))
       (append frame (list rolename val))
	 frame))
     
 (defun substitute-ids (terms)
   (multiple-value-bind
       (IDs others)
       (separate #'(lambda (x) (eq (car x) 'ID)) terms)
     (if IDS
	 (mapc #'(lambda (x)
		   (setq others (subst (third x) (second x) others)))
	       IDs))
       others))

;;=======================
;;
;; Interface to OM mapping


(defun triples-to-keywords (x)
  (let ((typeP (car x))) ;; type must be first
    (if (eq (car typeP) 'type)
	(append (cons 'A (cdr typeP)) (process-triples (cdr x))))))

(defun process-triples (roles)
  (when roles
    (cons (util::convert-to-package (caar roles) :keyword)
	  (cons (third (car roles))
		(process-triples (cdr roles))))))


;;  Useful debug function


(defun debug-rule (id lfs rules)
  (let ((*debug-rule-id* id)
	(*trace-level* 4))
    (interpret-lf lfs rules)
    (format t "~%Final Match State:~%")
    *mnode-record*))
