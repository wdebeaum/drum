;; TODO:  eventually need a pattern store for matching multiple LFs at once
;;  variable binding is not right yet -- we don't ensure consistency across a match
;; equality reasoning

(in-package "IM")

;; Managing the LF store - a hashtable of all the LFs in the current discourse
;;   but could be used for more as a quick knowledge base

(defstruct LFterm
  Type  ;; Q (quant), F (pred), AND, OR, NOT, 
  Qtype   ;; Original specifier in LF form
  var     ;; the Var/id for this object
  Class   ;; the ont type of expression (key for fast indexing retrieval), for Qs and Fs
  Arity   ;; number of arguments and mods (key for rapid elimination of impossible matches, i.e., not enough arguments)
  Nummods
  Args    ;; alphabetical ordering of slot-value pairs
  Mods    ;; pointers to modifiers
  start
  end
  )

(defstruct LFobject    ;; the structure used to implement dereferencing for the union-find equality algorithm
   Name
   Value
   Ref  ;; the equality link
   ) 

(defvar *LF-store* (make-hash-table))
(defvar *ont-LF-index* (make-hash-table))
(defvar *missing-refs* (make-hash-table))  ;; back pointers to terms with ids that have not been defined yet

(defun resetLFstore nil
  (setq  *LF-store* (make-hash-table))
  (setq *ont-LF-index* (make-hash-table))
  (setq *missing-refs* (make-hash-table)))

(defun remember-missing-ref (id term)
  "the TERM has ID in it which is unresolved"
  (setf (gethash id *missing-refs*)
	(cons term (gethash id *missing-refs*))))

(defun buildLFterm (LF start end)
  (let ((spec (car LF))
	(var (cadr LF))
	(class (caddr LF))
	)
    (multiple-value-bind 
	  (args mods missing-refs)
	(breakoutargs (cdddr LF) nil nil)
      (Let ((Term (make-LFterm   :type (classify-type spec) 
				 :Qtype spec
				 :var var
				 :class class
				 :arity (list-length args)
				 :Nummods (if (consp mods) (list-length mods) 1)
				 :args args
				 :mods mods
				 :start start
				 :end end)))
	(if missing-refs
	    (mapcar #'(lambda (r) (remember-missing-ref r term))
		    missing-refs))
	term))))

(defun storeLF (LF start end &optional &keyword replaceOK)
  (let* ((id (second LF)))
    (if (symbolp id)
	(let ((lfterm  (make-LFobject :name id
				      :value (buildLFterm LF start end)))
	      (oldlf (gethash id *LF-store*)))
	  (cond 
	    ((and (null replaceOK) oldlf)
	      (im-warn "ID already defined - call with :replaceOK T"))
	    (oldlf  ;; already exists, we replace the LF fields
                    ;; maybe possible bug if new LF has unseen objects?
	     (setq oldlf (lfobject-value oldlf))
	     (let ((newlf (lfobject-value lfterm)))
	       (setf (LFterm-type oldlf) (LFterm-type newlf))
	       (setf (LFterm-Qtype oldlf) (LFterm-Qtype newlf))
	       (setf (LFterm-class oldlf) (LFterm-class newlf))
	       (setf (LFterm-arity oldlf) (LFterm-arity newlf))
	       (setf (LFterm-nummods oldlf) (LFterm-nummods newlf))
	       (setf (LFterm-args oldlf) (LFterm-args newlf))
	       (setf (LFterm-mods oldlf) (LFterm-mods newlf))))
	    (t  ;; usual case, a new LF term
	     (setf (gethash id *LF-store*) lfterm)
	     (instantiate-prior-terms id lfterm)
	     (add-to-ont-index lfterm)
	     ;; return the structure
	     lfterm)))
	(format t "Error: non atomic LF ID: ~S" id))
    ))

(defun add-to-ont-index (term)
  (let* ((class (LFterm-class (LFobject-value term)))
	(core-class (if (consp class) (second class) class)))
    (push term (gethash core-class *ont-LF-index*))))

(defun breakoutargs (in args mods)
  (if (null in)
      (multiple-value-bind (derefargs missing-refs)
	  (prepare-args args)
	(values derefargs mods missing-refs))
      (if (member (car in) '(:mod :mods))
	  (breakoutargs (cddr in) args (append mods (if (consp (cadr in)) (cadr in) (list (cadr in)))))
	  (case (car in) 
	    ((:start :end :proform :tma :coref :refers-to)  ;; ignore proform and tma
	     (breakoutargs (cddr in) args mods))
	      ;; otherwise add as an arg
	    (otherwise
	     (breakoutargs (cddr in) (list* (car in) (cadr in) args) mods))))))

(defun classify-type (spec)
  (case spec
    (ont::F 'F)
    (otherwise 'Q)))
  
(defun prepare-args (args)
  "make args into an assoc list, dereferencing ids when possible, and remember ones w/o refs yet"
  (when args
    (let ((ref (lookupLF (cadr args))))
      (multiple-value-bind (newargs newmissing)
	  (prepare-args (cddr args))
	(values (cons (list (car args) ref) newargs)
		(if (not (LFterm-p ref))
		    (cons ref newmissing)))))))

(defun instantiate-prior-terms (id val)
  "This replace the ID in formulas asserted previosuly with the structure just defined"
  (let ((terms (gethash id *missing-refs*)))
    (when terms
	(mapcar #'(lambda (x) (instantiate-value id val x)) terms))))

(defun instantiate-value (id val term)
  "replace ID with VAL in TERM -- note this is destructive!"
  (setf (lfterm-args term)
	(Safesubst val id (lfterm-args term))))

(defun safesubst (val id arglist)
  "replaces ID with VAL in EXPR - but avoids infinite loops in the structures"
  (if (and (LFobject-p val) (not (present-in id (lfterm-args (lfobject-value val)))))
      (subst val id arglist)
      arglist))

(defun present-in (val expr)
  "T is val is somewhere in the expression"
  (when expr
    (cond ((consp expr)
	   (or (present-in val (car expr)) (present-in val (cdr expr))))
	  ((LFobject-p expr)
	   (or (eq val (LFobject-name expr))
	       (present-in val (LFterm-args (LFobject-value expr)))))
	  (T
	   (eq val expr)))))

;; replace LF in record 

(defun replace-lf-in-store (lf)
  "eventually we'll need to fix up any existing equality bindings, etc"
  (storeLF lf nil nil :replaceOK t))
  
;; equality handling

(defun add-coref (id ref)   ;; simple version -- we don't merge any properties, just take the REF properties
  (let ((idterm (getLFbyID id))
	(idref (getLFbyID ref)))
    (if (and (LFobject-p idterm) (LFobject-p idref))
	(setf (LFobject-ref idterm)
	      idref))))

(defun deref (id)
  (or (chase-down-refs id)
      id))

;;  Accessing the LF-store, traversing the equality chains as necessary

(defun getLFbyID (id)
 "returns the LFOBJECT structure for the ID"
  (let ((obj (gethash id *LF-store*)))
    (when obj
      (if (null (LFobject-ref obj))
	  obj
	  (let ((rev-obj (chase-down-refs obj)))
	    (if rev-obj
		(progn ;; compressing the equality chain before returning
		  (setf (LFobject-ref obj) rev-obj) 
		  rev-obj)
		obj))))))

(defun getLFform (id)
  "returns the logical form for the ID"
  (let* ((x (getLFbyID id))
	 (v (if (lfobject-p x) (lfobject-value x)))
	 (mods (lfterm-mods v))
	 (args (replace-objects-with-vars (lfterm-args v)))
	 (TERM (list (lfterm-qtype v) id (lfterm-class v))))
    (if mods (setq term (append term args (list :mods mods)))
	(append term args)
   )))

(defun replace-objects-with-vars (arglist)
  (mapcan #'(lambda (fvpair)
	      (let ((feat (car fvpair))
		    (arg (cadr fvpair)))
		(list feat (if (lfobject-p arg)
			       (lfterm-var (lfobject-value arg))
			       arg))))
	  arglist))
	    
(defun get-start-end-for-lf (id)
  (let ((rec (getLFbyID (convert-to-package id :ont))))
    (if rec
	(values (lfterm-start (lfobject-value rec)) (lfterm-end (lfobject-value rec))))))

(defun showLF (id)
  (prettyfy (getLFbyID id)))

(defun prettyfy (expr)
  (when expr
    (cond ((LFobject-p expr)
	   (if (LFobject-ref expr)
	       (prettyfy (LFobject-ref expr))
	       (prettyfy (LFobject-value expr))))
	  ((LFterm-p expr)
	   (list* (LFterm-qtype expr) (LFterm-var expr) (LFterm-class expr)
		  :MODS (LFterm-mods expr)
		  (prettyfy (LFterm-args expr))))
	  ((consp expr)
	   (mapcar #'prettyfy expr))
	  (t 
	   expr))))

(defun chase-down-refs (obj)  ;; traces down the REF chain
  (when (LFobject-ref obj)
    (or (chase-down-refs (LFobject-ref obj)) (LFobject-ref obj))))
	       
(defun lookupLF (id)
  "returns stored value if there, otherwise retains the id"
  (or (getLFbyID id) 
      id))

(defun findLFSbyType (type)
  (mapcar #'LFobject-value (gethash type *ont-LF-index*)))

;; finding "similar" LF structure - used to quick retrieval before more expensive graph matching

(defun filter-by-fastmatch-subsumes (patid datumids)
  "returns the terms that fastmatch the pattern LF - returns IDs that are subsumed by PAT"
  (let ((pat (lfobject-value (lookupLF patid))))
     (mapcar #'LFterm-var 
	     (remove-if #'null (mapcar #'(lambda (d) (LF-fastmatch-term pat (lfobject-value (lookupLF d))))
			      datumids)))))

(defun filter-by-fastmatch-subsumed-by (patid datumids)
  "returns the terms that fastmatch the pattern LF - returns IDs that are subsumed by PAT"
  (let ((pat (lfobject-value (lookupLF patid))))
    (mapcar #'LFterm-var
	    (remove-if #'null 
		       (mapcar #'(lambda (d) (LF-fastmatch-term (lfobject-value (lookupLF d)) pat))
			       datumids)))))


(defun LF-fastmatch (patid datumid)
  "datum and pat are both IDs into the LF store - returns T if PAT subsumes DATUM"
  (LF-fastmatch-term (lfobject-value (lookupLF patid)) (lfobject-value (lookupLF datumid))))

(defun LF-fastmatch-term (pat datum)
  "pat and datum are LFterm structures - returns T if PAT subsumes DATUM"
  (if (or (eq pat datum)
	  (and (subtype-check (LFterm-class datum) (LFterm-class pat))
	       (<= (LFterm-arity pat) (LFterm-arity datum))
	       (<= (LFterm-nummods pat) (LFterm-nummods datum))))
      datum))
  

;; accessing by pattern matching

(defun find-matches-in-LFstore (lfs)  
  "Find elements in LFstore that match the provided Logical form - the first LF is the root LF"
  (let ((qlfs (convert-graph-to-LFterm-patterns lfs)))
    (find-matching-lf-terms (first qlfs)
			    (findLFSbyType (caddr lfs)))))

(defun find-matching-lf-terms (pat data)
  (remove-if #'null
	     (mapcar #'(lambda (d) (match-lf-terms pat d)) data)))

(defun match-lf-terms (pat datum)
  (if (eq pat datum)
      (list datum *success*)
      (if (and (match-vals 'lf (LFterm-var pat) (LFterm-var datum))
	       (subtype-check (LFterm-class datum) (LFterm-class pat))
	       (<= (LFterm-arity pat) (LFterm-arity datum))
	       (<= (LFterm-nummods pat) (LFterm-nummods datum)))
	  (let ((bndgs (Match-LF-args (LFterm-args pat) (LFterm-args datum))))
	    ;; should match mods as well!!
	    (if bndgs 
		(list datum (add-to-binding-list (list (list (LFterm-var pat) (LFterm-var datum)))
							 bndgs)))))))

(defun Match-LF-args (patArgs dataArgs)
  "matches two argument lists, with the argument positions in alphabetical order"
  (if patargs
    (let* ((fvpat (car patArgs))
	   (f (car fvpat))
	   (ArgsRest (assoc+ f dataArgs)))
      (if (and ArgsRest (<= (list-length patargs) (list-length ArgsRest)))
	  (let ((bndgs1 (Match-LF-values (cadr fvpat) (cadar ArgsRest)))
		(bndgsRest (Match-LF-args (cdr patArgs) (cdr ArgsRest))))
	    (if (and bndgs1 bndgsRest)
		(add-to-binding-list bndgs1 bndgsRest)))))
    *success*))

(defun Match-LF-values (pat data)
  "Here either are atoms or embedded LF-term structures"
  (if (LFobject-p pat)
      (and (LFobject-p data)
	   (cadr (match-LF-terms (LFobject-value (deref pat))     ;; we just need the bindings (hence the CADR)
				 (LFobject-value (deref data)))))
      ;; both are non-structures
      (match-with-subtyping pat data)))
      
(defun assoc+ (item list)
  "returns the list of values remaining after finding the item"
  (when list
    (if (eq item (caar list))
	list
	(assoc+ item (cdr list)))))

	
;;;====

;; testing code


(defun reff (x)
  (resolve-and-add (parse-to-lfs x)))

(defun resolve-refs-in-new-lf (lfs)
  (let* ((n (add-utt-to-record lfs))
	 (new-rec (aref *im-record* n))
	 (new-exprs (utt-record-salience new-rec)))
    (mapcar #'(lambda (r)
		(let ((hyps (find-possible-refs r n nil nil)))
		  (setf (referent-ref-hyps r) hyps)
		  (if (car hyps)
		      (progn 
			(install-hyp-in-referent r (car hyps))
			(append (referent-lf r) (list :coref (referent-coref r))))
		      (referent-lf r))))
	    new-exprs)))
    

(defun add-utt-to-record (lfs)
  (setq *im-utt-count* (+ *im-utt-count* 1))
  (setf (aref *im-record* *im-utt-count*)
	(im::make-utt-record   :lfs lfs
			       :status T
			       :salience  (mapcar #'(lambda (lf) (build-ref-structure lf))
					      lfs)
			       :index *im-utt-count*
			       ))
  *im-utt-count*)

(defun build-ref-structure (lf)
  (storeLF nil nil lf)
  (make-referent
   :id (second lf)
   :lf LF
   :accessibility (classify-accessibility LF)
   ))

;;== converting LFs to queries by adding question marks

(defun convert-graph-to-LFterm-patterns (lfs)
  (mapcar #'buildLFterm (read-value (add-questionmark-to-vars lfs) nil)))

(defun add-questionmark-to-vars (lfs)
  (let* ((mappings (mapcar #'gen-var-pair lfs)))
    (values (subst-mappings lfs mappings) mappings))
  )

(defun gen-var-pair (lf)
  (list (second lf) (build-var (second lf) nil)))

(defun subst-mappings (lfs mappings)
  (if  mappings
       (let ((m (car mappings)))
      (subst-mappings (subst (cadr m) (car m) lfs) (cdr mappings)))
    lfs))

(defun build-quickmatch-pattern (lf)
  "this just makes every arg a variable"
  (list* (car lf) (build-var (second lf) nil) (third lf)
	 (build-quickmatchargs (cdddr lf))))

(defun build-quickmatchargs (args)
  (when args
    (list* (car args) (build-var (second args) nil)
	  (build-quickmatchargs (cddr args)))))
  
