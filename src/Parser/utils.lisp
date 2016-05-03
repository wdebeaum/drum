(in-package "PARSER")

(defun count-real-transforms (krontology)
  (let ((maptable (om::kr-ontology-map-table krontology))
	(res 0)
	)
    (maphash (lambda (key val)
	       ;; null lfs are on transforms for name reference only
	       (when (om::lf-kr-transform-lf val)
		 (setq res (+ res 1))
		 )
	       )
	     maptable)
    res))
  

(defun lexicon-entry-cat (entry)
  (cond
   ((lex-entry-p entry)
    (constit-cat (lex-entry-constit entry)))
   ((and (listp entry) (eql (first entry) 'composite))
    (lexicon-entry-cat (third entry))
    )
   (t (break))
   ))


(defun lexicon-entry-LF (entry)
  (cond
   ((lex-entry-p entry)
    (get-arg-value 'lf (constit-feats (lex-entry-constit entry))))
   ((and (listp entry) (eql (first entry) 'composite))
    (lexicon-entry-lf (third entry))
    )
   (t (break))
   ))


(defun lexicon-entry-sem (entry)
  (cond
   ((lex-entry-p entry)
    (get-arg-value 'sem (constit-feats (lex-entry-constit entry))))
   ((and (listp entry) (eql (first entry) 'composite))
    (lexicon-entry-lf (third entry))
    )
   (t (break))
   ))

(defun lexicon-entry-dobj (entry)
  (get-arg-value 'dobj (constit-feats (lex-entry-constit entry))))

(defun my-match (sym val)
  "Match a symbol to a value which can be another symbol or a variable"
  (cond
   ((var-p val)
    (or (null (var-values val)) 
	(eql sym (var-values val))
	(and (listp (var-values val)) (member sym (var-values val))))
    )
   (t (eq sym val))
   ))

(defun lexicon-entry-base-form (entry pos)
  "Verifies that the given entry is a base name for the specified part of speech. For now, checks only adjectives or adverbs"
  (cond
   ((eql (lexicon-entry-cat entry) 'n)
    (my-match '3s (get-arg-value 'agr (constit-feats (lex-entry-constit entry)))))
   ((eql (lexicon-entry-cat entry) 'v)
    (my-match 'base (get-arg-value 'vform (constit-feats (lex-entry-constit entry)))))
   (t t)
   ))

(defun synt-argument-sem (syntarg)
  "Given a syntactic from the lexicon, get a sem. If it is a variable, get the sem of the first value"
  (cond
   ((listp syntarg) (synt-argument-sem (first syntarg)))
   ((var-p syntarg)
    (synt-argument-sem (var-values syntarg))
    )
   ((constit-p syntarg)
    (get-arg-value 'sem (constit-feats syntarg)))
   )
  )



(defun good-vo-pair (verb noun)
  " A function to determine that v/n combination is a good pair. Returns the list of (v o vsem objsem) for every separate instance the pair matches"
  (let ((vdefs (getDefWithFeats verb :pos 'v :composite-allowed nil))
	(ndefs (getDefWithFeats noun :pos 'n :composite-allowed nil))
	(res nil)
	)
    (dolist (vdef vdefs)
      (dolist (ndef ndefs)
	(when (good-vo-def-pair vdef ndef)
	  (push (list verb noun (lexicon-entry-sem (second vdef)) (lexicon-entry-sem (second ndef))) res))
	))
    res		
    ))

(defun good-vo-def-pair (vdef ndef)
  "Checks if the v-o pair is good."
  (let* ((dobj (lexicon-entry-dobj (second vdef)))
	 (dobjsem (synt-argument-sem dobj))
	 (nsem (lexicon-entry-sem (second ndef)))
	 )
    (when (grammar-match-vals dobjsem nsem)
      (list (first vdef) (first ndef) (lexicon-entry-sem (second vdef)) nsem)
      )
    ))

(defun mark-vo-def-pair (vdef ndef)
  "Returns the pair in the format T/NIL v n vsem nsem where T/NIL indicates whether the pair is good"
  (let* ((dobj (lexicon-entry-dobj (second vdef)))
	 (dobjsem (synt-argument-sem dobj))
	 (nsem (lexicon-entry-sem (second ndef)))
	 (descr (list (first vdef) (first ndef) (lexicon-entry-sem (second vdef)) nsem))
	 )
    (if (grammar-match-vals dobjsem nsem)
	(cons t descr)
      (cons nil descr))
    ))



(defun separate-good-pairs (list)
  "Given a list, separates it good and bad pairs"
  (let ((goodp nil) (badp nil))
    (dolist (pair list)      
      (let ((pd (mark-vo-def-pair (first pair) (second pair)))
	    )
	(if (first pd)
	    (push (rest pd) goodp)
	  (push (rest pd) badp))
	))
    (list
     (reverse goodp) (reverse badp))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A useful testing function to get the defined entries and see all warnings
;;
(defun find-lexical-entries (words &key pos lfs (composite-allowed t))
  (mmapcan (lambda (x) (getDefWithFeats x :pos pos :lfs lfs :composite-allowed composite-allowed))
	   words))

(defun getDefWithFeats (word &key pos lfs (composite-allowed t))
  (mapcar (lambda (def)
	    (list word def))
	  (remove-if-not
	   (lambda (x)
	     (and 
	      (or composite-allowed (lex-entry-p x))	      
	      (or (null pos) (and (eql (lexicon-entry-cat x) pos) (lexicon-entry-base-form x pos)))
	      (or (null lfs) (member (lexicon-entry-lf x) lfs :test (lambda (x y) (subtype 'lf x y))))
	      ))	   
	   (lxm::retrieve-from-lex word)
	   )	    
    ))

(defun cproduct (set1 set2)
  "Makes a cartesian product: all pairs with the first element of set1 and second element of set2"
  (cond
   ((null set1) nil)
   (t
    (let ((el (first set1)))	  
      (append
       (mapcar (lambda (x) (list el x)) set2)
       (cproduct (rest set1) set2)
       ))
    ))
  )

(defun make-basic-sentence (pair)
  "Makes a basic sentence out of a pair which is just verb the noun"
  (list
   'sentence-test
   :sent
   (format nil "~S the ~S" (first pair) (second pair))
   ))

(defun print-var (var fname) 
  (let ((outstream (open fname :direction :output
			 :if-exists :supersede :if-does-not-exist :create)))
    (print var outstream)
    (close outstream)
    ))


(defun get-VO-pair (sent)
  "Given v [prep] the n sentence, produces a (v n) pair"
  (let* ((p1 (position #\Space sent))
	 (p2 (position #\Space sent :from-end t))
	 )
    (list (subseq sent 0 p1)
	  (subseq sent (+ p2 1))
	  )
    ))

(defvar *vtable* (make-hash-table :test #'equal)) 
(defvar *otable* (make-hash-table :test #'equal)) 
(defvar *ftable* (make-hash-table :test #'equal)) 
(defvar *fvtable* (make-hash-table :test #'equal)) 

(defconstant *type-feature-name* 'list-type)

(defvar *vcounter* 0)
(defvar *ocounter* 0)


(defun clear-tables ()
  (clrhash *vtable*)
  (clrhash *otable*)
  (setq *vcounter* 0)
  (setq *ocounter* 0)
  )

(defun create-feature-encodings (&key exclude-features exclude-values allow-parents lowest-levels)
  "Creates feature encodigns based on the current feature table. Lovest-levels is a list indexed by feature. If a feature value falls under a lowest level, it is encoded with that level's value"  
  (let ((fnamecounter 0)	
	)
    (clrhash *ftable*)  
    (clrhash *fvtable*)
    (sethash *type-feature-name* 0 *ftable*)    
    (sethash '- 0 *fvtable*)
    (sethash '+ 1 *fvtable*)
    
    (let ((tvcounter -1)
	  )
      (maphash (lambda (key val)
		 (sethash key (incf tvcounter) *fvtable*))
	       (ling-ontology-feature-type-table *lf-ontology*))
        
      (maphash (lambda (key val)
		 (unless (member key exclude-features)
		   (sethash key (incf fnamecounter) *ftable*)
		   (create-feature-value-encodings (om::feature-description-values val) 0 :exclude-values exclude-values :allow-parents allow-parents :lowest-levels (second (assoc key lowest-levels)))
		   ))
	       (ling-ontology-feature-table *lf-ontology*))
      )
    ))
    
     

(defun is-leaf-description (x)
  (null (featval-description-children (cdr x))))
    
(defun create-feature-value-encodings (vallist fvcounter &key (allow-parents nil) (exclude-values nil) (lowest-levels nil))
  (let* ((res nil)
	 (leaves (remove-if-not #'is-leaf-description vallist))
	 (parents (remove-if #'is-leaf-description vallist))
	 (coded-leaves (set-difference leaves lowest-levels :test (lambda (x y) (subtype 'sem (first x) y))))
	 (coded-parents (intersection parents lowest-levels :test (lambda (x y) (eql (first x) y))))
	 (firstcoded (union (union coded-leaves coded-parents :test #'equal) (and allow-parents parents) :test #'equal))
	 (levelcoded (set-difference leaves coded-leaves :test #'equal))
	 (listcoded (set-difference (and (not allow-parents) parents) coded-parents :test #'equal))
	 (fvalcounter fvcounter)
	 )
    ;;(print coded-leaves)
    ;;    (print listcoded)
    (dolist (fdescr firstcoded)
      (let* ((fv (first fdescr))
	     (enc (gethash fv *fvtable*))
	    )
	(cond
	 ((null enc)
	  (sethash fv (incf fvalcounter) *fvtable*))	 
	 ((eql fv '-) nil) ;; don't try to encode -
	 ((and exclude-values (member fv exclude-values)) nil) ;; don't try to encode excluded values
	 (t ;; have encoding - doublecheck
	  (incf fvalcounter)
	  (when (not (eql enc fvalcounter))
	    (format t "Possibly inconsistent encoding ~S for value ~S: expecting ~S~%" enc fv fvalcounter))
	  ))
	))
    
    (dolist (fvdescr levelcoded)
      (let ((level (first (member (first fvdescr) lowest-levels :test (lambda (x y) (subtype 'sem x y))))))
	(sethash (first fvdescr) (gethash level *fvtable*) *fvtable*)))
    
    (dolist (fvdescr listcoded)
      (let ((children (featval-description-children (cdr fvdescr)))
	    (enc nil))
	;;	(print enc)			
	(dolist (child children)
	  (let ((chenc (gethash (om::featval-description-value child) *fvtable*)))
	    ;;	    (format t "~% CHENC ~S ~S" child chenc)
	    (if (listp chenc)
		(setq enc (append chenc enc))
	      (push chenc enc))
	    ))
	(format t "~S ~S~%" (first fvdescr) enc)
	(sethash (first fvdescr) (remove-duplicates enc) *fvtable*)
	))
    ))

(defun print-vo-learning-pair (pair stream &key exclude-features (minus-impossible-features t))
  "Given a verb-object pair, adds the verb and the object to the tables as necessary and prints out a learning pair. Prints as - values of all features inconsistent with the type. Set minus-impossible-features to nil to prevent adding any feature values"
  (let* ((verb (first pair))
	 (obj (second pair))
	 (feats (and (fourth pair) (var-values (fourth pair))))
	 (ftype (and feats (aref feats 0)))
	 (vnum (gethash verb *vtable*))
	 (onum (gethash obj *otable*))
	 ;; will add features, but not the type feature, and not those on exclude list
	 (addfeats (remove *type-feature-name* (set-difference (mapcar #'second (get-sorted-descr-key *ftable*)) exclude-features)))
	 )
    (when (null vnum)
      (setq vnum *vcounter*)
      (setq *vcounter* (+ *vcounter* 1))
      (sethash verb vnum *vtable*)
      )
    (when (null onum)
      (setq onum *ocounter*)
      (setq *ocounter* (+ *ocounter* 1))
      (sethash obj onum *otable*)
      )    
    ;;    (print obj)
    ;; (print onum) 
    (format stream "~S ~S " vnum onum)
    (when ftype
      (print-fv-in-learning-pair stream *type-feature-name* ftype exclude-features)
      ;; now encode features
      (dotimes (i (- (length feats) 1))
	(let* ((fnum (1+ i))
	       (fname (get-feature-name-by-index ftype fnum))
	       (fval (aref feats fnum))
	       )
	  ;; if this feature is defined, we should not try to supplement - later
	  (setq addfeats (remove fname addfeats))
	  (print-fv-in-learning-pair stream fname fval exclude-features)
	  ))
      ;; now print supplementary - features
      (when minus-impossible-features
	(dolist (afname addfeats)
	  (print-fv-in-learning-pair stream afname '- exclude-features)))
      (format stream "~%")
      )
    ))


(defun print-noun-descr (ndef stream &key exclude-features (minus-impossible-features t))
  "Given a verb-object pair, adds the verb and the object to the tables as necessary and prints out a learning pair. Prints as - values of all features inconsistent with the type. Set minus-impossible-features to nil to prevent adding any feature values"
  (let* ((noun (first ndef))
	 (feats (var-values (lexicon-entry-sem (second ndef))))
	 (ftype (and feats (aref feats 0)))
	 ;; will add features, but not the type feature, and not those on exclude list
	 (addfeats (remove *type-feature-name* (set-difference (mapcar #'second (get-sorted-descr-key *ftable*)) exclude-features)))
	 )
    (format stream "~S " noun)
    (when ftype
      (print-fv-in-learning-pair stream *type-feature-name* ftype exclude-features)
      ;; now encode features
      (dotimes (i (- (length feats) 1))
	(let* ((fnum (1+ i))
	       (fname (get-feature-name-by-index ftype fnum))
	       (fval (aref feats fnum))
	       )
	  ;; if this feature is defined, we should not try to supplement - later
	  (setq addfeats (remove fname addfeats))
	  (print-fv-in-learning-pair stream fname fval exclude-features)
	  ))
      ;; now print supplementary - features
      (when minus-impossible-features
	(dolist (afname addfeats)
	  (print-fv-in-learning-pair stream afname '- exclude-features)))
      (format stream "~%")
      )
    ))




(defun print-fv-in-learning-pair (stream fname fvalue exclude-features)
  (when (not (member fname exclude-features))
    (let ((encval (encode-feature-value fname fvalue))) 
      (format stream "~S ~S  "
	      (gethash fname *ftable*)
	      (if (consp encval) encval (list encval)))
      )))

(defun encode-feature-value (fname fval)
  "Takes a feature value and attempts to encode it"
  (cond
   ((null fval) ;; no value - do all possible values, but skip lists, because they contain repetitions
    (remove-duplicates (remove-if #'listp (encode-feature-value fname (get-all-feature-values fname)))))
   ((symbolp fval)
    (gethash fval *fvtable*))
   ((listp fval)
    (remove-duplicates (mapcar (lambda (x) (encode-feature-value fname x)) fval)))
   ((var-p fval)
    (encode-feature-value fname (var-values fval)))    
  ))

(defun get-all-feature-values (fname &key superval)
  "Get all possible values for feature by name"
  (if (eql fname *type-feature-name*)
      *sem-types*
    (remove-if
     (lambda (x)
       (and superval (not (subtype fname x superval))))
     (mapcar #'first (om::feature-description-values (gethash fname (ling-ontology-feature-table *lf-ontology*))))
     )))
	   
      
(defun get-vo-pairs (testlist)
  "Given a list in sentence-test format, produces a list of learning pairs"
  (mapcar (lambda (x) (get-VO-pair (basic-test-entry-uttered x))) testlist)
  )

(defun save-vo-pairs-from-test-list (testlist fname)
  (let ((outstream (open fname :direction :output
			 :if-exists :supersede :if-does-not-exist :create)))
    (mapcar (lambda (x) (print-vo-learning-pair x outstream)) (get-vo-pairs testlist))
    (close outstream)
    ))

(defun save-vo-pairs-from-list (list fname &key exclude-features)
  (let ((outstream (open fname :direction :output
			 :if-exists :supersede :if-does-not-exist :create)))
    (mapcar (lambda (x) (print-vo-learning-pair x outstream :exclude-features exclude-features)) list)
    (close outstream)
    ))


(defun save-words-from-list (wlist fname &key exclude-features)
  (let ((outstream (open fname :direction :output
			 :if-exists :supersede :if-does-not-exist :create)))
    (mapcar (lambda (x) (print-word-descr x outstream :exclude-features exclude-features)) list)
    (close outstream)
    ))


(defun save-vo-pair-descriptions (fname &key exclude-features)
  "Saves the descriptions of verbs and objects in the file that can be later used to decode the results"
  (let* ((outstream (open fname :direction :output
			  :if-exists :supersede :if-does-not-exist :create))
	 (vds (get-sorted-descr-key *vtable*))
	 (ods (get-sorted-descr-key *otable*))
	 (fs (get-sorted-descr-key *ftable*))
	 ;; features not excluded from consideration in the table
	 (goodfs (remove-if (lambda (feat) 
			      (member (second feat) exclude-features)
			      ) 
			    fs))
	 )
    (format outstream "First: ~S~%" (length vds))
    (mapc
     (lambda (x) (format outstream "~S  ~S~%" (first x) (second x)))
     vds)
    (format outstream "Second: ~S~%" (length ods))
    (mapc
     (lambda (x) (format outstream "~S  ~S~%" (first x) (second x)))
     ods)  
    (format outstream "Features: ~S ~%" (length goodfs))
    (dolist (feat goodfs)
      (let* ((fname (second feat))
	     (fvals (get-all-feature-values fname))
	     (fvds (get-sorted-descr-key *fvtable* :limitvals fvals))
	     )		
	(print fvals)
	;;	(print fvds)
	(pushnew '(0 -) fvds :test #'equal)
	(format outstream "~S  ~S    ~S ~%" (first feat) fname (length fvals))
	(mapc (lambda (x) (format outstream "  ~S  ~S~%" (first x) (second x)))
	      fvds)
	))	  
    (close outstream)
    ))



(defun get-sorted-descr-key (table &key limitvals)
  "Given a hashtable that makes string correspond to numbers, returns a list in the format (number string) sorted by the number"
  (let ((res nil))
    (maphash
     (lambda (key val) 
       (when (and (numberp val) (or (null limitvals) (member key limitvals)))
	 (push (list val key) res)))
     table)
    (sort res #'< :key #'car)
    ))


(defun equal-sem-var (var1 var2)
  (or 
   (and (null var1) (null var2))
   (and (var-p var1) (var-p var2) (equal-sem-arrays (var-values var1) (var-values var2)))))

(defun equal-pairs (p1 p2)
  (and
   (equal (first p1) (first p2))
   (equal (second p1) (second p2))
   (equal-sem-var (third p1) (third p2))
   (equal-sem-var (fourth p1) (fourth p2))
   ))

(defun equal-sem-arrays (ar1 ar2)
  (and (arrayp ar1) (arrayp ar2)
       (not (mismatch ar1 ar2 :test #'equal-values))))

(defun equal-values (val1 val2)
  (or (equal val1 val2)
      (and
       (var-p val1) (var-p val2)
       (equal (var-values val1) (var-values val2))
       )))

(defun contains-feature-value (feat val arr)
  "True if a given sem array contains a specified feature value"
  (cond 
   ((var-p arr)
    (contains-feature-value feat val (var-values arr)))
   ((arrayp arr)    
    (equal-values val (get-sem-feature-value arr feat)))
   (t nil)
   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Analyzing grammar for features
;;

(defun print-feature-rule-list (&key (stream t))
  "Prints nicely a feature-rule list produced by make-feature-rule-list function"
  (let ((lst (sort (make-feature-rule-list) #'string-lessp :key #'(lambda (x) (format nil "~S" x)))))
    (dolist (el lst)
      (let ((fval (first el))
	    (rules (rest el))
	    )
	(format stream "~S: ~S~%" fval (length rules))
	(dolist (rule rules)
	  (format stream "        ~S~%" rule)
	  )
	))
    ))
	

(defun make-feature-rule-list ()
  "From the current grammar, creates a list with elements in the form
   ((feature-name feature-vale) rule-id-list)
   where rule-id-list is the list of all rule ids in which a given feature-value pair is mentioned
  "
  (let ((res nil)
	(rulefeats (find-all-rule-features))
	(rule-feat-hash (make-hash-table :test #'equal))
	)
    (dolist (el rulefeats)
      (let ((rule-id (first el))
	    (feats (second el))
	    )
	(dolist (featval feats)
	  ;;	  (format  t "(push ~S (gethash ~S rule-feat-hash) -- ~S~%" rule-id featval (gethash featval rule-feat-hash))
	  (push rule-id (gethash featval rule-feat-hash))
	  )
	))
    (maphash #'(lambda (key val) 
		 (push (cons key val) res)
		 )
	     rule-feat-hash)
    res
    ))
  

(defun find-rules-with-feature-value (feat val)
  "Produces a list of all rules which mention this feature value. Does not take account of subtypes, or of variables with multiple values."
  (mapcar #'rule-id
	  (remove-if-not #'(lambda(rule)
			     (rule-contains-feature-value feat val rule))
			 (cgrammar-rules *default-grammar*))
	  ))

(defun find-rules-with-feature (feat)
  "Returns a list of all rules which mention this feature."
  (mapcar #'rule-id
	  (remove-if-not #'(lambda(rule)
			     (assoc 'feat (get-rule-sem-features rule)))
			 (cgrammar-rules *default-grammar*))
	  ))


(defun find-all-rule-features ()
  "Return the list in the format (rule-name rule-features)
   where rule-name is the name of a rule which mentions sem features either on left or right hand side
   and rule-features is the list of all features mentioned in that rule
   Only features mentioned in SEM of rule constituents are counted. "

  (remove-if #'(lambda(res) (null (second res)))
	     (mapcar #'(lambda (rule)
			 (list (rule-id rule)
			       (get-rule-sem-features rule)))
		     (cgrammar-rules *default-grammar*)
		     ))
  )

(defun rule-contains-feature-value  (feat val rule)
  "Determine if a rule contains the (feat val) pair. 
   The rule contains the pair if either the left hand side or one of the right hand side consitutents
   contain a SEM feature which contains (feat val) pair"
  (or (contains-feature-value
       feat val
       (get-value (rule-lhs rule) 'sem)			    
       )
      (member-if #'(lambda (constit)
		     (contains-feature-value
		      feat val
		      (get-value constit 'sem)
		      ))
		 (rule-rhs rule))))    


(defun get-rule-sem-features (rule)
  "Returns a list of all semantic features mentioned in the rule"
  (remove-duplicates 
   (append (get-constit-sem-features (rule-lhs rule))
	   (mmapcan #'get-constit-sem-features  (rule-rhs rule))
	   )
   :test #'equal
   ))

(defun get-constit-sem-features (constit)
  "Returns the list of sem features, if any, contained in the SEM feature on the constit"
  (let ((semval (get-value constit 'sem)))
    (when (var-p semval)
      (setq semval (var-values semval)))
    (when (arrayp semval)
      (let ((flist (build-list-from-sem-array semval)))
	(cons
	 (list 'list-type (first flist))
	 (rest flist))
	))
    ))
