(in-package "PARSER")

;; From ../Lexicon/Code/make-new-lex.lisp
;;(declaim (special *lexicon-data*))

(defvar *symbol-prefix*)
(defvar *use-sem* t "Set to nil to stop generating selelctional restrictions")

(defvar *special-entry-function* nil "Can be set to function which returns a list of special entries for a given word, e.g. if it represents a formula id in the input")

;;;======================================================================
;;;  TRAINS Chart parser code 
;;; Copyright (C) 1997 James F. Allen
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;======================================================================

;;  GRAMMAR AND LEXICON CODE
;;  This file contains functions that manage the grammar and the lexicon
;;  These are mostly I/O routines to provide for a more user-friendly system.

;;  A GRAMMAR and its LEXICON are compield from external files into a
;;  CGRAMMAR structure consisting of
;;       RULES - the compiled rules of the grammar
;;       IDS - a list of all the rule ids in the grammar, used to generate warnings
;;       INDEX - a hashtable into the rules using the cat of the first constituent on the RHS
;;       LEX - the lexicon entries for the grammar
;;       DEFAULT-RULE-PROB - the default probability for rules

;;  The user may compile and maintain several different grammars and specify which
;;  one they are interested in which each parser call.
;;  Or, these arguments can be omitted and the parser will operate using
;;  the default grammar specified by the user.
				     
;; a dummy variable to be used repeatedly in the lexicon
;; to save gc-ing

(defvar *dummy-lex-var* (make-var :name 'dummy)) ;; since these are always bound immediately
					; the same one is used in each entry

(defvar *default-rule-probability* 1.0)  ;;  the default prob for rules


;; The default grammar for use when a grammar is not specified

(defun snapshot-grammar nil
  (copy-cgrammar *default-grammar*))

(defun select-grammar (cgrammar)
  (if (cgrammar-p cgrammar)
      (progn 
	;;  build index if there isn't one
	(if (null (cgrammar-index cgrammar))
	    (build-index cgrammar))
	(setq *default-grammar* cgrammar))
    (parser-warn "Illegal grammar passed to SELECT-GRAMMAR:~%   ~S" cgrammar)))

(defun change-lexicon (newlex cgrammar)
  "Changes the lexicon in an existing grammar"
  (cond ((null newlex) cgrammar)
	((and (cgrammar-p cgrammar) (cgrammar-p newlex))
	 (let
	     ((newg (copy-cgrammar cgrammar)))
	   (setf (cgrammar-lex newg)
	     (cgrammar-lex newlex))
	   (setf (cgrammar-lex-rules newg)
	     (cgrammar-lex-rules newlex))
	   newg))
	(t (parser-warn "Bad arguments to CHANGE-LEXICON: ~S~% ~S" newlex cgrammar))))

(defun getGrammar nil
  (cgrammar-rules *default-grammar*))
  
(defun get-rule-ids nil
  (cgrammar-ids *default-grammar*))

(defun get-lexicon nil
  (if *default-grammar*
      (cgrammar-lex *default-grammar*)))

(defun set-default-rule-probability (n &optional cgrammar)
  (if (null cgrammar) 
      (setq cgrammar *default-grammar*))
  (if (cgrammar-p cgrammar)
      (setf (cgrammar-default-rule-prob cgrammar) n)
    )
  (setq *default-rule-probability* n))
  
(defun get-default-rule-prob (rhs &optional cgrammar)
  (if (null cgrammar) 
       (setq cgrammar *default-grammar*))
  (if (cgrammar-p cgrammar)
      (if (and (consp rhs) (eq (list-length rhs) 1))
	  .99  ;; unit rules don't get penalized
	  (cgrammar-default-rule-prob cgrammar))))


;;  NB: destructive
(defun append-new-rules (rules cgrammar)
  (setf (cgrammar-rules cgrammar)
    (append (cgrammar-rules cgrammar) rules)))

;;  LEXICAL CATEGORIES 


(defun defineLexicalCats (cs &optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (if (listp cs) 
      (setf (cgrammar-lexicalCats cg) cs)
    (Format t "Bad Format in ~S~%  you must pass in a list of lexical categories" cs)))

(defun addLexicalCat (c &optional cg)
   (if (null cg)
       (setq cg *default-grammar*))
   (if (symbolp c)
       (if (not (member c (cgrammar-lexicalCats cg)))
	   (setf (cgrammar-lexicalCats cg)
	     (cons c (cgrammar-lexicalCats cg))))
     (Format t "Lexical categories must be atoms. ~S is ignored" c)))

(defun getLexicalCats (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (cgrammar-lexicalCats cg))
  
(defun lexicalConstit (c &optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (and (constit-p c) 
       (member (constit-cat c) (cgrammar-lexicalCats cg))))
  
(defun nonLexicalConstit (c &optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (and (constit-p c) 
       (not (member (constit-cat c) (cgrammar-lexicalCats cg)))))


;;**************************************************************************
;;  THE GRAMMAR DATA STRUCTURE

;;     Grammar rules are of the form
;;        (<constit-pattern>  ->  <constit-pattern> ... <constit-pattern>)
;; e.g.,  ((S (INV -) (AGR ?a)) -> (NP (AGR ?a)) (VP (AGR ?a)))

;; Definition moved to structures.lisp for easier compilation.
;;(defstruct (rule
;;            (:print-function (lambda (p s k)
;;			       (declare (ignore k))
;;                               (Format s "~%<~S~%   ~S ~S>" (rule-lhs p) (rule-id p) (rule-rhs p)))))
;;  lhs id rhs)
	    
;; this copies all the variables in a rule, making sure that identical variables
;;  in different places are replaced by the identical copy
;;  This allows the same rule to be used multiple times in a parse without
;;   running into any variable conflict problems
;;   Any variables specified in the binding list will be replaced by their value
;;    just as they would using the subst-in function


;;======
;; RETRIEVAL OF RULES FROM GRAMMAR
;; returns some rules from the grammar for which the first constit
;; on the rhs *might* be unifiable with constit.  All rules which
;; apply are guaranteed to be in the list, but not all rules on the
;; list are guaranteed to be applicable.  Further filtering
;; (i.e. actually attempting unification) may be necessary.
(defun lookup-rules (constit &optional cgrammar)
  (gethash (constit-cat constit)
	   (cgrammar-index (if (cgrammar-p cgrammar)
			       cgrammar
			     *default-grammar*))))



;;======

;;  CONSTRUCTION OF GRAMMAR FROM INPUT FORMAT

;;   These functions convert a grammar specified in CAT or headfeature
;;   format into internal grammar format


;;  COMPILING THE GRAMMAR - Note this is destructive
;;  Although if passed a null grammar, it creates a new one, inheriting the gap and sem info from the default grammar

(defun compile-grammar (g &optional existing-grammar)
  (let ((new-cgrammar
	 (if (cgrammar-p existing-grammar)
	     existing-grammar
	   (progn 
	     (let ((cg (copy-cgrammar *default-grammar*)))
	       (setf (cgrammar-rules cg) nil)
	       (setf (cgrammar-lex cg) (make-hash-table))
	       (setf (cgrammar-lex-rules cg) nil)
	       (setf (cgrammar-ids cg) nil)
	       cg)))))
    (setf (cgrammar-index new-cgrammar) nil) ;; flush old index
    (convert-grammar g new-cgrammar)
    ;;(index-rules new-cgrammar)
    new-cgrammar))
 
;; CONVERT-GRAMMAR does the actual conversion from the input formats
;;  DESTRUCTIVELY CHANGING THE CGRAMMAR STRUCTURE

(defun convert-grammar (newg cgrammar)
  (if newg
    (let ((format (car newg))
	  (rules (cdr newg)))
      (Cond ((eq format 'CAT)
	     (append-new-rules
	      (merge-lists (mapcar #'(lambda (r) (build-rule r cgrammar))
				 rules))
	      cgrammar))
	    ((eq (car format) 'w::Headfeatures)
	     (append-new-rules
	      (mapcar #'(lambda (x)
			  (insertHeadFeatures x (cdr format)))
		      (merge-lists (mapcar #'(lambda (r)
					       (build-rule r cgrammar))
					   rules)))
	      cgrammar))
	    (t (parser-Warn "Bad grammar format") newg)))
    cgrammar))


;;  MERGE-LISTS collapses a list of lists into one list (using append)
   
(defun merge-lists (g)
  (cond ((null g) nil)
        (t (append-if-necessary (car g) (merge-lists (cdr g))))))

;; given a rule component in list form
;; returns type of component (NP, QUAL, etc. ) -Mark
;;
(defun strip_head (element)
  (if (listp element)
      (if (equal (car element) 'w::head) (caadr element) (car element))
    'ATOM))


;;  BUILD-RULE
;;   inserts the CAT feature for each constituent and builds all the variables.
;;   It also checks the format of the rule, and expands out any complex SEM features

(defun build-rule (r cg)
    (init-var-table)
    (if (not (verify-rule-id (cadr r) cg))
	(parser-warn "Duplicate rule id, ~S, used in rule~%  ~S~%" (cadr r) r))
    (let* ((lhs (car r))
	   (id (cadr r))
	   (third-element (third r))
	   ;; Myrosia: change to allow symbols in probabilities  -- 3/21 unmodified since get-symbol-prob weas not defined anyway JFA
	   (rhs (if (numberp third-element) (cdddr r) (cddr r)))
	   (prob (if (numberp third-element) third-element (get-default-rule-prob rhs)))
	   (lhs-constit (Verify-and-build-constit lhs r nil))
	   (newrule
	    (make-rule :lhs lhs-constit
		       :id id
		       :prob prob
		       :rhs (mapcar #'(lambda (x)
					(cond ((isvar x) 
                                              (check-if-var-can-be-bound x r)
                                              (read-value x r))
                                             ((eq (car x) 'w::head)
					      (if (caddr x) (parser-warn "Bad head specification format in rule~%  ~S" r))
					      (verify-and-build-constit (cadr x) r t))
					     (t (verify-and-build-constit x r nil))))
				   RHS)
		       :var-list (get-var-list)
		       :*-flag (or (is-*-present lhs) (some #'is-*-present rhs))
		      )))
      (if (duplicate-feature-check lhs-constit r)
	  (parser-warn "Duplicate features not allowed. Feature ~S occurs more than once in rule ~S"
		       (duplicate-feature-check lhs-constit r) (SECOND r)))

      (if (GapsDisabled) (list newrule)
	(generate-gap-features-in-rule newrule))))

(defun duplicate-feature-check (c r)
  "checks if same feature is defined twice in a constituent, and returns it if found"
  (check-for-dup (constit-feats c) r))

(defun check-for-dup (feats r)
  (if (and (listp feats) feats)
      (let ((f (caar feats)))
	(if (some #'(lambda (x) (eq (car x) f)) (cdr feats))
	    f
	  (check-for-dup (cdr feats) r)))
    ))

(defun verify-and-build-constit (constit rule head)
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((temp (car constit))
	 (cat (if (atom temp) temp
		 (read-value temp rule)))) ;;  handles vars as CAT
    ;; extract out the LF graph specification
    (multiple-value-bind (lfg synfeats)
	(split-list #'(lambda (x) (eq (car x) 'w::lfg)) (cdr constit))
      (if (and (not (symbolp cat)) (not (var-p cat)))
	  (parser-warn "Constituent category must be an atom or variable. Bad constituent ~s in rule~s  ~s"
		       constit rule))
      (let ((feats 
	   (if (check-for-defined-predicate cat)
	       ;; procedure calls do not use feature-value pairs
	       (mapcar #'(lambda (v) (read-value v rule))
		       synfeats)
	     ;; regular constituents must use feature value pairs
	     (mapcar #'(lambda (x) (read-fv-pair x rule))
		     synfeats))))
      (build-constit-in-rule cat feats (car lfg) head rule)))))
    

(defun check-if-var-can-be-bound (var rule)
  "Variables that appear in constituent positions on the RHS must have occured 
     somewhere previously in the rule in order to allow the change that they are bound"
  (let ((var-name (if (atom var)
                    (string-left-trim "?" var)
                    (symbol-name (cadr var)))))
    (if (null (get-var var-name))
      (parser-warn "Bad rule. It contains a variable ~S in a constituent position that cannot be bound: ~% ~S"
                   var-name rule))
    ))
    

(defun is-*-present (lhs)
  "Returns T if a * value is found in the specification of the lhs"
  (cond ((member lhs '(* ** ***)) t)
	((consp lhs)
	 (some #'is-*-present lhs))))


(defun verify-rule-id (id cg)
  (if (not (member id (cgrammar-ids cg)))
      (setf (cgrammar-ids cg)
	(cons id (cgrammar-ids cg)))))
  

;;  READ-FV-PAIR reads a single feature-value pair and returns its internal format
;;   The feature must either be an atom or a variable. It also converts all 
;; SEM and ARGSEM features into SEM variables.

				      
(defun read-fv-pair (fv-pair rule)
    (if (not (and (listp fv-pair) 
                  (eql (list-length fv-pair) 2)))
	(parser-warn "Bad feature-value specification ~s in rule ~s"
		     fv-pair rule)
      (let 
	  ((feat (car fv-pair))
	   (val (cadr fv-pair)))
	(check-sem-feature
	 (list (cond 
		((isvar feat)
		 (read-value feat rule))
		((symbolp feat)
		 feat)
		(t (parser-warn "Bad feature-value specification ~s in rule ~s"
				    fv-pair rule) feat))
	       (read-value val rule))
         (second rule))
        )))

;;  Code to expand SEM features

(defun check-sem-feature (fv-pair rule-id)
  "checks SEM features and makes sure its a variable. In addition, if *use-sem* is true, makes these features, and the ROLES feature, into a variable"
  (if (member (car fv-pair) '(w::sem w::argsem w::subcatsem))
      (if *use-sem*
	  (let ((val (second fv-pair)))
	    (cond 
	     ((var-p val)
	      (let ((v (var-values val)))
		(if (and v
			 (not (arrayp v))
			 (not (and (consp v) (every #'arrayp v))))
		    (parser-warn "Illegal SEM specification ~S in ~S in rule ~S" val fv-pair rule-id))
		fv-pair))
	     ((or (arrayp val)
		  (and (consp val) (every #'arrayp val)))
	      (list (car fv-pair)
		    (make-var :name (gen-v-num 'sem) ;;(gen-symbol 'sem) 
			      :values val)))
	     (t
	      (parser-warn "Illegal SEM specification ~S in ~S in rule ~S" val fv-pair rule-id)
	      fv-pair)))
	;; If we are not using sem, just generate a variable
	(list (car fv-pair)
	      (make-var :name (gen-symbol 'sem)))
	)
    (if (and (not *use-sem*) (eql (car fv-pair) 'w::roles))
	(list (car fv-pair)
	      (make-var :name (gen-symbol 'sem)))
      fv-pair)
    ))


;;  READ-VALUE checks the value to see if it is a variable, or an embedded
;;   constituent. Makes sure that variables of the same name are compatible


(defun read-value (val rule)
   "reads expression VAL associated with rule named RULE. DO NOT CALL AS A TOP-LEVEL
    FUNCTION - USE READ-EXPRESSION! This does not initialize the table of seen variables"
   (cond ((isvar val)
	  (let* ((var-name (if (atom val)
			       ;; we need symbol-name here in
			       ;; case the variable name is coming out
			       ;; of a different package
			      (string-left-trim "?" (symbol-name val))
			     (if (symbolp (cadr val))
				 (symbol-name (cadr val))
				 (cadr val))))
		 (var (get-var var-name))
		 (varval (if (consp val) (cddr val)))
		 (newval 
		  (if (consp val)
		      (if (= (length varval) 1)
			  (cond
			    ((isembeddedconstit (car varval))
			     (read-embedded-constit (car varval) 
						    rule))
			    ((is-sem-structure (car varval))
			     (build-sem-array (cons '$ (read-value (cdr (car varval)) 
								   rule)) rule))
			    (t (read-value (car varval) rule)))
		      (read-value varval rule)))))
	    (when (and (stringp var-name) (string= var-name ""))
	      ;; anomalous condition - set so we don't die!
	      (setq var-name "NULL"))
	    ;;  If we have already seen this VAR, we check compatability
            (if var
		(Check-var-compat-and-return-var newval var rule)
	      ;; Otherwise we build the new var
              ;;  if the first character of the name is "!", then set 
	      ;; the non-empty flag
	      (let ((non-empty (if (and (stringp var-name) (eq (char var-name 0) #\!)) t nil)))
		(if (atom val)
		    (add-var var-name (build-var ;;(gentemp var-name) 
				       (if (stringp var-name) (gensym var-name) var-name)
				       nil non-empty))
		  (add-var var-name (build-var ;;(gentemp var-name)
				     (if (stringp var-name) (gensym var-name) var-name)
				     newval  non-empty)))))))
	 
         ((isembeddedconstit val)
	  (read-embedded-constit val rule))
	
         ((isconjunction val)
          (read-embedded-constit (cons '% val) rule))
	 
	 ;;((and (consp val) (eq (car val) 'ONT::OR))
	  ;;	  (build-var (gentemp "X") (cdr val) nil))
	  ;;	  (build-var (gensym "X") (cdr val) nil))
	 
	 ;;((and (consp val) (eq (car val) 'ONT::NOT))
	 ;;	 (build-var (gentemp "X") (cdr val) t))
	 ;;(list ont::NOT (read-value (cadr val) rule)))
	
	((Is-sem-structure val)
	 (build-sem-variable (cons '$ (read-value (cdr val) rule)) 
			     rule))
	
	((atom val) val)
	
	((listp val)
	 (mapcar #'(lambda (x) (read-value x rule)) val))))



(defun read-expression (expr)
  "Top level function to read and convert variables and constituents"
  (init-var-table)
  (read-value expr nil))

(defun check-var-compat-and-return-var (newval oldvar rule)
  "Checks compatability of new instance of this var with old value. Prints warning
    message if bad. Returns an updated oldvar"
  (cond
   ((null newval))
   ((null (var-values oldvar)) 
    (setf (var-values oldvar) newval))
   ((symbolp newval)
    (if (not (equal newval (var-values oldvar)))
       (parser-warn "conflicting values (1) given to the same variable ~s in rule ~s~%    other value was ~s"
               oldvar rule newval)))
   ((constit-p newval)
    (if (not (match-vals 'w::sem (var-values oldvar) newval))
      (parser-warn "conflicting values (2) given to the same variable ~s in rule ~s~%    other value was ~s"
              oldvar rule newval)))
   ((listp newval)
    (if (and (= (length newval) 1) (atom (var-values oldvar)))
      (if (not (equal (car newval) (var-values oldvar)))
       (parser-warn "conflicting values (3) given to the same variable ~s in rule ~s~%    other value was ~s"
                  oldvar rule newval))
      (if (not (equal (var-values oldvar) newval))
        (parser-warn "conflicting values (4) given to the same variable ~s in rule ~s~%    other value was ~s"
		     oldvar rule newval))))
   ((arrayp newval)   ;;  i.e., a SEM value - just let it go
    )    
   (t (parser-warn "conflicting values (5) given to the same variable ~s in rule ~s~%    other value was ~s"
                                  oldvar rule newval)))
  oldvar)

;; read-embedded-consitutent
;; this converts a list of form (% cat feat-val-pair*) into a constituent

(defun read-embedded-constit (spec rule)
  (let ((cat (second spec)))
    (cond
     ((eq cat '&)
      (if (bad-conjunct (cddr spec))
	  (parser-warn "In rule ~S Conjuncts should involve explicit lists of feature-value pairs. Using variables can
                            lead to parser crashes if incorrectly bound: ~S" (second rule) spec))
      (make-constit :cat '&
                    :feats (read-value (cddr spec) rule)))
     ((and (or (atom cat)
	       (isvar cat))
           (every #'listp (cddr spec)))
      (if (and (eq cat '-))
        *empty-constit*
	
        (check-for-optional
         (make-constit 
          :cat (read-value cat rule)
          :feats (parser-add-oblig-features cat
                  (mapcar #'(lambda (x) (read-fv-pair x rule))
                          (cddr spec)))))
        ))
     (t (parser-warn "bad embedded constituent specification found: ~S" spec)))))

(defun bad-conjunct (ll)
  "All arguments in Conjuncts should be lists"
  (not (every #'(lambda (x) (and (consp x) (eql (list-length x) 2))) ll)))
    
(defun check-for-optional (c)
  "This checks if a constituent has an OPTIONAL feature and sets the :optional value"
  (let ((opt (get-value c 'w::optional)))
    (when opt
	  (setf (constit-feats c) (remove-if #'(lambda (x) (eq (car x) 'w::optional))
					     (constit-feats c)))
	  (if (eq  opt '+)
	      ;;(print "setting optional to +")
              (setf (constit-optional c) t))))
  c)

(defun parser-add-oblig-features (cat feats)
  "embedded syntactic constituents must all have VAR, SEM, WH and GAP features defined"
  (mapcar #'(lambda (x)
              (if (not (assoc x feats))
		  ;; Myrosia 20050201 changed x to (gen-symbol x) to ensure unique names
		  (let ((v  (gen-v-num x)))
		    (setq feats (cons (list x (add-var v (make-var :name v)))
				    feats)))))
	  (case cat
	    ((w::PROP w::PRED w::DESCRIPTION) '(w::VAR w::SEM))
	    ((w::ROLES &) nil)
	    (otherwise '(w::GAP w::VAR w::SEM))))
  feats)


;;  This allows variables to be specified in three different forms 
;;           ?X,  (? X), or (? X Val1 ... Valn)

(defun isvar (expr)
  (or (and (symbolp expr)
           (equal (char (symbol-name expr) 0) #\?))
      (and (listp expr)	   
           (equal (util::convert-to-package (car expr) :parser) '?))))


;;  Embedded constituents are of form (% cat feat-val-list)
(defun isembeddedconstit (expr)
  (and (listp expr)
       (equal (util::convert-to-package (car expr) :parser) '%)))

;; conjunctions have a special syntax
(defun isconjunction (expr)
  (and (listp expr)
       (equal (util::convert-to-package (car expr) :parser) '&)))


;;   VAR-TABLE MAINTENANCE
;;  These functions maintain a binding list of variables when reading an expression. This is used to make sure
;;   that variables in the input get interpreted as the same variable structure.

(let ((var-table nil)
      (var-table-stack nil))

  (defun init-var-table nil
    (setq var-table nil))

   (defun get-var (x)
    (cadr (assoc x var-table :test #'equal)))
  
  (defun add-var (name var)
    (setq var-table (cons (list name var)
                          var-table))
    var)
  

  ;; push-var-table and pop-var-table added by Aaron for use in a
  ;; hierarchical lexicon, where each level in the hierarchy can push
  ;; new bindings onto the stack.

  (defun push-var-table ()
    (push var-table var-table-stack))
  
  (defun pop-var-table ()
    (setq var-table (pop var-table-stack)))
  
  (defun get-var-list ()
    (mapcar #'cadr var-table))

  ) ;; end scope of VAR-TABLE



;;  INSERTHEADFEATURES

;; inserts head features into a rule

(defun insertHeadFeatures (rule headfeatList)
  (let* ((mother (rule-lhs rule))
         (headfeats (cdr (assoc (constit-cat mother) headfeatList)))
         (rhs (rule-rhs rule))
         (head (findfirsthead rhs)))
    (cond 
     ;;  If there are no head features, just return the old rule
     ((null headfeats) rule)
     ;;  Otherwise, construct the feature-value pairs for the headfeats and insert them
     (t
      (cond ((null head)
	     (parser-warn "No head specified in rule ~s" rule)
	     rule)
	    (t
	     (Insertfeatures rule 
			     (remove-if #'null
					(mapcar #'(lambda (hf)
					 (BuildHeadFeat hf mother head rule))
				     headfeats)))))))))

             
;;  BUILDHEADFEAT builds a feature/value pair to insert in the mother and head
;;  We must check both the mother and head to see if these features already are
;;  defined
(defun BuildHeadFeat (headfeat mother head rule)
  (let ((mval (get-value mother headfeat))
        (hval (get-value head headfeat))
        (varname (gen-v-num headfeat))) ;;(gen-symbol headfeat)))
    (cond ((and (null mval) (null hval))
           (list headfeat (make-var :name varname)))
          ((null mval)
           (list headfeat hval))
	  ((or (null hval)
	       (equal mval hval))
           (list headfeat mval))
	  ((and (var-p mval) (var-p hval))
	   (let ((mvals (var-values mval))
		 (hvals (var-values hval)))
           (cond 
             ((null mvals)
              (list headfeat hval))
             ((Null hvals)
              (list headfeat mval))
             ((and (consp mvals) (consp hvals)
		   (intersection mvals hvals))
	      (list headfeat (make-var :name varname :values (intersection mvals hvals))))
	     ((and (constit-p mvals) (constit-p hvals)
                (match-head-constits headfeat mval hval)))
             (t (parser-warn "Head feature ~s incompatible in rule~%  ~s"
                     headfeat rule)))))
	  ((and (constit-p mval) (constit-p hval)
                (match-head-constits headfeat mval hval)))
          (t (parser-warn "Head feature ~s incompatible in rule~%  ~s"
                     headfeat rule)))))

(defun match-head-constits (headfeat mval hval)
  "unifies two constituents and returns the unified result"
  (let ((bndgs (match-vals headfeat mval hval)))
    (if bndgs (list headfeat mval))))

(defun findFirstHead (rhs)
  (cond ((null rhs) nil)
        ((constit-head (car rhs)) (car rhs))
        (t (findFirstHead (cdr rhs)))))

;;INSERTFEATURES builds the rule, inserts the feature-value pairs (values)
;;   into the mother and any consituent on the rhs marked as a head.

(defun insertfeatures (rule values)
  (let ((newvars (remove-nils (mapcar #'(lambda (x)
			     (let ((val (cadr x)))
			       (if (var-p val) val)))
			 values)))
	(mother (rule-lhs rule)))
   
    (make-rule :lhs (build-constit (constit-cat mother)
                                   (mergefeatures (constit-feats mother) values) nil)
               :id (rule-id rule)
               :rhs (mapcar #'(lambda (c)
				(if (constit-p c)
				    (if (constit-head c)
		 			(build-constit (constit-cat c)
						       (mergefeatures (constit-feats c) values)
						       t)
				      c)
				  c))
                            (rule-rhs rule))
	       :prob (rule-prob rule)
	       :var-list (union (rule-var-list rule) newvars)
	       :*-flag (rule-*-flag rule))
    ))

(defun remove-nils (ll)
  (cond ((null ll) nil)
	((null (car ll)) (remove-nils (cdr ll)))
	(t (util::reuse-cons (car ll) (remove-nils (cdr ll)) ll))))

;; MERGEFEATURES adds the feature-value pairs in feats to the constit,
;; It assumes that the feature value in newfeats is the one desired if
;; both present

(defun mergefeatures (oldfeats newfeats &optional results)
  (if (null oldfeats)
      (append-if-necessary results newfeats)
    (let* ((oldpair (car oldfeats))
	   (feat (car oldpair))
	   (newpair (assoc feat newfeats))
	   (pair (if newpair newpair (car oldfeats))))
      (mergefeatures (cdr oldfeats)
		     (removefeature feat newfeats)
		     (cons pair results)))))


;; REMOVEFEATURE returns a copy of a feature list with the feature named fname removed

(defun removefeature (fname flist)
 (remove-if #'(lambda (y) (eq fname (car y))) flist))

#||  ;; already defined in UTIL.lisp
;;  GEN-SYMBOL generates a unique identifier to identify a constituent

(defun gen-symbol (name)
  "This generates symbols used internally but we the user never needs to know about them"
  ;;  (gentemp (string name)))
  (gensym (string name)))
||#

(defvar *vnum* 0)
(defun gen-v-num (name)
  (declare (ignore name))
  (setq *vnum* (+ 1 *vnum*)))

(defun gen-reusable-symbol (name)
  "This generates symbols that can then be resused in a later parse"
  ;; will be rewritten
  (gentemp (string name)))

(defun gen-permanent-symbol (name)
  "These are permanetly unique symbols -- e.g., VAR names used in the LF"
    (gentemp (string name) *parser-package* ))


(let ((new-*-gensym-needed t)
      (new-**-gensym-needed t)
      (new-***-gensym-needed t)
      (current-*-gensym nil)
      (current-**-gensym nil)
      (current-***-gensym nil))
  
  (defun new-*-needed ()
    (setq new-*-gensym-needed t)
    (setq new-**-gensym-needed t)
    (setq new-***-gensym-needed t))
  
  (defun get-var-for-* nil
    (when new-*-gensym-needed
      (setq new-*-gensym-needed nil)
      (setq current-*-gensym (gen-permanent-symbol *symbol-prefix*)))
    current-*-gensym)

  (defun get-var-for-** nil
    (when new-**-gensym-needed
      (setq new-**-gensym-needed nil)
      (setq current-**-gensym (gen-permanent-symbol *symbol-prefix*)))
    current-**-gensym)

   (defun get-var-for-*** nil
    (when new-***-gensym-needed
      (setq new-***-gensym-needed nil)
      (setq current-***-gensym (gen-permanent-symbol *symbol-prefix*)))
    current-***-gensym)

  
  
  ) ;; end scope of NEW-GENSYM-NEEDED

;;===========
;;  FUNCTIONS FOR MANAGING VARIABLE BINDINGS IN RULES

;; Copy all vars in the rule, then substitute
;; these new values into the values of the binding list.

;;(defvar *copied-vars*)
;;(defvar *new-bndgs*)

(defun copy-vars-in-rule (rule bndgs)
  (multiple-value-bind (groundbndgs nongroundbndgs)
      (split-list #'variable-grounded bndgs)
    ;; we need to make new copies of the nongroundbndgs and the vars in the rule with no binding at all
    (let* ((copied-vars (make-copy-bndgs (remove-if #'(lambda (x) (assoc x groundbndgs)) (rule-var-list rule))))
	   (new-nongroundbndgs (make-modified-bndgs nongroundbndgs copied-vars))
	   (new-bndgs (append groundbndgs new-nongroundbndgs))
	   (lhs (rule-lhs rule))
	   (rhs (rule-rhs rule)))
      ;; replacing the * with a variable
    ;;(format t "~%~%Bindings have been modified")
   ;; (setq *COPIED-VARS* copied-vars)
    ;;(setq *new-bndgs* new-bndgs)
      (when (rule-*-flag rule)
	;;  Nore, need to redo this so we don'ty genrate symbols unless necessary
      #||(let ((newvar (gen-permanent-symbol 'V))
	    (newvar2 (gen-permanent-symbol 'V)))||#
	(new-*-needed)
	(setq lhs (replace-*-in-constit lhs))
	(setq rhs (mapcar #'(lambda (c) (replace-* c))
			  (rule-rhs rule)))
	(setq new-bndgs (replace-* new-bndgs )))
      (multiple-value-bind (newlhs vars-seen)
	  (subst-in1 lhs new-bndgs nil)
	(make-rule :lhs newlhs
		   :id (rule-id rule)
		   :rhs (subst-in1 rhs new-bndgs vars-seen)
		   :var-list (mapcar #'cadr copied-vars)
		   :*-flag (rule-*-flag rule))
      ))))

(defun variable-grounded (bndg)
  (no-var (second bndg)))

(defun no-var (val)
  (and val
       (or (symbolp val)
	   (and (var-p val) (no-var (var-values val)))
	   (and (listp val) (every #'symbolp val)))))

;;   MANAGING THE SPECIAL * FEATURE
;;  this replaces any "*" with a new gensym (the same one for all '* in the rule)

(defun replace-*-in-constit (c)
  (let ((feats (replace-* (constit-feats c))))
    (if (eq feats (constit-feats c))
	c
      (make-constit :cat (constit-cat c)
		    :feats feats
		    :head (constit-head c)))))

(defun replace-* (val)
  (cond ((eq val '*)
          (get-var-for-*))
	((eq val '**)
	 (get-var-for-**))
	((eq val '***)
	 (get-var-for-***))
        ((constit-p val)
         (replace-*-in-constit val))
        ((consp val)
         (util::reuse-cons (replace-* (car val))
			   (replace-* (cdr val))
			   val))
	((var-p val)
	 (if (null (var-values val))
	     val
	   (if (constit-p (var-values val))
	       (setf (var-values val) (replace-*-in-constit (var-values val)))
	     val)))
	     
         
        (t val)))


(defun make-copy-bndgs (vars)
  (mapcar #'(lambda (v)
	      (list v (make-var :name (gen-v-num (var-name v));;(gen-symbol (var-name v))
				:values (var-values v)
				:non-empty (var-non-empty v))))
	  vars))

(defun make-modified-bndgs (bndgs copied-vars)
   (add-non-used-vars copied-vars
		     (fix-bindings bndgs copied-vars nil)))
		   #||    (mapcar #'(lambda (x)
				   (List (Car x) (subst-in1 (cadr x) copied-vars)))
			       bndgs)))||#

;;  adds in any copied vars that are not in bndgs
(defun add-non-used-vars (vars bndgs)
  (cond ((null vars) bndgs)
	((assoc (caar vars) bndgs) (add-non-used-vars (cdr vars) bndgs))
	(t (cons (car vars) (add-non-used-vars (cdr vars) bndgs)))))

;; build-index takes a grammar in the internal format, and records
;; its rules in the index so they can be looked up efficiently.

(defun build-index (cg)
  (setf (cgrammar-index cg) (make-hash-table))
  (mapcar #'(lambda (rule)
	      (push rule
		    (gethash (constit-cat (car (rule-rhs rule)))
			     (cgrammar-index cg))))
	(append-if-necessary (reverse (cgrammar-rules cg)) (cgrammar-lex-rules cg))))

;;***************************************************************************
;;
;;   USER ACCESS TO THE GRAMMAR

(defun get-grammar ()
  (getGrammar))

(defun show-grammar (&rest ruleids)
  (if ruleids
    (mapcar #'%print (remove-if-not #'(lambda (rule)
                                        (member (rule-id rule) ruleids))
                                    (getGrammar)))
    ;;  otherwise print all the rules
    (mapcar #'%print (getGrammar)))
  (values))

(defun get-rules (ruleid)
  "Returns the rules defined for a specific rule id"
   (remove-if-not #'(lambda (rule)
                     (eq (rule-id rule) ruleid))
                 (getGrammar)))


(defun %print (obj)
  (Format t "~%~S" obj)
)
         
;;;  GRAMMAR SPECIFIC FEATURES


;; MAKE-GRAMMAR removes the old active grammar and creates a new one

(defun make-grammar (g)
  (if (null *default-grammar*)
	    (setq *default-grammar* (make-cgrammar)))
  "First, create a copy of the default-grammar with old grammar deleted"
  (let ((cg (copy-cgrammar *default-grammar*)))
    (setf (cgrammar-rules cg) nil)
    (setf (cgrammar-ids cg) nil)
    (setf (cgrammar-index cg) (make-hash-table))
    (setf (cgrammar-default-rule-prob cg) *default-rule-probability*)
    (select-grammar
      (compile-grammar g cg)))
  (values))
  
;;AUGMENT-GRAMMAR adds a new grammar onto the existing active grammar 
  
(defun augment-grammar (g)
  (select-grammar (compile-grammar g *default-grammar*))
  (values))

;; Access functions for worddefs from Lexicon Manager
                                                                                                 
;; Comes back as a (ID (words) prob constit) list.
                                                                                                 
(defun get-worddef-id (worddef) (first worddef))
(defun get-worddef-words (worddef) (second worddef))
(defun get-worddef-prob (worddef) (third worddef))
;; You need to call read-expression on this to make it useful.
(defun get-worddef-constit (worddef) (fourth worddef))

(defun lib-retrieve-from-lex (w feats)
  "a substitute function for retrieve-from-lex that calls the lexiconmanager package instead of using old Parser/Lexicon/Code
get word definition by calling get-word-def in lexiconmanager package, 
then convert the result  to the lex-entry format that the old parser code expects (i.e., what it used to get from retrieve-from-lex)"
  ;; get the word definition from the lexicon manager package and convert the symbols to the parser package
  (let* ((wdef 
	  (expand-with-alternatives-if-necessary
	   (remove-if #'null (lexiconmanager::get-word-def w (list* :var-prefix *symbol-prefix* feats)))
	   feats)))
    (if wdef ;; if we have a definition, create a lex-entry structure for the parser
	(cons 
	 (make-lex-entry
	  :constit (make-constit :cat 'w::word :feats `((w::lex ,w)))
	  :id (get-worddef-id (car wdef))
	  :prob lxm::*no-kr-probability*
	  )
	 ;; make lex-entry structures for every definition of w
	 (compress-lex-entries
	  (mapcar #'(lambda (entry) 
		      ;; if it's a multiple word entry from the lexicon (not from text tagger), tack on '(COMPOSITE (rest) for the old parser
		      ;; we know we need to do this if we got a list of words back from the LXM, but W is a single word.
		      (if (and (second (get-worddef-words entry)) (symbolp w))
			  (list 'composite (rest (get-worddef-words entry))
			       (make-lex-entry 
				:constit (read-expression (get-worddef-constit entry))
				:id (get-worddef-id entry)
				:prob (get-worddef-prob entry)))
		       ;; otherwise it's a basic entry -- just fill in the lex-entry structure
		       (let ((c (read-expression (get-worddef-constit entry))))
			 (make-lex-entry 
			  :constit c
			  :id (get-worddef-id entry)
			  :prob (adjust-lex-probability (get-worddef-prob entry) c)))));;(get-worddef-prob entry))))
		 wdef))
	 )
      ;; if there is no definition, try for a special entry here if there is special processing defined
      ;; this will be used for things like mixed formula input which are marked up in advance
      (and *special-entry-function* (funcall *special-entry-function* w))
      )
    )
  )

(defun is-a-placeholder (entry)
  (let* ((constit (fourth entry))
	(lf (cadr (assoc 'w::lf (cddr constit)))))
    (if (consp lf) 
	(member (second lf) '(ont::REFERENTIAL-SEM ont::situation-root))
	(member lf '(ont::REFERENTIAL-SEM ont::situation-root)))))
	
(defun expand-with-alternatives-if-necessary (entries feats)
  (if (every #'is-a-placeholder entries)
      (let* ((sense-info (car (find-arg feats :sense-info)))
	     (alternates (find-arg sense-info :alternate-spellings))
	     (orig-lex (caadar entries))) ; what about multi-words?
	(if alternates
	    (let* ((neww (remove-if #'null (lexiconmanager::get-word-def (car (tokenize (car alternates)))
									(list* :var-prefix *symbol-prefix* feats))))
		   (good-ones 
				      (remove-if #'is-a-placeholder neww)))
	      (if good-ones
		  ;; found a good one -- get one more for robustness
		  
		  (let* ((next-alt (if (cadr alternates)
				      (remove-if #'null 
						 (lexiconmanager::get-word-def (car (tokenize (cadr alternates)))
									       (list* :var-prefix *symbol-prefix* feats)))))
			(more-good-ones 
			 (remove-if #'is-a-placeholder next-alt)))
		    
			;(append (mapcar #'tweak-score (append good-ones more-good-ones))  entries))
			(append (mapcar #'(lambda (x) (add-orig-lex x orig-lex)) (mapcar #'tweak-score (append good-ones more-good-ones)))  entries))
		  ;; looks for more?
		  (look-at-other-alternatives entries (cdr alternates) feats)
		  ))
	    ;; no alternates
	    entries
	    ))
      ;; have some contentful entries so don't need more
      entries))

(defun tweak-score (c)
  "lowers score a hair as its coming from the spelling correction"
  (list* (car c) (cadr c) (* (third c) .99)
	 (cdddr c)))

(defun add-orig-lex (c orig-lex)
  "add a tag to indicate this is not the original spelling"
  (list* (car c) (cadr c) (third c) 
	 (append (cadddr c) (list (list 'w::orig-lex orig-lex)))
	 (cddddr c) ; in case there are more terms 
	 )
  )

(defun look-at-other-alternatives (entries alts feats)
  (if (null alts)
      entries
      (let* ((neww (remove-if #'null (lexiconmanager::get-word-def (car (tokenize (car alts)))
								  (list* :var-prefix *symbol-prefix* feats))))
	     (good-ones (remove-if #'is-a-placeholder neww)))
	(if good-ones
	    (append good-ones entries)
	    ;; looks for more?
	    (look-at-other-alternatives entries (cdr alts) feats)
	    ))
      ))

(defun adjust-lex-probability (prob entry)
  ;; generally we just use the prob returned from the lexicon, except when
  ;; its a tagged multiword that only has a SEM of REFERENTIAL-SEM
  (let* ((lf (get-value entry 'w::LF))
	 (lex (get-value entry 'w::lex))
	 (core-lf (if (consp lf) (second lf) lf)))
	(if (and (eq core-lf 'ont::referential-sem) (consp lex))
	    (* prob .96)
	    prob)))

(defvar *compress-lex-entries* nil)

(defun compress-lex-entries (entries)
  "remove duplicate entries"
  (if (and entries (cdr entries))
      (cons (car entries)
	    (compress-lex-entries (remove-if #'(lambda (x) (is-duplicate-entry x (car entries))) (cdr entries))))
      entries))

(defun is-duplicate-entry (x y)
  "returns t if lex entries X and Y are identical up to variable renaming"
  (when (and (lex-entry-p x) (lex-entry-p y))
	(let ((xconstit (lex-entry-constit x))
	      (yconstit (lex-entry-constit y)))
	  (and (eq (constit-cat xconstit) (constit-cat yconstit))
	       (fconstit-match (remove-feat (constit-feats xconstit) 'w::var)
			       (remove-feat (constit-feats yconstit) 'w::var))))))

 #|| (if *compress-lex-entries*
      (multiple-value-bind (preps others)
	  (find-prep-entries entries)
	(append (compress-preps preps) others))
      entries))

(defun compress-preps (preps)
  "This returns only the most abstract entries present in the entries"
  (let ((sortlist (mapcar #'(lambda (x) (cons (constit-cat (lex-entry-constit x)) x)) preps))
	(roots (remove-if #'null
			  (mapcar #'(lambda (x) (not (some #'(lambda (y) (if (not (eq x y))
									     (subtype-check x y))) sortlist))) sortlist))))
    ))||#
    
;;  rules to preprocess grammar to compute constituent cohorts for barriers (preferences from stat parser)

(defvar *cohorts* nil)
(defvar *additional-cats* nil)
;;(defvar *barrier-cats* nil)

(defun build-grammar-indices (cats)
  "Build structure used to optimize parsing - called every time grammar is reloaded"
  (setq *cohorts* nil)
  (Mapc #'build-cohorts-for-cat cats)
  ;; we've built the basic indices, not gather all the items
  (setf (barrier-cats *chart*) (mapcar #'start-end-closure cats))
  )

(defun build-cohorts-for-cat (cat)
  (multiple-value-bind (starts subs)
      (find-cohorts cat)
    (setq *cohorts*
	  (cons (list cat starts subs)
		*cohorts*))
    (mapc #'add-additional-cat (append starts subs))
    (do ()
	((null *additional-cats*))
      (let ((cs  *additional-cats*))
	(setq *additional-cats* nil)
	(mapc #'build-cohorts-for-cat cs)))))

(defun add-additional-cat (cat)
  (when (and (not (assoc cat *cohorts*))
	     (not (member cat *additional-cats*)))
    (push cat *additional-cats*)))
    
(defun find-cohorts (cat)
  (multiple-value-bind (starts durings)
      (get-cohorts-for-cat cat (get-grammar) nil nil)
    (values
     (remove-duplicates starts)
     (remove-duplicates durings)))) 

(defun get-cohorts-for-cat (cat rules starts durings)
  (if (null rules)
      (values starts durings)
      (let ((rule (car rules)))
	(if (eq (constit-cat (rule-lhs rule)) cat)
	    (let ((subs (get-sub-cats (rule-rhs rule))))
	      (if subs (get-cohorts-for-cat cat (cdr rules) (cons (car subs) starts)
				    (append subs durings))
		  (get-cohorts-for-cat cat (cdr rules) starts durings)))
	    (get-cohorts-for-cat cat (cdr rules) starts durings)))))

(defun get-sub-cats (rhs)
  (when rhs
    (let ((c (car rhs)))
      (cond ((var-p c) '(w::pp w::np w::cp))
	    ((and (constit-p c) (not (check-for-defined-predicate (constit-cat c))))
	     (cond ((symbolp (constit-cat c))
		    (cons (constit-cat c) (get-sub-cats (cdr rhs))))
		   ((var-p c)
		    (append (var-values c)  (get-sub-cats (cdr rhs))))
		   (t  (get-sub-cats (cdr rhs)))))
	    (t (get-sub-cats (cdr rhs)))))))
	   
(defvar *already-seen-cats* nil)

(defun start-end-closure (cat)
  "using the *cohorts* table, we gather all the possible starts and subs for the indicated cat"
  (let* ((cohorts (assoc cat *cohorts*))
	 (starts (cadr cohorts))
	 (subs (caddr cohorts)))
    (list cat (remove-duplicates (expand-starts starts)) (remove-duplicates (expand-subs subs)))))

(defun expand-starts (cs)
  (setq *already-seen-cats* nil)
  (mapcan #'expand-start cs))

(defun expand-start (cat)
  (if (member cat *already-seen-cats*)
      (progn
	(list))
      (let* ((starts (cadr (assoc cat *cohorts*))))
	(push cat *already-seen-cats*)
	(copy-list (union starts
			  (mapcan #'expand-start starts))))))

(defun expand-subs (cs)
  (setq *already-seen-cats* nil)
  (mapcan #'expand-sub cs))

(defun expand-sub (cat)
  (if (member cat *already-seen-cats*)
      (progn
	(list))
      (let* ((subs (caddr (assoc cat *cohorts*))))
	(push cat *already-seen-cats*)
	(copy-list (union subs
			  (mapcan #'expand-sub subs))))))
	  	  
