(in-package "PARSER")

;;   This file contains the functions that manage the features in constituents,
;;   primarily constituent matching and unification

;; (defconstant *success* '((NIL NIL))) ;; moved to structures.lisp

;;============================================================================
;;   MANAGING CONSTITUENTS

;;   A constituent is represented as a list of form 
;;        ((<feature> <value>) ... (<feature> <value>))
;;   where a <value> may be
;;       an atom
;;       a variable
;;       a constrained variable, restricted to one of a list of values

;;  FOOT Features

(let ((footFeatures nil))
  (defun define-foot-feature (f)
    (if (not (member f footFeatures))
	(setq footFeatures (cons f footFeatures))
      ))
  
  (defun get-foot-features nil
    footFeatures)
  
  (defun init-foot-features nil
    (setq footFeatures nil)))


;; Make a constituent of the indicated category with the indicated features

#||(defun Build-constit-in-rule (cat feats lfg head)
  "This checks for control features like OPTIONAL, which are removed"
  (let ((f (assoc 'w::optional feats)))
    (if f
	(make-constit :cat cat
                  :feats (remove-if #'(lambda (x) (eq (car x) 'w::optional)) feats)
                  :head head
                  :optional (if (eq (second f) '+) t))
    (make-constit :cat cat :feats feats :head head))))||#


(defun Build-constit-in-rule (cat feats lfg head rule)
  "This checks for control features like OPTIONAL, which are removed"
  (let* ((f (assoc 'w::optional feats))
	(fs (if f (remove-if #'(lambda (x) (eq (car x) 'w::optional)) feats)
		feats)))
    (make-constit :cat cat
		  :feats (if lfg
			     (cons (build-lfg-spec lfg rule) fs)
			     fs)
		  :head head
		  :optional (if (eq (second f) '+) t))
    ))

(defun build-lfg-spec (lfgspec rule)
  (setq lfgspec (cadr lfgspec)) ;; maybe temporary as rule format may change
  (let ((root (find-arg-in-act lfgspec :root))
	(nodes (find-arg-in-act lfgspec :nodes))
	(arcs (find-arg-in-act lfgspec :arcs))
	(arg (find-arg-in-act lfgspec :arg))
	)
    (when (not (or (null root)
		 (symbolp root)
		 (var-p root)))
	(format t "~%Warning: in rule ~S, a root must be a constant or variable. It was ~S" rule root)
	(setq root nil))
    (when (not (or (null nodes)
		 (every #'(lambda (x) (and (consp x) (eq (car x) 'w::node))) nodes)))
	(format t "~%Warning: in rule ~S, ill formed node in ~S" rule nodes)
	(setq nodes nil))
    (read-value lfgspec rule)
    ))
    

(defun build-constit (cat feats head)
  (make-constit :cat cat 
                :feats feats
                :head head
		))
   


;; Add a new feature-value pair to an existing constituent
;; DESTRUCTIVELY Add a new feature-value pair to an existing constituent

(defun add-feature-value (constit feat val)
  (setf (constit-feats constit)
              (cons (list feat val) (constit-feats constit)))
  constit)

(defun add-feature-value-to-copy (constit feat val)
  (make-constit :cat (constit-cat constit)
		:feats (cons (list feat val) (constit-feats constit))
		:head (constit-head constit)))



;;   add a set of features into a constit, unless they are there already

(defun Merge-feature-values (constit fvlist)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (null fvlist) constit
    (let ((newfvlist nil)
	  (failure-flag nil)
	  (bndgs nil))
      (mapcar #'(lambda (x)
		  (let* ((newbndgs nil)
			 (feat (car x))
			 (newval (cadr x))
			 (oldval (get-value constit feat)))
		    (cond ((null oldval) ;;(eq oldval '-)
			   (setq newfvlist (cons x newfvlist)))
			  ;;  Otherwise, see if values match. If so, either update binding
			  ;;  list and add new value unless oldval was a constant
			  ((setq newbndgs (match-vals feat oldval newval))
			   (setq bndgs (append-if-necessary newbndgs bndgs))
			   (if (not (var-p newval)) (setq newfvlist (cons x newfvlist))))
			  ((not (same-value newval oldval))
			   (setq failure-flag t)))))
			  ;;(parser-warn "Inconsistent foot feature in constit ~S. Features are ~S" constit fvlist)
	      fvlist)
      (if (not failure-flag)
	  (values-list 
	   (list (build-constit (constit-cat constit) 
			 (append-if-necessary (constit-feats constit) newfvlist)
			 (constit-head constit))
		 bndgs))
	  (progn
	    (trace-msg 1 "~% Failure in merging foot features ~S. Constit is ~S" fvlist constit)
	    nil)))))

(defun same-value (x y)
  (or (equal x y)
      (and (constit-p x) (constit-p y)
	   (fconstit-match (constit-feats x) (constit-feats y)))))

;; Modified by AK to work on cat feature.
(defun replace-feature-value (constit feat val)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (eq feat 'cat)
      (build-constit val
		     (constit-feats constit)
		     (constit-head constit))
    (if (null (get-fvalue (constit-feats constit) feat))
	(add-feature-value constit feat val)
      (build-constit (constit-cat constit)
		     (replace-feat (constit-feats constit) feat val)
		     (constit-head constit)))))

(defun replace-feat (feats feat val)
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond ((null feats) (list (list feat val)))
        ((eq (caar feats) feat) (cons (list feat val) (cdr feats)))
        (t (reuse-cons (car feats) (replace-feat (cdr feats) feat val) feats))))

(defun remove-feat (feats feat)
  (remove-if #'(lambda (x) (eq (car x) feat))
             feats))

;;  Get the value of a specific feature from a constituent

(defun get-value (constit feature)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (eq feature 'cat)
    (constit-cat constit)
    (get-fvalue (constit-feats constit) feature)))

;;;; This gets the value from a feature-value list
;;;(defun get-fvalue (featlist feature)
;;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
;;;;;;;;; Myrosia: use equal as a test when feature is a list starting with :*
;;;  (if (symbolp feature)
;;;      (cadr (assoc feature featlist))
;;;    (if (and (consp feature) (eq (first feature) ':*))
;;;	(cadr (assoc feature featlist :test #'equal))
;;;      
;;;      ))
;;;  )

; This gets the value from a feature-value list
(defun get-fvalue (featlist feature)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (symbolp feature)
    (cadr (assoc feature featlist))
    (cadr (assoc feature featlist :test #'equal))
  ))

;;===========================================
;;  VARIABLES

;; Construct a new variable with the indicated name, and possible values
;;  This maintains lists of already constructed variables for reuse.

(defun build-var (name values &optional non-empty)
 
  (when (and values
	   (consp values)
           (> (length values) 1)
           (some #'(lambda (v)
		(not (or (symbolp v) (numberp v))))
	    values))
      (parser-warn  "Illegal variable value in ~S~%   Complex values such as ~S are not supported in variable multiple value lists~%   Use a separate rules for such values" 
		    (cons '?
			 (cons name 
			       values))
		   (find-if #'(lambda (v)
				(not (symbolp v)))
			    values))
      (setq values (car values)))
  
    (make-var :name name :values values :non-empty non-empty))

;;*************************************************************************************                 
;;*************************************************************************************                 
    
    
;;  MANAGING THE GAP FEATURE


(defvar *default-gap-cat-feat-lists* '((w::NP w::VAR w::AGR w::SEM w::CASE)))
                
(defun gapsDisabled (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (not (cgrammar-gapsEnabled cg)))

(defun gapsEnabled (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (cgrammar-gapsEnabled cg))

(defun disableGaps (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (setf (cgrammar-gapsEnabled cg) nil)
  cg)

(defun enableGaps  (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (setf (cgrammar-gapsEnabled cg) t)
  (if (null (cgrammar-gap-cat-feat-lists cg))
      (setf (cgrammar-gap-cat-feat-lists cg)
	*default-gap-cat-feat-lists*))
  cg)
  
(defun declare-gap-cat (cat feats &optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (setf (cgrammar-gap-cat-feat-lists cg)
    (cons (cons cat feats)
	  (remove-if #'(lambda (x) (eq (car x) cat)) 
		     (cgrammar-gap-cat-feat-lists cg))))
  cg)

(defun reset-gap-cats  (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (setf (cgrammar-gap-cat-feat-lists cg) nil)
  cg)

(defun get-gap-cats  (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (mapcar #'first (cgrammar-gap-cat-feat-lists cg)))

  
(defun get-gap-feat-list (cat  &optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (cdr (assoc cat (cgrammar-gap-cat-feat-lists cg))))


;;********************************************************************************
;;   CODE TO INSERT GAP FEATURES INTO GRAMMAR
;;

;;  This is the main function. It generates the GAP features into the rules as described
;;   in Chapter 5. It returns a list of modified rules, since there may be more than
;;   one gap rule generated from a single original rule.

(defun generate-gap-features-in-rule (rule)
  (if 
    ;; If the rule explicitly sets the GAP feature, then it is left alone
    ;; Rules with lexical lhs also do not have gap features
    (or (gap-defined-already rule)
        (lexicalConstit (rule-lhs rule)))
    (list rule) 
    ;; Otherwise, break up the rule and analyse it
    (let* ((rhs (rule-rhs rule))
           (head (findfirsthead rhs))
           (numbNonLex (count-if #'nonLexicalConstit  rhs)))
      (cond
       ;; If no nonlexical subconsitutents, then no GAP possible
       ((<= numbNonLex 0) (list (make-rule :lhs (add-feature-value (rule-lhs rule) 'w::GAP '-)
					   :id (rule-id rule)
					   :rhs (rule-rhs rule)
					   :prob (rule-prob rule)
					   :var-list (rule-var-list rule)
					   :*-flag (rule-*-flag rule))))

       ;;  If head is a lexical category, propagate GAP to each non-lexical subconstituent
       ((or (null head)
	    (lexicalConstit head))
        (gen-rule-each-NonLex rule numbNonLex))

       ;;  If non-lexical head, set up GAP as a head feature
       (t (let ((var (make-var :name (gen-v-num 'g)))) ;;(gen-symbol 'g))))
            (list (make-rule :lhs (add-feature-value (rule-lhs rule) 'w::GAP var)
                             :id (rule-id rule)
                             :rhs (add-gap-to-heads rhs var)
			     :prob (rule-prob rule)
			     :var-list (cons var (rule-var-list rule))
			     :*-flag (rule-*-flag rule)))
			     ))))))

;; This returns true if the rule already specifies the GAP feature

(defun gap-defined-already (rule)
  (cond ((get-value (rule-lhs rule) 'w::gap) t)
        (t (find-gap-in-rhs (rule-rhs rule)))))

(defun find-gap-in-rhs (rhs)
  (cond ((null rhs) nil)
        ((get-value (car rhs) 'w::gap) t)
        (t (find-gap-in-rhs (cdr rhs)))))

;;  This adds the gap to every head subconstituent marked as a head

(defun add-gap-to-heads (rhs val)
  (if (null rhs) nil
      (let ((firstc (car rhs)))
        (if (constit-head firstc)
          (cons (add-feature-value firstc 'w::GAP val)
                (add-gap-to-heads (cdr rhs) val))
          (cons (add-feature-value firstc 'w::GAP '-) 
                (add-gap-to-heads (cdr rhs) val))))))


;; This generates a new rule for each non-lexical subconstituent
;;   n is the number of non-lexical subconstituents

(defun gen-rule-each-NonLex (rule n)
  (let ((var (make-var :name (gen-v-num'!g) ;;(gen-symbol '!g) 
		       :non-empty t)))
    (if (< n 0) nil
      (cons 
       (make-rule :lhs (add-feature-value-to-copy (rule-lhs rule) 'w::GAP (if (= n 0) '- var))
		  :id (rule-id rule) 
		  :rhs (insert-gap-features var n (rule-rhs rule))
		  :prob (rule-prob rule)
		  :var-list (rule-var-list rule)
		  :*-flag (rule-*-flag rule))
       (gen-rule-each-NonLex rule (- n 1))))))
      

          
;;  inserts the GAP var in the n'th non-lexical consituent, and - in the others
;;  note, if called with n=0, it just inserts (GAP -) in each subconstituent

(defun insert-gap-features (val n rhs)
  (if (null rhs) nil
    (mapcar #'(lambda (c)
                  (cond ((not (lexicalConstit c))
                         (setq n (1- n))
                         (if (= n 0)
                          (add-feature-value-to-copy c 'w::GAP val)
                          (add-feature-value-to-copy c 'w::GAP '-)))
                        (t c)))
              rhs)))
  
;;*****************************************************************************************
;;  FUNCTIONS USED BY THE PARSER
    
;; This checks to see if the GAP value of the next constituent could be satisfied
;;    the the next constituent. If so, it extends the arc appropriately


(defun insert-gap (arc cg)
  (let* ((next (car (arc-post arc)))
         (nextcat (constit-cat next))
         (gapvalue (get-value next 'w::gap)))
    ;; first check some basic stuff to see if its worth doing the real check
    (if (and (not (eq gapvalue '-))
	     (not (null gapvalue))
	     (member nextcat (get-gap-cats)))
	;;  There are two cases where we insert a gap:
	;;   Case 1: the GAP feature is a constituent that matches the next constit,
	;;   Case 2: the GAP feature is a variable and the cat of next is NOT the same as the 
	;;           cat of the mother, since that would create a constituent of form X/X
	(let ((gap
	       (if (constit-p gapvalue)
		   (if (constit-match gapvalue next) 
		       (make-gap-entry gapvalue arc cg))
		 (if (and (var-p gapvalue)
			  (not (eq (constit-cat (arc-mother arc)) nextcat)))
		     (make-gap-entry next arc cg)))))
	  (when gap
	    (put-in-chart gap)
	    (add-to-agenda (make-agenda-item :type 'arc :entry gap :id (entry-name gap)
					     :arc arc
					     :bndgs (if (constit-p gapvalue) ; compute the binding list for the gap
							(constit-match gapvalue next)
						      (list (list gapvalue (entry-constit gap))))
					     :start (arc-start arc) :end (arc-end arc)))
	    (trace-msg 3 "~%Inserting ~S at position ~S to fill a gap~%" gap (arc-end arc))
	    )
	  T				; return T so that normal arc extension is not tried
	  ))))
    
;; Takes a constituent as a template an generates a GAP constituent that
;;  would satisfy it. The value of the GAP feature is a copy of the constituent
    
(defun make-gap-entry (constit arc cg)
  (let* ((cat (constit-cat constit))
        (feats (gen-feats-for-gap cat (constit-feats constit) cg)))
    (make-entry :constit (make-constit 
                          :cat cat
                          ;;  set GAP feature, add +EMPTY
                          :feats (cons '(empty +)
				       (cons `(gapid ,(gen-v-num 'G-id)) ;;(gen-symbol 'G-id)) ;; This makes each inserted gap constituent unique in chart
					     (cons (list 'w::gap (make-constit :cat cat :feats feats))
						   feats))
				       ))
		
    
                :start (arc-end arc) 
                :end (arc-end arc) 
                :rhs nil
                :name (gen-v-num 'gap) ;;(gen-symbol 'GAP)
                :rule-id (if (eq (constit-cat constit) 'w::NP) 'NP-GAP-INTRO 'GAP-INTRO)
                :prob 1
		:first-cat (list 'w::gap)
		:size 1)))
              
;;  Remove GAP feature, and add all features in GAP-FEAT-LIST
;;  that are not currently defined

(defun gen-feats-for-gap (cat feats cg)
  (let ((feats (remove-if #'(lambda (x) (eq (car x) 'w::gap)) feats)))
    (mapcar #'(lambda (f)
                (if (not (get-fvalue feats f))
                  (setq feats (cons (list f (make-var :name (gen-v-num f))) ;;(gen-symbol f)))
                                    feats))))
            (get-gap-feat-list cat cg))
    feats))


         
;; *****************************************************************************************
;; ****************************************************************************************
;;    HANDLING THE SEM FEATURE

(defun semEnabled  (&optional cg)
    (if (null cg)
	(setq cg *default-grammar*))
    (cgrammar-SemEnabled cg))

(defun noSemEnabled (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (not (cgrammar-SemEnabled cg)))
  
(defun enableSem (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (setf (cgrammar-SemEnabled cg) t))
  
(defun disableSem (&optional cg)
  (if (null cg)
      (setq cg *default-grammar*))
  (setf (cgrammar-SemEnabled cg) nil))

;; obsolete::If semantic interpretation is enabled, a discourse variable must be
;;     created for the VAR feature
(defun instantiateVAR (constit)
  "This creats a new constant to be the unique value of the VAR feature. We also add the HEADCAT
    feature here, which is used in Collins probability functions"
  (setf (constit-feats constit)
	(append (constit-feats constit) (list (list 'w::headcat (constit-cat constit)))))
  (if (var-p (get-value constit 'w::VAR))
    (subst-in constit (list (list (get-value constit 'w::VAR) (gen-permanent-symbol 'V))))
    constit)
  )
