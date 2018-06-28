(in-package "PARSER")

;;  The UNIFICATION CODE

;;(defvar *vars-seen* nil)
(defvar *sem-match-failed* nil)
(defvar *sem-failure-penalty* .9)
(defvar *ignore-binding-loops* t)

;; ============================================
;; TOP-LEVEL FUNCTIONS

;; The following provide the top-level API to the unification facility
;;  Due to the fact that unificaiton uses destructive modification of variables
;;  You should only access the unification via these calls

;; Matches two constituents and returns the binding list if they can
;; be unified.  The first constituent must be a pattern from a rule,
;; and the second must be an actual constituent from the chart.
(defun constit-match (pattern constit)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (ans prob undos)
      (constit-match1 pattern constit nil)
    (if (and  ans (not (eq ans *success*)) (null undos))
	(break "something seems fishy here: ans is ~S  undos are ~S" ans undos))
    (undo-bindings undos)
    (values (fix-bindings ans ans nil) prob)))

;;  FCONSTIT-MATCH unifies two feature lists and returns the binding list

(defun fconstit-match (fpattern fconstit)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (ans prob undos)
      (fconstit-match1 fpattern fconstit nil)
    (undo-bindings undos)
    (values ans prob)))

;; Matches two arbitrary values and returns the binding list if they can be
;; unified.  The first value must come from a pattern in a rule, and
;; the second must be taken from an actual constituent on the chart.

(defun match-with-subtyping (p c)
  "This function is convenient for calls that use type hierarchy unification"
  (match-vals 'w::sem p c))

(defun match-vals (feat patternval constitval)
  (declare (optimize (speed 3) (debug 0)))
  (multiple-value-bind (ans prob undos)
      (match-vals1 feat patternval constitval nil)
    (undo-bindings undos)
    (values ans prob)))

;; SUBST-IN FUNCTION
;;  Given a list of bindings, instantiates the variables in the expression
;;  This is used to instantiate constituents and rules.


(defun subst-in (x bndgs)
 #|| (let ((*vars-seen* nil)
	)||#
    (subst-in1 x bndgs nil))


;;;=============================================================================
;; INTERNAL CODE - do not call these functions directly, use the top-level functions

;; substitution - variable binding

(defun subst-in1 (x bndgs vars-seen)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond
   ((or (symbolp x)
        (numberp x)
	(stringp x))
    x)
   ((or (null bndgs)
        ;;         (equal bndgs '((nil nil)))
	(eq bndgs *success*))
        #||(and (consp bndgs)
             (consp (car bndgs))
             (endp (cdr bndgs))
             (null (caar bndgs))
             (null (cadar bndgs))
             (endp (cddar bndgs))))     ; speed||#
    x)
   ((consp x)
    (subst-in-list x bndgs vars-seen))
   ((var-p x)
    (subst-in-var x bndgs vars-seen))
   ((eq x *empty-constit*)
    *empty-constit*)
   ((constit-p x)
    (multiple-value-bind 
	  (newcat new-vars-seen)
	(subst-in1 (constit-cat x) bndgs vars-seen)
      (multiple-value-bind 
	  (newfeats newer-vars-seen)
	  (subst-in1 (constit-feats x) bndgs new-vars-seen)
	(if (and (eq newcat (constit-cat x)) (eq newfeats (constit-feats x)))
	    (values x newer-vars-seen)
	    (values (make-constit :cat newcat
				  :feats newfeats
				  :head (constit-head x)
				  :optional (constit-optional x))
		    newer-vars-seen)))))
   ((entry-p x)
    (multiple-value-bind
	  (newentry new-vars-seen)
	(subst-in1 (entry-constit x) bndgs vars-seen)
      
      (if (eq newentry (entry-constit x))
	  (values x new-vars-seen)
	  (multiple-value-bind 
	    (rhs newer-vars-seen)
	      (subst-in-list (entry-rhs x) bndgs new-vars-seen)
	    (values (make-entry :constit newentry ;;(subst-in1 (entry-constit x) bndgs)
				:start (entry-start x)
				:end (entry-end x)
				:rhs rhs
				:name (entry-name x)
				:rule-id (entry-rule-id x)
				:prob (entry-prob x)
				:prob-aux (entry-prob-aux x)
				:first-cat (entry-first-cat x)
				:size (entry-size x))
		  newer-vars-seen)))))
   ((arc-p x)
    (multiple-value-bind (mother new-vars-seen)
	(subst-in1 (arc-mother x) bndgs vars-seen)
      (multiple-value-bind (pre newer-vars-seen)
	  (subst-in1 (arc-pre x) bndgs new-vars-seen)
	(multiple-value-bind (post newer-vars-seen1)
	    (subst-in1 (arc-post x) bndgs newer-vars-seen)
	  (multiple-value-bind (local-vars newer-vars-seen2)
	      (subst-in1 (arc-local-vars x) bndgs newer-vars-seen1)
	    (multiple-value-bind (foot-feats newer-vars-seen3)
		(subst-in1 (arc-foot-feats x) bndgs newer-vars-seen2)
	      (if (and (eq mother (arc-mother x))
		       (eq pre (arc-pre x))
		       (eq post (arc-post x))
		       (eq local-vars (arc-local-vars x))
		       (eq foot-feats (arc-foot-feats x)))
		  (values x newer-vars-seen3)
		  (values (make-arc :mother mother
				    :pre pre
				    :post post
				    :start (arc-start x)
				    :end (arc-end x)
				    :rule-id (arc-rule-id x)
				    :prob (arc-prob x)
				    :prob-aux (arc-prob-aux x)
				    :local-vars local-vars
				    :foot-feats foot-feats
				    :first-cat (arc-first-cat x)) newer-vars-seen3))))))))
   ((rule-p x)
    (multiple-value-bind (lhs new-vars-seen)
	(subst-in1 (rule-lhs x) bndgs vars-seen)
      (multiple-value-bind (rhs newer-vars-seen)
	  (subst-in1 (rule-rhs x) bndgs new-vars-seen)
	(multiple-value-bind (var-list newest-vars-seen)
	    (subst-in (rule-var-list x) bndgs)
	  (values 
	   (make-rule :lhs lhs
		      :rhs rhs
		      :id (rule-id x)
		      :prob (rule-prob x)
		      :var-list var-list
		      :*-flag (rule-*-flag x)))))))
   ((arrayp x)   ;; a SEM array
    (subst-in-sem x bndgs vars-seen))
   (t
    (values x vars-seen))))

(defun subst-in-list (x bndgs vars-seen)
  (if x
    (multiple-value-bind (elem new-vars-seen)
	(subst-in1 (car x) bndgs vars-seen)
      (multiple-value-bind (rest newer-vars-seen)
	  (subst-in-list (cdr x) bndgs new-vars-seen)
	(if (and (eq elem (car x)) (eq rest (cdr x)))
	    (values x newer-vars-seen)
	    (values (cons elem rest) newer-vars-seen))))
    (values nil vars-seen)))

(defun subst-in-var (x bndgs vars-seen)
  "If a variable has a value with variables in it, they need to be SUBST-IN'd as well. but only
     once for each individual variable"
  ;;(format t "~%Substituting into VAR ~S" x)
  (let ((val (assoc x vars-seen))) ;; seen before, use value computed before
    (if val
	(values (cdr val) vars-seen)
	(let ((v (get-most-specific-binding x bndgs vars-seen)))
	  (if (not (eq v x)) 
	      ;; check new binding
	      (if (var-p v)
		  (let ((v1 (assoc v vars-seen)))
		    (if v1
			(values (cdr v1) vars-seen)
			(subst-in-var-values v bndgs vars-seen)))
		  (values v vars-seen))
	      (subst-in-var-values x bndgs vars-seen))))))

(defun subst-in-var-values (var bndgs vars-seen)
  "substitutes with the value of a variable"
  (if (var-p var)
      (if (var-values var)
	  (if (no-var (var-values var))
	      (values var vars-seen)
	      (multiple-value-bind (vals new-vars-seen)
		  (subst-in1 (var-values var) bndgs vars-seen)
		(let ((newv (make-var :name (var-name var)
				      :values vals
				      :non-empty (var-non-empty var))))
		  (values newv (append (list (cons var newv) new-vars-seen))))))
	  ;; empty var values
	  (values var vars-seen))
      ;;no a variable - should be an error
      (break "SUBST-IN_VAR-VALUES called with non-var ~S" var)))
#||
	    (let ((vv
		   (cond
		    ((not (var-p v))
		     (subst-in1 v bndgs))
		    ((null (var-values v)) v)
		    ((no-var (var-values v)) v)
		    (t (make-var :name (var-name v)
				 :values (subst-in1 (var-values v) bndgs)
				 :non-empty (var-non-empty v))))))
	      (setq *vars-seen* (append (list (cons x vv) (cons vv vv)) *vars-seen*))
	      (if (not (eq v x)) (setq *vars-seen* (cons (cons v vv) *vars-seen*)))
	     
	      vv)))))||#

(defun subst-in-sem (sem bndgs vars-seen)
  ;;  see if any changes need to be made first before making a new array
  (let ((changes nil)
	(current-vars-seen vars-seen))
    (dotimes (i *sem-size*)
      (multiple-value-bind (newv new-vars-seen)
	  (subst-in1 (aref sem i) bndgs current-vars-seen)
	(setq current-vars-seen new-vars-seen)
	(if (not (eq newv (aref sem i)))
	    (setq changes (cons (cons i newv) changes)))))
    (if changes
      ;; need to create a new array
      (let ((result (make-array *sem-size*)))
	(dotimes (i *sem-size*)
	  (setf (aref result i) (or (cdr (assoc i changes))
				    (aref sem i))))
       (values result current-vars-seen))
      (values sem current-vars-seen))))

;;  this substitutes var bindings into values to ground out
;;   the bindings as much as possible
(defun fix-bindings (bndgs orig-bndgs vars-seen)
  "Special SUBST-IN operation to instantiate the values of variables as much as possible.
    We only need the vars-seen going in, we do not need to return it"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;;(trace subst-in1 subst-in-var subst-in-list)
  (when bndgs
    (let* ((first (car bndgs))
	   (firstvar (car first))
	   (firstval (cadr first)))
      (multiple-value-bind (newval new-vars-seen)
	  (subst-in1 firstval orig-bndgs vars-seen)
	(let ((rest (fix-bindings (cdr bndgs) orig-bndgs new-vars-seen)))
	  (if (and (eq firstval newval)
		   (eq rest (cdr bndgs)))  ;; EQ will work as we are checking if the list is unchanged
	      bndgs
	      (cons (list firstvar firstval) rest)))))))
  
(defun get-most-specific-binding (var bndgs vars-seen)
  "Returns the most specific binding of the value of a var. We need vars-seen to do substitute within complex values"
  (gmsb var bndgs nil vars-seen)
)

(defun gmsb (var bndgs seen vars-seen)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (member var seen)
      (if *ignore-binding-loops*
	  nil
	(break "VAR loop found!: ~S the loop is ~S ~%    binding list: ~S " var seen bndgs))
      (let ((val (cadr (assoc var bndgs))))
	(if val
	    (if (var-p val)
		;; if its a var, then see if that var is bound
		(let ((val2 (gmsb val bndgs (cons var seen) vars-seen)))
		  (if val2 val2 val))
		;; otherwise, it might contain vars that need binding
		(subst-in1 val bndgs vars-seen))
	    ;; it wasn't bound in bndgs
	    var))))

(defun undo-bindings (undos)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mapc #'(lambda (vv)
	    (if (var-p (car vv))
		(setf (var-values (car vv)) (cdr vv))
		(break)))
	undos))

(defun replace-vars (bndg-list undos)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
   (if (eq bndg-list *success*)
      undos
      (let ((newundos (mapcar #'(lambda (x) (cons (car x) (var-values (car x)))) bndg-list)))
	(mapc #'(lambda (vv)
		  (let ((var (car vv)))
		    (when var
		      (if (var-p var)
			  (setf (var-values var) (second vv))
			  (break)))))
	      bndg-list)
	;; return the new UNDO list
	(append newundos undos))
      ))

;;   AUXILIARY FUNCTIONS FOR UNIFICATION

(defun constit-match1 (pattern constit undos)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (and (constit-p pattern) (constit-p constit))
      (multiple-value-bind (bndgs prob catundos)
	  (match-vals1 'cat (constit-cat pattern) (constit-cat constit) undos)
	(if bndgs
	  (multiple-value-bind (result prob newundos)
	      (fconstit-match1 (constit-feats pattern) (constit-feats constit) catundos)
	    (if result
		(if (equal bndgs *success*)
		    (values result prob newundos)
		    (values (add-to-binding-list bndgs result) prob newundos)) ;;(replace-vars result newundos))))
		(values nil nil newundos)))
	  (values nil nil catundos)))
      (break "Error: constit-match1 called with a non-constit: ~s and ~S" pattern constit)
      ))

(defun fconstit-match1 (fpattern fconstit undos)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (null fpattern) (values *success* nil undos)
      (let* ((featval (caar fpattern))
	     (feat (if (var-p featval)
		       (chase-down-var featval)
		       featval))
	     (val (cadar fpattern))
	     (cval (get-fvalue fconstit feat)))
	(if (eq feat '-)
	    (fconstit-match1 (cdr fpattern) fconstit undos)
	    (multiple-value-bind (bndgs prob newundos)
		(match-vals1 feat val cval undos)
	      (if bndgs
		  (multiple-value-bind (result prob2 newerundos)
		      (fconstit-match1 (cdr fpattern) fconstit newundos) ;;(replace-vars bndgs newundos))
		    (if result
			(if (equal bndgs *success*) 
			    (values result (combine-probs prob prob2) newerundos)
			    (values (add-to-binding-list bndgs result) (combine-probs prob prob2) newerundos)) ;; don't need replace-vars here as the last recursive call will have done it 
			  (values nil nil newerundos)))
		  (values nil nil newundos))))
	    )))


;;   recursively matches each element down the list, substituting for
;;    variables as it goes

(defun match-lists (feat val cval undos)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond
    ((and (null val)
	  (null cval))
     (values *success* nil undos))
    ((null val)
     (if FAILURE-TRACE (failure-message val cval feat))
     (values nil nil undos))
    (t
     (multiple-value-bind (bndgs prob newundos)
	 (match-vals1 feat (car val) (car cval) undos)
       (if bndgs
	   (multiple-value-bind (bndgs2 prob2 newerundos)
	       (match-lists feat (cdr val) (cdr cval) newundos) ;;(replace-vars bndgs newundos))
	     (if bndgs2
		 (cond
		   ((equal bndgs2 *success*)
		    (values bndgs (combine-probs prob prob2) newerundos))
		   (t
		    (values (add-to-binding-list bndgs bndgs2) (combine-probs prob prob2) newerundos)))
		 (progn
		   (if FAILURE-TRACE (failure-message val cval feat))
		   (values nil nil newerundos))))
	   (progn
	     (if FAILURE-TRACE (failure-message val cval feat))
	     (values nil nil newundos)))))
    ))

(defun combine-probs (p1 p2)
  (cond ((null p1) p2)
	((null p2) p1)
	(t (* p1 p2))))


   
(defun basic-expr (x)
  (or (symbolp x) (and (consp x) (eq (car x) :*)
		       (every #'(lambda (y) (not (var-p y))) x))))

(defun match-vals1 (feat patternval constitval undos)
  (declare (optimize (speed 3) (debug 0)))
  (if (null constitval) (setq constitval '-))     ;; Use - as the default
  (if (null patternval) (setq patternval '-))
  (if (var-p constitval)
    (setq constitval (chase-down-var constitval)))
  (if (var-p patternval)
    (setq patternval (chase-down-var patternval)))
  (cond 
   ((and (basic-expr constitval)
	 (basic-expr patternval)
	 (compat feat constitval patternval))
    (values *success* nil undos))
      
   ((eq constitval patternval) 
    (values *success* nil undos)) ;; added this to handle numbers
   
   ;; if one or both of the values are variables, and the two
   ;; values can be unified, then return the bindings necessary
   ;; for the unification. 
   ((var-p constitval)
    ;; if both arguments are the same variable, no bindings
    ;; necessary.
    (if (eq constitval patternval)
	(values *success* nil undos)
      (if (and (var-non-empty constitval) 
	       (member patternval (list '- *empty-constit*)))
	  (progn
	    (if FAILURE-TRACE (failure-message constitval patternval feat))
	    (values nil nil undos))
	  (multiple-value-bind
		(intersection bndgs prob newundos)
	      (variable-intersect feat constitval patternval 'reversed undos)
	    (if intersection
		(let ((newbndgs (add-to-binding-list bndgs
					     (add-to-binding-list
					      (if (not (equal constitval intersection))
						  (make-binding-list constitval intersection))
					      (if (and (var-p patternval)
						       (not (equal patternval intersection)))
						  (make-binding-list patternval intersection))))))
		  (if newbndgs 
		      (values newbndgs prob (replace-vars newbndgs newundos))
		      ;;  CONSTITVAL was a variable that matched exactly the constant PATTERNVAL
		      (values *success*  prob undos)))
		(progn
		  (if FAILURE-TRACE (failure-message constitval patternval feat))
		  (values nil nil newundos)))))
	
	  ))
   
   ((var-p patternval) ;; patternval is a var and constitval isn't
    ;; if var has non-empty restriction and val is -', then fail
    (if (and (var-non-empty patternval) 
	     (member constitval (list '- *empty-constit*)))
	(cond
	 ((null (var-values patternval))
	  (when FAILURE-TRACE (failure-message constitval patternval feat))
	  (values nil nil undos))
	  ;; as long as '- is not in the values we are OK
	 ((and (consp (var-values patternval))
	       (not (member '- (var-values patternval))))
	       (values *success* nil undos))
	 ;; otherwise we just fail
	 (FAILURE-TRACE
	   (failure-message constitval patternval feat)
	   (values nil nil undos)))
      ;; either patternval does not have EXLUSION values, or constitval is not - or *empty-constit*
      (multiple-value-bind
	  (intersection bndgs prob newundos)
	  (variable-intersect feat patternval constitval nil undos)
	(if intersection
	    (if (not (equal patternval intersection))
		(let ((newbndgs (make-binding-list patternval intersection)))
		  (values ;;(replace-vars
		   (add-to-binding-list bndgs newbndgs) prob (replace-vars newbndgs newundos)))
	      (values *success* prob undos))
	  (progn 
	    (if FAILURE-TRACE (failure-message constitval patternval feat))
	    (values nil nil newundos))))))
   
   ;;  matching two lists
   ((and (listp patternval) (listp constitval))
    (match-lists feat patternval constitval undos))
   
   ;;  recursive matching of two values that are constituents
   ((and (constit-p patternval) (constit-p constitval))
    (constit-match1 patternval constitval undos))
   ;; Failed
   (t
    (if FAILURE-TRACE (failure-message constitval patternval feat))
    (values nil nil undos))
   ))

(defun make-binding-list (var val)
  (cond
   ((var-p val)
    ;;    (format t "Checking var: ~S and ~S" var val)
    (if (equal var (chase-down-var val))
      (break)
      (list (list var val))))
   ((not (equal var val))
    (list (list var val)))
   (t *success*)))

(defun add-to-binding-list (bndgs newbndgs)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
   (cond
   ((eq bndgs *success*) newbndgs)
   ((eq newbndgs *success*) bndgs)
   (t
    (append bndgs newbndgs))))

(defun chase-down-var (var)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "follow down a chain of var bindings, then return var, or its
    value if it is a single value"
  (if (var-p var)
      (let ((var-val (var-values var)))
	(cond
	 ;; chase down var pointers
	 ((var-p var-val)
	  (chase-down-var var-val))
	 ;; if a list of alternatives, return the var
	 ((or (listp var-val) (arrayp var-val) (constit-p var-val))
	  var)
	 ;; it is exclusionary, make it into a list of values
	 ((var-non-empty var)
	  (setf (var-values var) (list (var-values var)))
	  var)
	 #|| ;; its a single value, if the var is not exclusionary, and its not a constit, return the value
	 ((null (var-non-empty var))
	 (if (constit-p (var-values var))
	 var
	 (var-values var)))||#
	 (t var)))
     
    var))

(defun variable-intersect (feat var val reversed undos)
  "VAR is always a variable, and is the PATTERN unless REVERSED is non-nil.
    returns the intersection, the bindings and a probability"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((varval (var-values var)))
    (cond
     ;;  We have a SEM
     ((arrayp varval)
      (unify-sems var val undos)
      )
     ((constit-p varval)
      (if reversed (match-embedded-constits val var undos)
	(match-embedded-constits var val undos)))
     ((or (Symbolp Varval) (numberp varval))
      (values (intersect-values feat var val) nil nil undos))
     ((listP varval)
      (values (intersect-values feat var  val) nil nil undos))
     (t (parser-warn "Illegal variable found: ~S" var)
	(break)
	nil))))
   	
;; INTERSECT-VALUES - Takes a variable and an arg (val) that is a value,
;;      simple variable or constrained variable
;; IT DOES NOT HANDLE EMBEDDED CONSTITUENTS IN VARIABLE VALUES, thus it doesn't bind new vars
;;  A constrained variables consists of a list-of-values and a boolean that
;; indicates
;;    whether the list indicates inclusions or exclusions.
;;  Assuming VAR is a variable with a list of values and an exclusion boolean:
;;  This returns the intersection in the cases where
;;     val is an expression and 
;;             (a) exclude-flag is NIL and val is in the list of values, or
;;             (b) exclude-flag is T and val is not in the list of values,
;;          then the answer is val
;;     val is an unconstrained variable, then the answer is VAR
;;     val is a constrained variable, then there is a four way set of
;; combinations
;;        expressed as follows, where VAL1 are the values for VAR, and VAL2
;; for VAL,
;;        and macth succeeds if the answer is non-empty
;;  VAR (V1) \  VAL (V2)      INCLUSION-VALUES         EXCLUSION-VALUES
;;                     ---------------------------|--------------------------
;; INCLUSION-VALUES    |      V1 intersect V2     |     V1 - V2             |
;;                       ------------------------------------------------------
;; EXCLUSION-VALUES    |      V2 - V1             |     exclude V1 union V2 |
;;                     ------------------------------------------------------
;;
;; Unlike for the above functions, it doesn't matter which argument is
;; from the chart and which is from a rule pattern.

;; Note: this generally returns a value, but not the binding -- the binding is added in variable-intersect	
 
(defun intersect-values (feat var val)
  "VAR is a variable with one or more values, that are not constit or SEMs"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((v-list (var-values var))
	 (value-list (if (listp v-list) v-list (list v-list)))
	 (var-exclude (var-non-empty var))
	 (ans
	  (cond 
	   ;; If the var is unconstrained:  Succeed unless var occurs in val
	   ((null v-list) 
	    (if (occurs-in var val) 
		nil val))
	   ;;  If val is not a variable, then it must be a value
	   ;; check if it is (or is not) in the value-list
	   ((not (var-p val))
	    (let ((compat-vals (if (not var-exclude)
				   (member-match feat val value-list)
				   (non-member-match feat val value-list))))
	      ;; if one one then we have a constant result, if more than one we need a variable
	      
	      (cond ((null compat-vals) nil)
		    ((or (symbolp compat-vals)
			 (not (consp compat-vals)))
		     compat-vals)
		    ((eq (list-length compat-vals) 1)
		     (car compat-vals))
		    ((equal compat-vals value-list)  ;; reuse old variable if every value matched!
		     var)
		    (t
		     (make-var :name (gen-v-num 'v) :values compat-vals)))))
                 
	   ;; otherwise, VAL is a variable
	   (t ;;(var-p val)
	    (let*
		((o-val (var-values val))
		 (other-values (if (consp o-val) o-val (list o-val))))
	      (if (null o-val)
		  ;; VAL is an unconstrained variable, return VAR
		  VAR
		;; VAL has list of values, compute the appropriate
		;;"intersection"
		(let*
		    ((val-exclude (var-non-empty val))
		     (int-values 
		      (if var-exclude
			  (if val-exclude 
                              (union value-list other-values)
			    (set-subtract other-values value-list))
			(if val-Exclude
			    (Set-subtract value-list other-values)
			  (intersection-match feat value-list other-values)))))
		  ;;  now build the new answer
		  (cond 
		   ;;  If int-values is null, then the match failed
		   ((null int-values) nil)
		   ;;  if both were exclusions, build an exclusion answer
		   ((and val-exclude var-exclude)
		    (build-var (var-name var) int-values t))
		   ;;  else return int-values as the answer
		   (t (build-var (gen-v-num 'v) ;(gen-symbol 'v) 
				 int-values)))))))
                 )))
    (if (and (not (listp val)) (listp ans) (eq (length ans) 1))
	(setq ans (car ans)))
    ans))

(defun occurs-in (var val)
  "Occurs check: a variable can't unify with an expression that contains the same variable"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond ((listp val)
	 (cond ((null val) nil)
	       ((member var val) (trace-msg 2 "~%OCCURS CHECK ELIMINATES ~S and ~S match~%" var val) t)
	       (t (some #'(lambda (x) (occurs-in var x)) val))))
	((constit-p val)
	 (occurs-in var (constit-feats val)))
	((var-p val)
	 (occurs-in var (var-values val)))
	))

(defun match-embedded-constits (pattern var undos)
  "Matches constituents that are values of variables; returns 4 values: var, bndgs, prob, and undos, to be compatible with variable-intersect code"
  (cond
    ((and (var-p pattern) (null (var-values pattern)))
     (values var nil nil undos))
    ((and (var-p var) (null (var-values var)))
     (values pattern nil nil undos))
    (t (let ((p (if (var-p pattern) (var-values pattern) pattern))
	     (v (if (var-p var) (var-values var) var)))
	 (if (and (constit-p p) (constit-p v))
	     (multiple-value-bind (bndgs prob newundos)
		 (constit-match1 p v undos)
	       (If bndgs (values var bndgs prob newundos)
		   (values nil nil nil newundos)))
	     (values nil nil nil undos))))))
 
#||   ;; already defined in unify.lisp
(defun verify-and-build-constit (constit rule head)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
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
      (build-constit-in-rule cat feats (car lfg) head rule)))))||#

#|
(defun member-match (feat val vlist)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
   (remove-if-not  #'(lambda (v)
             (or (if (eq val v) v)
                (compat feat val v)))
	   vlist))
|#

(defun member-match (feat val vlist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (remove-if #'null (mapcar #'(lambda (v) (compat feat val v)) vlist))
  )


(defun non-member-match (feat val vlist)
  (if (every #'(lambda (v)
               (and (not (eq val v))
                    (not (compat feat val v))))
	   vlist)
    val))

(defun set-subtract (set minus)
  (remove-if #'(lambda (v)
                 (some #'(lambda (m)
                           (subtype 'w::sem  v m))
                       minus))
             set))

(defun intersection-match (feat vlist1 vlist2)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((ans))
    (cond ((null vlist1) nil)
          ((setq ans (compat-match feat (car vlist1) vlist2))
           (cons ans (intersection-match feat (cdr vlist1) vlist2)))
          ((intersection-match feat (cdr vlist1) vlist2)))))

(defun compat-match (feat val vlist)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((ans))
    (cond ((null vlist) nil)
        ((eq val (car vlist)) val)
        ((setq ans (compat feat val (car vlist))) ans)
        (t (compat-match feat val (cdr vlist))))))

;;  This function  returns the the more specific value is v1 is a subtype of v2
;;     or v2 is a subtype of v1

(defun compat (feat v1 v2)
   (declare (optimize (speed 3) (safety 0) (debug 0)))
   (cond ((eq v1 v2) v1)
	 ((subtype feat v1 v2) v1)
	 ((subtype feat v2 v1) v2)))

#||
;; VARIABLE MANAGEMENT

;;  We try to manage the memory space for variables and reuse them in successive parses.
;;    This helps reduce memory consumption as it seems that gensym'ed names are not garbaged collected

(defvar *available-vars* nil)
(defvar *temporary-vars* nil)
(defvar *available-names* (make-hash-table))
(defvar *temporary-varnames* nil)

(defun make-temporary-var (name values flag)
  (let* ((v (if *available-vars*
	       (pop *available-vars*)
	     (make-var)))
	(nametable (gethash name *available-names*))
	(n (if nametable (pop nametable)
	     (gen-v-num name)))) ;(gen-symbol name))))
    (push v *temporary-vars*)
    (push (cons name n) *temporary-varnames*)
    (setf (var-name v) n)
    (setf (var-values v) values)
    (setf (var-non-empty v) flag)
    v))

(defun reclaim-vars ()
  (mapc #'(lambda (x)
	    (push (cdr x) (gethash (car x) *available-names*)))
	*temporary-varnames*)
  (setq *temporary-varnames* nil)
  (setq *available-vars* (append *temporary-vars* *available-vars*))
  (setq *temporary-vars* nil))
||#
		 
