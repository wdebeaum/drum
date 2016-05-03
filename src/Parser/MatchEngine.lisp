(in-package "IM")

;; Reading and Matching Rules to interpret Parser LF
;;   Rules are compiled into a linked structure that efficiently keeps track of the
;;     partially matched rules so far.
;;  Rules consist of
;;      Input  - LF terms that match the input
;;      Context Patterns - which match the input but do not "consume" it. There are three
;;         types of contextual patterns
;;              + patterns = must match input just like regualr patterns
;;              - patterns = which must NOT match the input
;;              P procedure calls to arbitrary lisp functions, which return a binding list if they succeed
;;     Output - the new transformed expressions that correspond to the content in the input

(defvar *im-rule-table* nil) ;; an assoc list indexing rules by ID
(defvar *initial-match-trees* nil)  ;; a list of start nodes for rules
(defvar *active-rules* nil)  ;; a list of active nodes in current matching process
(defvar *im-trace* nil)   ;; controlling tracing

(defstruct IM-rule 
  :id       ;; a uniquue identifer for the rule
  :pattern  ;; a list of terms that must match input
  :context-match  ;; a list of terms that must match input but are not covered by the output
  :exclude  ;; a list of terms that must not match the input
  :call ;; a list of procedure calls to make that must succeed
  :output  ;; the output terms
  :max-pattern-number   ;; the max index for patterns (as opposed to context matches)
  :pattern-Index   ;; an array of the patterns and context terms for rapid retrival when traverse match tree
  )

(defstruct context-pattern
  :type :pattern)

(defstruct mnode
  :pattern     ;; the pattern that must match
  :cover-pattern?   ;; t if this ia a true pattern and not just a context check
  :bndgs       ;;  bindings
  :children    ;;   index tree to succeeding matches
  :covers      ;;  record of what input is covered
  :rule-id)    ;; the rule ID

(defun reset-im nil
  (setq *im-rule-table* nil)
  (setq *initial-match-trees* nil)
  (setq *active-rules* nil))

;; access rules
(defun find-rule (id)
  (cdr (assoc id *im-rule-table*)))

(defun find-pattern-in-rule (index id)
  (let ((rule (find-rule id)))
    (if (im-rule-p rule)
	(aref (im-rule-pattern-index rule) index)
      (im-warn "no rule defined for id ~S" id))))

;; reading a rule

(defun add-im-rule (r)
  (let ((rule (read-LF-rule r)))
    (if (im-rule-p rule)
	(let ((id (im-rule-id rule)))
	  (if id
	      (if (assoc id *im-rule-table*)
		  (im-warn "duplicate id ~S found. Rule is ignored" id)
		(progn
		  (setq *im-rule-table* (cons (cons (im-rule-id rule) rule) *im-rule-table*))
		  (setq *initial-match-trees*
			(append (gen-match-trees rule)
				*initial-match-trees*))))
		(im-warn "No ID in rule ~S. It is ignored" r))))))

(defun read-LF-rule (r)
  "Example rule:
      ((SPEECHACT ?x SA_TELL :CONTENT ?c)     ;; input pattern 
       (F ?c LF_NECESSITY :THEME ?a)          ;; input pattern
       (+ (F ?a LF_ACTION))                   ;; context pattern, must match but is not converted by rule
       --->
       (DONE *1)                              ;;  output
       (A *1 PROPOSE :THEME ?a))              ;;  output"
  
  (when (verify-rule-format r)
    (let ((im-rule (make-im-rule))
	  )
      (init-var-table)  ;; clear VAR table for reading the rule
      (read-next-term r im-rule)
      (set-up-index-in-im-rule im-rule)
      im-rule)))

(Defun  read-next-term (r im-rule)
  "Reads one term and recurses to read other terms. This assumes we're reading the INPUT side.
   We convert the terms to constituents in order to resuse the parser matching capabilities.
   NB: Does not initialize variable table"
  (when r
    (let ((first (car r)))
      (cond
       ((symbolp (car r))
	(setf (im-rule-id im-rule) (car r))
	(read-output-terms (cdr r) im-rule))
       ((consp first)
	(case (car first)
	  (+ (setf (im-rule-context-match im-rule)
		   (append (read-LF-term (second first)) (im-rule-context-match im-rule))))
	  (- (setf (im-rule-exclude im-rule)
		   (append (read-LF-term (second first)) (im-rule-exclude im-rule))))
	  (C (setf (im-rule-call im-rule)
		   (cons (im-read-value (second first)) (im-rule-call im-rule))))
	  (otherwise (setf (im-rule-pattern im-rule)
			   (append (read-LF-term first) (im-rule-pattern im-rule))))
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

(defun read-lf-term (term)
  "this reads the term by converting it into a list of 
   NB: does not initialize var table"
  (im-read-value
   (cons (list (car term) (cadr term) (caddr term))
	 (gen-role-terms (cdddr term) (cadr term)))))

(defun gen-role-terms (roles var)
  (when roles
    (cons (list (car roles) var (cadr roles))
	  (gen-role-terms (cddr roles) var))))

  (defun verify-rule-format (r)
  t)


(defun set-up-index-in-im-rule (im-rule)
  (let* ((terms (append (im-rule-pattern im-rule) (im-rule-context-match im-rule)))
	(N (list-length terms))
	(arr (make-array N)))
    (setf (im-rule-max-pattern-number im-rule) (length (im-rule-pattern im-rule)))
    (dotimes (i N)
     
      (setf (aref arr i) (nth i terms)))
    (setf (im-rule-pattern-Index im-rule) arr)
    im-rule))

;; Reading input

(defun im-read-value (x)
  "This invokes the parser reader - but we need to convert special symbols first"
  (read-value (subst 'parser::? '? x) nil))


(defun read-LFs (lfs)
  (init-var-table)
  (mapcan #'read-LF-term lfs))

;; BUILDING the match TREE
;;  When a rule is read, the input terms and the +pattern terms are grouped together and all
;;   possible orderings of these terms are generated and added to the matching graph.

(defun gen-match-trees (rule)
  (let* ((n (- (+ (im-rule-max-pattern-number rule) (length (im-rule-context-match rule))) 1))
	 (mtree (gen-orderings (gen-number-list n 0))))
    (generate-new-frontier (im-rule-id rule) mtree nil nil)))

(defun gen-orderings (ll)
  "generates a decision tree-like structure capturing all orderings of objects in the list ll.
   e.g.  (gen-orderings '(a b c)) returns the tree ((A (B (C)) (C (B))) (B (A (C)) (C (A))) (C (A (B)) (B (A))))"
  
  (if (eq (list-length ll) 1)
      (list ll)
    ;; more than one - take each off and recurse on rest
    (mapcar #'(lambda (x)
		;;(mapcar #'(lambda (z)
		(cons x
		      (gen-orderings (remove-if #'(lambda (y) (eq x y)) ll))))
	    ll)))

(defun gen-number-list (n min)
  "generates a list from n down to zero"
  (if (>= n min)
      (cons n (gen-number-list (- n 1) min))))

(defun generate-new-frontier (id children cover-info bndgs)
  "takes a mtree for a rule (identified by ID) with associated bndgs"
  (let* ((rule (find-rule id))
	 (max-pattern-number (im-rule-max-pattern-number rule)))
    (mapcar #'(lambda (mtree)
		(let* ((index (car mtree))
		       (pattern (aref (im-rule-pattern-index rule) index)))
		    (make-mnode :pattern (subst-in pattern bndgs)
				:cover-pattern? (< index max-pattern-number)
				:bndgs bndgs
				:rule-id id
				:covers cover-info
				:children (cdr mtree))))
	      children)
  ))

(defun im-warn (&rest args)
  (apply #'format (cons t args)))


;;;  MATCHING

(defun interpret-LF (LFs)
  (let*
      ((candidates (generate-rule-matches LFs))
       (filtered-candidates (filter-candidates candidates LFs)))
    (multiple-value-bind (min-cover remainder)
	(find-min-cover filtered-candidates (gen-number-list (list-length LFs) 1))
      (values
	  (if  min-cover
	      (mapcan #'(lambda (x) (subst-in (im-rule-output (find-rule (mnode-rule-id x))) (mnode-bndgs x)))
		      min-cover))
	  (mapcar #'(lambda (x) (nth (- x 1) LFs)) remainder) ;; note: we will need to instantiate bindings here eventually, but where do they come from? JFA 8/03
	  ))))

  

(defvar *completed-rules* nil)

(defun record-rule (n covers bndgs)
  (im-trace-msg "~%    rule ~S completed with bndgs ~S" (mnode-rule-id n) bndgs)
  (setq *completed-rules*
	(cons
	 (make-mnode :rule-id (mnode-rule-id n)
		     :covers covers
		     :bndgs bndgs)
	 *completed-rules*))
  (values))

(Defun generate-rule-matches (LFs)
  (let ((*completed-rules* nil))
    (find-match-candidates LFs)
    *completed-rules*))

(defun find-match-candidates (input)
  (match-next-input input 1 *initial-match-trees*)
  )

(defun match-next-input (input lf-index frontier)
  (if (null input)
      frontier
    (let ((i (car input)))
      (im-trace-msg "~%Matching LF ~S ..." i)
      (let*
	  ((successes (mapcar #'(lambda (r)
				   (let ((bndgs (match-vals (mnode-pattern r) i)))
				     (setq bbb bndgs) (setq ppp (mnode-pattern r))

				     (if bndgs (cons r (add-to-binding-list bndgs (mnode-bndgs r))))))
			       frontier))
	   (new-frontier (mapcan #'(lambda (s)
				     (when s
				       (let ((mn (car s))
					     (bndgs (cdr s)))
					 (if (mnode-children mn)
					     (generate-new-frontier (mnode-rule-id mn)
								    (mnode-children mn)
								    (compute-cover lf-index
										   mn
										   )
								    bndgs)
					   (record-rule mn (compute-cover lf-index mn) bndgs)))))
				 successes)))
      (if new-frontier (trace-pprint "~%New Frontier is:" new-frontier)
	(if (null successes) (im-trace-msg "~%No match found, moving on to next LF")))
      (match-next-input (cdr input) (+ lf-index 1)
			(or new-frontier frontier) ;; if didn't find a match, pass on old frontier
			)))))

(defun compute-cover (lf-index mn)
  "adds index to cover if the number indicates that a pattern was matched"
  (if (mnode-cover-pattern? mn)
      (cons lf-index (mnode-covers mn))
      (mnode-covers mn)))

(defun filter-candidates (mnodes input)
  "This checks negative constraints and does procedure calls"
  (remove-if #'null
	     (mapcar #'(lambda (mn) (filter-candidate mn input))
		     mnodes)))

(defun filter-candidate (mn input)
  "checks excludes and calls and returns MNODE (possible a new one with additional bindings)
    only if they are OK"
  (let* ((rule (cdr (assoc (mnode-rule-id mn) *im-rule-table*)))
	 (exclude (im-rule-exclude rule))
	 (call (im-rule-call rule))
	 ;;(covers (mnode-covers mn))
	 (bndgs (mnode-bndgs mn)))
    (im-trace-msg "~%Checking rule ~S with bndgs ~S" (mnode-rule-id mn) bndgs)
    (if (null exclude)
	(if (null call)
	    mn
	  (check-call call mn bndgs))
      (let
	  ((newbndgs (check-exclude exclude mn bndgs input)))
	(if newbndgs
	    (if (null call)
		(insert-new-bindings mn newbndgs)
	      (check-call call mn newbndgs)))))))

(defun check-exclude (exclude mn bndgs input)
  (if (null exclude)
    bndgs
    (let ((first (subst-in (car exclude) bndgs)))
      (if (null (match-one first input))
	  (check-exclude (cdr exclude) mn bndgs input)
	(im-trace-msg "Rule ~S eliminated due to exclusion match ~S" (mnode-rule-id mn) first)
	))))

(defun match-one (pattern input)
  (some #'(lambda (i)
	    (match-vals pattern i))
	input))

(defun check-call (call mn bndgs)
  (let ((specific-call (subst-in call bndgs)))
    (format t "SHould call ~S now " specific-call)
    (values)))

(defun insert-new-bindings (mnode bndgs)
  "creates a new Mnode with the additional bindings"
  (make-mnode :pattern (mnode-pattern mnode)
	      :bndgs (add-to-binding-list bndgs (mnode-bndgs mnode))
	      :children (mnode-children mnode)
	      :covers (mnode-covers mnode)
	      :rule-id (mnode-rule-id mnode)))

;;  FINDING MINIMAL RULE COVERING OF INPUT

(defun find-min-cover (MNs items)
  "find the minimal subset of MNs that cover all positions from 1 to N.
    This is the crudest algorithm - we pick the largest coverage, remove the numbers, filter and recurse"
  (find-next-best-single-cover MNs items nil))

(defun find-next-best-single-cover (MNs items current-cover)
  ;;(format t "~%IN FNBSC: items are ~S, current cover is ~S, MNs are ~S" items current-cover MNs)
  (if MNs
      (let* ((sorted-NMs (sort MNs #'> :key #'(lambda (x) (list-length
						       (remove-if-not #'(lambda (x) (member x items))
								      (Mnode-covers x))))))
	     (best (car sorted-NMs))
	     (covered (intersection (mnode-covers best) items))
	     (new-cover (if covered (cons best current-cover) current-cover))
	     (remaining-items (remove-if #'(lambda (x) (member x covered)) items)))
	(if (null remaining-Items)
	    new-cover
	  (if (null covered)
	      (values new-cover remaining-Items)
	    (find-next-best-single-cover (cdr sorted-NMs) remaining-items new-cover))))
    ;; no more Mnodes, return what we have
    (values current-cover items)))

(defun im-trace-msg (&rest args)
  (when *im-trace*
     (apply #'format (cons t args)))
  (values))

(defun trace-pprint (msg x)
  (when *im-trace*
    (format t msg)
    (pprint x))
  (values))
    

(defun interp (lfs)
  (let ((remainder (read-lfs lfs))
	(final-output nil)
	(unaccounted-for nil))
  (loop while remainder
	do
	(im-trace-msg "~%Interpreting ~S" remainder)
	(multiple-value-bind (output r)
	    (interpret-LF remainder)
	  (im-trace-msg "~%Produced ~S, LFs remaining: ~S" output r)
	  (if output
	      (progn
		(setq final-output (append final-output output))
		(setq remainder r))
	    ;; could not find a match with first term, so we skip it
	    (progn
	      (setq unaccounted-for (cons (car remainder) unaccounted-for))
	      (setq remainder (cdr remainder))))))
  (values (simplify-output final-output) unaccounted-for)))

(defun simplify-output (terms)
  (substitute-ids (reassemble-frames
		   (subst (gen-symbol 'X) '*1 terms))))

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
	   (new-frames (mapcar #'(lambda (f) (add-to-frame (if (keywordp rolename) rolename (keywordify rolename))
							   var val f)) frames)))
      (insert-roles (cdr roles) new-frames))))

(defun add-to-frame (rolename var val frame)
  (if (eq var (second frame))
       (append frame (list rolename val))
	 frame))
     
 (defun substitute-ids (terms)
   (multiple-value-bind
       (IDs others)
       (separate #'(lambda (x) (eq (car x) 'ID)) terms)
     (mapcar #'(lambda (x)
		 (setq others (subst (third x) (second x) others)))
	     IDs)))

(defvar *keyword-package* (find-package :keyword))

(defun keywordify (x)
  (intern (symbol-name x) *keyword-package*))

(defun gen-symbol (x)
  (gentemp (string x)))