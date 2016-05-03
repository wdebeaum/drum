;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu,  9 Feb 2004
;;;; Time-stamp: <Tue Apr 27 13:49:42 EDT 2004 ferguson>
;;;;

(in-package :simplekb)

(defun kb-assert-fact (fact)
  "Adds FACT to the KB."
  (add-clause (list fact)))

(defun kb-assert-rule (rule)
  "Adds RULE (a Horn clause) to the KB."
  (add-clause rule))

(defun kb-assert-fact-uniquely (fact)
  "Adds FACT to the KB if it can't be proven. Use only as directed."
  (unless (kb-prove fact :max-solutions 1)
    (kb-assert-fact fact)))

(defvar *results* nil
  "List containing results of proofs.")

(defvar *max-solutions* nil
  "Maximum number of solutions to find. NIL means find them all.")

(defun kb-prove (fact &key max-solutions)
  "Tries to prove FACT. Returns two values: list of results and list of
bindings (NIL if can't be proved)."
  (let ((*results* nil)
	(*max-solutions* max-solutions))
    (prove-all (list fact '(gather-prolog-vars))
	       no-bindings)
    (setq *results* (nreverse *results*))
    (values (loop for bindings in *results*
		  collect (subst-bindings bindings fact))
	    *results*)))
	  
(defun gather-prolog-vars (args bindings other-goals)
  "Saves successful bindings, for returning later."
  (declare (ignore args))
  ;; Save this solution
  (push bindings *results*)
  ;; Keep looking?
  (cond
   ((or (null *max-solutions*) (> (decf *max-solutions*) 0))
    ;; Yes
    fail)
   (t
    ;; No
    (prove-all other-goals bindings))))

(setf (get 'gather-prolog-vars 'clauses) 'gather-prolog-vars)

(defun kb-prove-all (facts)
  "Tries to prove list of FACTS (with consistent bindings). Returns two
values: list of results and list of bindings (NIL if can't be proved)."
  (let ((*results* nil))
    (prove-all (append facts '((gather-prolog-vars)))
	       no-bindings)
    (values (loop for bindings in *results*
		  collect (subst-bindings bindings facts))
	    *results*)))

(defun kb-retract (fact)
  "Removes from the KB all clauses (facts and rules) whose head matches FACT."
  (loop for predicate in *db-predicates*
	do (setf (get predicate 'clauses)
		 (delete-if #'(lambda (clause)
				(unify (clause-head clause) fact))
			    (get predicate 'clauses)))))


(defun kb-all ()
  "Return all clauses for all predicates in the db."
  (apply #'concatenate 'list (mapcar #'(lambda (p)
					 (get-clauses p))
				     *db-predicates*)))

(defun kb-dump ()
  "Pretty-print all clauses for all predicates in the db."
  (loop for p in *db-predicates*
	do (loop for c in (get-clauses p)
		 do 
		 (format t "~S~%" (list* '<- (first c) (rest c))))))
