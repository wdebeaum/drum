(in-package :dsl)

(defun unify-feat-vals (feature-values)
  "Given a list of values for the same feature (possibly with disjunctions, variables, and nils), return multiple values:
   - a unified value, or nil
   - if unification succeeded, a list of variable bindings or nils, one for each of the input values
   "
  (let (variables valueses)
    (dolist (val feature-values)
      (cond
        ((or (null val) (util:is-variable-name val))
	  (push val variables))
        ((typep val 'disjunction)
	  (push nil variables)
	  (push (cdr val) valueses)
	  )
	(t
	  (push nil variables)
	  (push (list val) valueses)
	  )
	))
    (let* ((int (reduce #'intersection (reverse valueses)))
           (unified
	     (cond
	       ((null int) nil)
	       ((null (cdr int)) (car int))
	       (t (cons 'w::or int))
	       ))
	   )
      (if unified
        (values unified
	        ;; pair each var with the unified value to make bindings
	        (mapcar (lambda (var) (when var (cons var unified))) 
		        (reverse variables))
		)
	(values nil nil)
	))
    ))

(defun unify-feats (&rest feature-lists)
  "Given a number of feature lists (possibly with disjunctions and variables), return multiple values:
   - a single feature list with all the features that can be made consistent across all the input lists
   - a list of variable binding lists, one for each of the inputs, mapping variables seen in those inputs to values used in the unified list
   - a list of remainder feature lists, one for each of the inputs, containing all features that conflict with other lists, and are thus not included in the unified list
   Note that variables are matched greedily, so if the same variable appears as the value of more than one feature in the same feature list, earlier occurrences may unify (and end up in the unified list and bindings) while later ones conflict (and end up in the remainder with the previously-bound value).
   "
  (let (unified-feats
        (bindingses (mapcar (constantly nil) feature-lists))
        (remainders (mapcar (constantly nil) feature-lists))
	(feature-names
	  (remove-duplicates
	      (mapcan (lambda (feats) (mapcar #'car feats)) feature-lists)))
	)
    (dolist (name feature-names)
      (let ((vals
              (mapcar
	        (lambda (feats)
		  (second (assoc name feats)))
		feature-lists)))
        ;; apply bindingses so far to vals
        (loop for vals-tail on vals
	      for val = (car vals-tail)
	      for bindings in bindingses
	      when (util:is-variable-name val)
	        do (let ((bound-val (cdr (assoc val bindings))))
		     (when bound-val
		       (setf (car vals-tail) bound-val)))
	      )
	(multiple-value-bind (unified-val new-bindings)
	    (unify-feat-vals vals)
	  (cond
	    (unified-val
	      (push (list name unified-val) unified-feats)
	      (loop for bindingses-tail on bindingses
	            for new-binding in new-bindings
		    when new-binding
		      do (push new-binding (car bindingses-tail))
		    )
	      )
	    (t ; unification failed, add vals to remainders
	      (loop for remainders-tail on remainders
	            for val in vals
		    when val
		      do (push (list name val) (car remainders-tail))
		    ))
	    ))
        ))
    (values
      (reverse unified-feats)
      (mapcar #'reverse bindingses)
      (mapcar #'reverse remainders)
      )
    ))

;; TODO wrapper for unify-feats as a test whether everything in the first feature list unifies with something in the second
;; (null (first (nth-value 2 (unify-feats f1 f2))))
