(in-package :dsl)

(defun partition (key-fn l &key (test #'eql))
  "Given a key function and a list, return a list of pairs (key-val . items),
   where items are the items in the original list for which the key function
   returned key-val."
  (loop with ret = nil
        for item in l
	for key-val = (funcall key-fn item)
	for pair = (or (assoc key-val ret :test test)
	               (car (push (cons key-val nil) ret)))
	do (push item (cdr pair))
	finally (return ret)))

(defun hash-table-keys (h)
  (loop for k being the hash-keys of h collect k))

;;; helpers for eval-path-expression

(defmacro add-seed-to-output (seed output)
  `(if (hash-table-p output)
     (pushnew (list ,seed) (gethash ,seed ,output) :test #'equalp)
     (pushnew ,seed ,output :test #'eq)
     ))

(defmacro add-step-to-output (input source label target output)
  `(if (hash-table-p ,output)
     (let ((step-path (list ,target ,label))) ; ,source)))
       (dolist (source-path (gethash ,source ,input))
	 (pushnew (append step-path source-path) (gethash ,target ,output)
		  :test #'equalp)))
     (pushnew ,target ,output :test #'eq)
     ))

(defun output-empty-p (output)
  (or (null output)
      (and (hash-table-p output) (= (hash-table-count output) 0))
      ))

(defmacro eval-relation-step (expr input direction label result output)
  `(let ((input-concepts (if (hash-table-p ,input)
			   (hash-table-keys ,input)
			   ,input)))
    (dolist (i input-concepts)
      (when (typep i 'concept) ; only concepts have relations
	(let ((concept-name (name i)))
	  ;; don't bother trying to look up ld:: names since they're probably
	  ;; gensyms.
	  (unless (eq (symbol-package concept-name)
	  	      (find-package :lexicon-data))
	    (require-concept concept-name)))
	(dolist (r (,direction i))
	  (when (eq ,label (label r))
	    (add-step-to-output ,input i ,expr (,result r) ,output)))))))

(defun repetition-helper (output next)
  "(See the 'repeat branch of eval-path-expression)
   Given output and next, return a modified output and prev.
   Make it so prev has everything we first got on the previous iteration, and
   output has everything we got on any iteration after min-count.
   "
  (cond
    ((hash-table-p output)
      (let ((prev (make-hash-table :test #'eq)))
	(maphash
	  (lambda (item path-to-item)
	    (unless (nth-value 1 (gethash item output))
	      (setf (gethash item prev) path-to-item)
	      (setf (gethash item output) path-to-item)))
	  next)
	(values output prev)))
    (t
      (values
        (union next output :test #'eq)
        (set-difference next output :test #'eq)
	))
    ))

(defun xor (a b)
  (not (eq (not a) (not b))))

(defun eval-path-expression (expr &optional input (db *db*))
  "Given a path expression and a list of input concepts (or values in general,
   but this is mostly intended for concepts), return the list of output
   concepts reachable from the input concepts via paths matching the
   expression.

   If the input is a hash table instead of a list, its keys are taken to be the
   input concepts, and its values are lists of paths back from those concepts
   to the original input for the whole expression. Paths alternate between
   concepts and basic steps. Then this function returns a new hash table in the
   same format for the output concepts.
   
   Path expressions are similar to those in WordNetPath, but are S-expressions
   instead of strings, and operate mainly on slots and relations instead of
   WordNet pointers (though WN pointers can be relations too).
   
   Path expression components:
     Seed:
       resource::concept-name
     Basic steps:
       >label - follow relations with this label forwards, getting targets
       <label - backwards
       ->label - forwards, getting the relations themselves
       <-label - backwards, getting relations
       (> pkg::label), (< pkg::label), (-> pkg::label), (<- pkg::label)
         - as above but for relations whose labels are not keywords
       slot-name - get the slot value for each concept (note that this does not
	 flatten list-valued slots like a function would; if you want that,
	 follow it with #'identity)
       #'function-name
       (lambda (concept) lisp code...) - call the function and use the returned
	 list of concepts as the output. If the return value isn't a list, wrap
	 it in a list.
     Sequencing and repetition:
       (1 exprs...) - follow the exprs in sequence exactly once
	 (exprs...) also works if the first subexpression doesn't conflict with
	 the other operators
       (? exprs...) - zero or one times
       (+ exprs...) - one or more times
       (* exprs...) - any number of times
       (repeat m n exprs...) - between m and n times, inclusive
       (repeat m nil exprs...) - at least m times
     Predicates:
       (when exprs...) - keep those input concepts for which anything is
         reachable via the sequence of exprs
       (unless exprs...) - keep those input concepts for which nothing is
         reachable via the sequence of exprs
     Set operations:
       (& exprs...) - the output is the intersection of the outputs of the
         exprs evaluated on the same input
       (/ exprs...) - union
       (- exprs...) - difference (the first expr is positive, the rest are
         negative)
   "
  (let ((output (when (hash-table-p input) (make-hash-table :test #'eq))))
    (cond
         ;; basic steps
     ((symbolp expr)
       (let ((name (symbol-name expr)))
	 (cond
	   ((not (eq (symbol-package expr) (find-package :dsl))) ; seed
	     (require-concept expr)
	     (let ((val (gethash expr (concepts db))))
	       (when val
		 (add-seed-to-output val output)
		 )))
	   ;; relations (see listp case for packaged relation labels)
	   ((string= "->" (subseq name 0 2))
	     (let ((label (intern (subseq name 2) :keyword)))
	       (eval-relation-step expr input out label identity output)))
	   ((string= "<-" (subseq name 0 2))
	     (let ((label (intern (subseq name 2) :keyword)))
	       (eval-relation-step expr input in label identity output)))
	   ((char= #\> (elt name 0))
	     (let ((label (intern (subseq name 1) :keyword)))
	       (eval-relation-step expr input out label target output)))
	   ((char= #\< (elt name 0))
	     (let ((label (intern (subseq name 1) :keyword)))
	       (eval-relation-step expr input in label source output)))
	   (t ; slot
	     (dolist (i (if (hash-table-p input) (hash-table-keys input) input))
	       (when (and (slot-exists-p i expr) (slot-boundp i expr))
		 (let ((o (slot-value i expr)))
		   (add-step-to-output input i expr o output)
		   ))))
	   )))
     ((functionp expr)
       (let ((input-concepts (if (hash-table-p input)
			       (hash-table-keys input)
			       input)))
	 (dolist (i input-concepts)
	   (let ((ret (funcall expr i)))
	     (dolist (o (if (listp ret) ret (list ret)))
	       (add-step-to-output input i expr o output))))))
     ((listp expr)
       (case (car expr)
	 ;; relations (see above for keyword relation labels)
	 (-> (eval-relation-step expr input out (second expr) identity output))
	 (<- (eval-relation-step expr input in  (second expr) identity output))
	 (>  (eval-relation-step expr input out (second expr) target output))
	 (<  (eval-relation-step expr input in  (second expr) source output))
         ((function lambda)
	   ;; just like functionp, except we have to eval it first
	   ;; the step label is still expr, though
	   (let ((fn (eval expr))
		 (input-concepts (if (hash-table-p input)
				   (hash-table-keys input)
				   input)))
	     (dolist (i input-concepts)
	       (let ((ret (funcall fn i)))
		 (dolist (o (if (listp ret) ret (list ret)))
		   (add-step-to-output input i expr o output))))))
	 ;; sequencing and repetition
	 (1
	   (loop with prev = input
	         with empty-p = nil
		 for subexpr in (cdr expr)
		 do (setf prev (eval-path-expression subexpr prev db))
		 until (setf empty-p (output-empty-p prev))
		 finally (unless empty-p (setf output prev))
		 ))
	 (?
	   (setf output
	         (eval-path-expression `(repeat 0 1 ,@(cdr expr)) input db)))
	 (+
	   (setf output
	         (eval-path-expression `(repeat 1 nil ,@(cdr expr)) input db)))
	 (*
	   (setf output
	         (eval-path-expression `(repeat 0 nil ,@(cdr expr)) input db)))
	 (repeat
	   (destructuring-bind (_ min-count max-count &rest subexprs) expr
	     (let (prev (next input) (once-expr `(1 ,@subexprs)))
	       ;; get the required number of repetitions to start with
	       (loop for c from 1 upto min-count
	             until (output-empty-p next)
		     do
		       (setf prev next)
		       (setf next (eval-path-expression once-expr prev db))
		     )
	       ;; collect output while doing more repetitions until we can't or
	       ;; we reach max-count
	       (loop for c from (1+ min-count)
		     until (or (output-empty-p next)
			       (and max-count (> c max-count)))
		     do
		       (multiple-value-setq (output prev)
		           (repetition-helper output next))
		       ;; take the next step
		       (setf next (eval-path-expression once-expr prev db))
		     finally
		       (multiple-value-setq (output prev)
		           (repetition-helper output next))
		     )
	       )))
	 ;; predicates
	 ((when unless)
	   (let ((unless-p (eq 'unless (car expr)))
	         (rest-expr `(1 ,@(cdr expr))))
	     (if (hash-table-p input)
	       (maphash
		   (lambda (concept paths)
		     (when (xor unless-p
		                (eval-path-expression
				    rest-expr (list concept) db))
		       (setf (gethash concept output) paths)))
		   input)
	       (setf output
		 (remove-if-not
		     (lambda (concept)
		       (xor unless-p
			    (eval-path-expression rest-expr (list concept) db)))
		     input))
	       )))
	 ;; set operations
	 (& ; intersection
	   ;; TODO might be possible to avoid evaluating later subexpressions
	   ;; if we already know the whole expression yields no concepts
	   (let ((se-outputs
		   (mapcar
		       (lambda (subexpr)
			 (eval-path-expression subexpr input db))
		       (cdr expr))))
	     (if (hash-table-p output)
	       (setf output 
		 (reduce
		     (lambda (output-so-far next-output)
		       (maphash
			   (lambda (concept paths)
			       (declare (ignore paths))
			     (unless (gethash concept next-output)
			       (remhash concept output-so-far)))
			   output-so-far)
		       output-so-far)
		     se-outputs))
	       ; else, lists
	       (setf output
		     (reduce
			 (lambda (a b) (intersection a b :test #'eq))
			 se-outputs
			 ))
	       )))
	 (/ ; union
	   (let ((se-outputs
		   (mapcar
		       (lambda (subexpr)
			 (eval-path-expression subexpr input db))
		       (cdr expr))))
	     (if (hash-table-p output)
	       (setf output
		 (reduce
		     (lambda (output-so-far next-output)
		       (maphash
			   (lambda (concept paths)
			     (setf (gethash concept output-so-far)
				   (union (gethash concept output-so-far)
					  (gethash concept next-output)
					  :test #'equalp)
				   ))
			   output-so-far)
		       output-so-far)
		     se-outputs))
	       ; else, lists
	       (setf output
		     (reduce
		         (lambda (a b) (union a b :test #'eq))
			 se-outputs
			 ))
	       )))
	 (- ; difference
	   ;; TODO same issue as intersection
	   (let ((se-outputs
		   (mapcar
		       (lambda (subexpr)
			 (eval-path-expression subexpr input db))
		       (cdr expr))))
	     (if (hash-table-p output)
	       (setf output
		 (reduce
		     (lambda (output-so-far next-output)
		       (maphash
			   (lambda (concept paths)
			       (declare (ignore paths))
			     (when (gethash concept next-output)
			       (remhash concept output-so-far)))
			   output-so-far)
		       output-so-far)
		     (cdr se-outputs)
		     :initial-value (car se-outputs)
		     ))
	       ; else, lists
	       (setf output
		     (reduce
		         (lambda (a b) (set-difference a b :test #'eq))
			 (cdr se-outputs)
			 :initial-value (car se-outputs)
			 ))
	       )))
	 (otherwise; treat unknown operators as just the beginning of a sequence
	   (setf output (eval-path-expression (cons 1 expr) input db)))
	 ))
     (t
       (error "expected symbol, function, or list as path expression, but got: ~s" expr))
     )
   output))


#| probably still too general
(defun traversal-tree (start visit-fn &optional (traversed (make-hash-table :test #'eq)))
  "Do a depth-first traversal of a network of class/structure instance, calling
   visit-fn on each one and returning a tree representing the traversal.
   start - the instance to start at
   visit-fn - a function taking an instance and returning a list of slots to
   traverse (possibly empty)
   traversed - a hash table mapping already-traversed instances to their tree
   values
   "
  (multiple-value-bind (old-tree present-p) (gethash start traversed)
    (when present-p
      (return-from traversal-tree old-tree)))
  (let ((tree (list start)))
    (setf (gethash start traversed) tree)
    (let ((child-slots (funcall visit-fn start)))
      (setf (cdr tree)
            (mapcar
	        (lambda (slot)
		  (traversal-tree (slot-value start slot) visit-fn traversed))
		child-slots
		)
	    ))
    tree))
|#

#| argh, this won't work
(defun traversal-tree (start visit-fn &optional (traversed (make-hash-table :test #'eq)))
  "Traverse a network of class/structure instances calling visit-fn on each one
   and building a tree representing the traversal to return. visit-fn should
   take an instance and return two or three values: a list of the names of
   slots to traverse, a boolean indicating whether this instance should have
   its own node in the final tree (t) or just have its children included among
   its parent's (nil), and optionally the car of the node in the final tree.
   Each instance will be visited exactly once; the result tree for an instance
   reachable in more than one way will be shared, by storing the return values
   of this function in the traversed hash. This function returns two values: a
   tree or a list of child trees, and a boolean indicating which it is (t=tree,
   nil=list of children) It is possible to create circular data structures with
   this function, so setting *print-circle* to t is recommended if calling this
   from the REPL."
  (multiple-value-bind (old-tree present-p) (gethash start traversed)
    (when present-p
      (return-from traversal-tree (values-list old-tree))))
  (let ((placeholder (cons nil nil)))
    (setf (gethash start traversed) placeholder)
    (multiple-value-bind (child-slots new-node-p new-node-car)
	(funcall visit-fn start)
      (
  ))))
|#

#| old and busted

(defmethod dnf ((c concept))
  (let* ((parents
           (mapcar #'target
	           (remove-if-not
		     (lambda (r)
		       ;; TODO also :subtype-of?... filter on provenance?
		       (eq :inherit (label r)))
		     (out c))))
         (parent-dnfs (mapcar #'dnf parents)))
    (cons 'W::OR
          (mapcar (lambda (x)
			   ;; get all the parts to be conjoined that are of the
			   ;; same type
	            (let* ((conj-by-type
		             (partition #'type-of
			                (apply #'append (mapcar #'cdr x))))
			   ;; merge parts of the same type into a new instance
			   (merged-conj
			     (mapcar (lambda (p)
			               (reduce #'merge-concepts
					       (cdr p)
				               :initial-value
					         (make-instance (car p))
					       ))
				     conj-by-type))
			   )
		      ;; TODO merge parts of different types where one is part
		      ;; of another (use concept-part-of-p)
		      (cons 'W::AND merged-conj)))
	          (cartesian-product (mapcar #'cdr parent-dnfs))))))

;; TODO generalize this to operate on all concepts by pushing details into merge-concepts, and use concept-part-of-p to merge parts into wholes
(defmethods dnf ((this (or sem-frame syn-sem)))
  (let ((this-type (type-of this))
        (inherited-dnf (call-next-method)))
    (cons 'W::OR
        (mapcar
	    (lambda (conj)
	      (cons 'W::AND
		  (mapcar
		      (lambda (term)
		        (if (eql this-type (type-of term))
			  (merge-concepts term this)
			  term))
		      (cdr conj)
		      )))
	    (cdr inherited-dnf)
	    ))))

;;; FIXME merge-concepts seems old and busted, need a better way to do this

(defmethod merge-concepts ((dst role-restr-map) (src role-restr-map))
  ; TODO intersect (restriction dst/src)?
  )

(defmethod merge-concepts ((dst list) (src list))
  "Since a list/feats may be nil, this returns the new feature list instead of modifying dst"
  ;; Unfortunately we can't say this in the parameter list because feats is a
  ;; deftype, not a class.
  (declare (type feats dst src))
  (dolist (feat src)
    (let ((existing (assoc (car feat) dst)))
      (if existing
	(let* ((dst-val (if (consp (second existing)) (second existing) (list (second existing))))
	       (src-val (if (consp (second existing)) (second existing) (list (second existing))))
	       (int-val (intersection dst-val src-val)))
	  (when (null int-val)
	    (error "can't unify ~s with ~s" existing feat))
	  (setf (second existing) int-val)
	  )
	; feat is new in dst
	(push feat dst)
	)))
  dst)

(defmethod merge-concepts ((dst semantics) (src semantics))
  (dolist (role (roles src))
    (let ((existing (find (name role) (roles dst) :key #'name)))
      (if existing
	(merge-concepts existing role)
	(push role (roles dst))
	)))
  (setf (sem dst) (merge-concepts (sem dst) (sem src)))
  (when (next-method-p) (call-next-method))
  )

(defmethod merge-concepts ((dst syntax) (src syntax))
  ;; TODO redo this to take into account disjunctions
  (dolist (arg (arguments src))
    (let ((existing (find (syntactic-argument arg) (arguments dst) :key #'syntactic-argument)))
      (cond
	((not existing)
	  (push arg (arguments dst)))
	((equalp arg existing)
	  nil)
	(t
	  (error "tried to add syntactic argument ~s previously defined as ~s" arg existing))
	)))
  (setf (features dst) (merge-concepts (features dst) (features src)))
  (when (next-method-p) (call-next-method))
  )

|#

