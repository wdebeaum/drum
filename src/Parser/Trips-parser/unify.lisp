;;; This file contains functions that let us unify parts of the logical form 
;;; from the parser.

;______________________________________________________________________________
;; Declaration of being in the parser package.

(in-package parser)

;______________________________________________________________________________
;;; Global variables.

(defvar *dummy-var* '*DUMMY* "Placeholder for the variable of the object
	currently being investigated")

;;#||
;______________________________________________________________________________
;; the top-level function for the recursion; that is, it takes two arb
;; *atoms* and unifies them (either they're equal, or they're variables
;; which can be unified
;;
;; the return value is either the unified value (which may be a new variable)
;; or NIL, if unification is not possible.
(defun match-arb (s1 s2)
  (when (and (atom s1) (atom s2))
    (cond ((equal s1 s2)
	   s1)
	  ((and (var s1) (var s2))
	   (match-objects (cdr (lookup s1))
			  (cdr (lookup s2))))
	  (t nil))))


;; this function may also be called at the top level.
;;
;; it takes whatevers that have been pulled out of the object table and tries to unify
;; them.  it returns either the unified object (its variable) or nil.
;;
;; NOTE we may make changes to the object table, but later find that these
;; two node are not resolvable.  that shouldn't be a problem.
(defun match-objects (node1 node2)
  (cond ((equal-types node1 node2)
	 (case (obj-type node1)
	   (:PROP (match-props node1 node2))
	   (:PATH (match-paths node1 node2))
	   (:DESCRIPTION (match-nouns node1 node2))))
	;; we're skipping :PREDs for now
	(t nil)))

;______________________________________________________________________________
;;; Match two propositional forms.

;; This takes the constraints for two speech acts, and checks to see if there
;; are any clashes.  Here is the place to do referencing, combining of paths
;; and any other extended matching.  Currently the two sets of constraints only
;; combine if each pair of constraints is equal (e.g. both are :*YOU* or nil).

(defun match-props (prop1 prop2)
  (let ((temp nil))
    (when (match-verb (rest (find-cl prop1)) 
		      (rest (find-cl prop2)))
	  (setf temp (match-nodes (find-edges prop1) (find-edges prop2)))
	  (when temp
		(check-edge (swap temp (find-edges prop1) prop1))))))
	

(defun match-nodes (edges1 edges2)
  (let ((result edges2) (current nil))
    (dolist (element edges1 result)
      (setf current (assoc (first element) result))
      (if current
	(let ((temp (match-arb (third current) (third element))))
	  (if temp 
	      (setf result (swap (swap temp (third current) current) current result))
	      (return nil)))
	(setf result (append result (list element)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a general description of :PATHs
;;
;;   a :PATH structure looks like:
;;   	(:PATH (:VAR :V8642)
;;   	  (:CONSTRAINT (:AND (:VIA :V8642 :V8646) (:TO :V8642 :V8672))))
;;   
;;   generic path structure:
;;   (:PATH (:VAR :VXXX) (:CONSTRAINT <list structure>))
;;   modifiers of :TO, :FROM, and :VIA (binary relations) --
;;   	  :AGAIN, :THEN, :ULTIMATE, :DIRECT
;;   
;;   modifier of :BEFORE (unary pred) is :IN-SEQUENCE.  this combo has been 
;;   witnessed only in the parsing of "before" alone, and probably should
;;   just be ignored.
;;   
;;   these predicates may be combined as (:AND p1 p2 .. pn).  
;;   
;;   they also may be modified, as in (:DIRECTLY p1).
;;   
;;   :DIRECTLY may also modify the conjunct, as in (:DIRECTLY (:AND ...))
;;   
;;   :LEX-AND, as :DIRECTLY, may take either primitive preds or conjuncts
;;   of preds or a mixture of the two:
;;   (:LEX-AND p1 (:AND p3 p4) p2)
;;   
;;   probably a bug: "go directly from A to B" produces a :DIRECTLY modifier
;;   of the "from A to B" path, but "go directly from A to B and from B to C"
;;   loses the :DIRECTLY entirely (and puts in the :LEX-AND).
;;   
;;   order is important here.  The paths are passed in in the order in which
;;   they were voiced.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE we will not attempt to process pronouns inside of these paths.
;;
;; for now, do almost nothing
;;
;; this function takes 2 :PATH structures (as described above) and returns
;; NIL if they both already have :FROM slots, otherwise a new var which points
;; to a :PATH in the object table that has the :AND of the constraint lists
;; for the 2 arguments.
;;
(defun match-paths (p1 p2)
  (cond ((and (has-from p1) (has-from p2))
	 nil)
	(t
	 (let ((newvar (keywordify (gentemp "V")))
	       (v1 (second (assoc :VAR (cdr p1))))
	       (v2 (second (assoc :VAR (cdr p2)))))
	   
	   ;; these next 3 sexps to update the table
	   (update (list v1 v2)
		   (list
		    newvar
		    :PATH
		    (list :VAR newvar)
		    (list :CONSTRAINT
			  (list :AND
				(second (assoc :CONSTRAINT (cdr p1)))
				(second (assoc :CONSTRAINT (cdr p2)))))))
	   (insert (list v1 newvar))
	   (insert (list v2 newvar))

	   ;; the retval
	   newvar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; noun objects look like an object type :DESCRIPTION with the following slots:
;; :VAR, :STATUS, :LEX (maybe), :SORT, :CLASS, and :CONSTRAINT (maybe)
;;
;; the :CONSTRAINT slot can have an arbitrarily complex subtree, which may
;; refer to other objects
;;
;; note that we "dummy-self" the object so we don't try to look ourselves
;; up in the symbol table
;;
;; this is called on 2 arbitrary description objects
;;
;; the retval is either nil or the variable that points to the MGU
;;
(defun match-nouns (x y)
  (let* ((ans (unify-slots (dummy-self (cdr x)) (dummy-self (cdr y))))
	 (var-ans (second (assoc :VAR ans)))
	 (ans (undummy-self ans var-ans))
	 (var-x (second (assoc :VAR (cdr x))))
	 (var-y (second (assoc :VAR (cdr y)))))
    (cond ((validity ans)

	   ;; all this to update the table
	   (update (list var-x var-y)
		   (cons var-ans (cons :DESCRIPTION ans)))
	   (insert (list var-x var-ans))
	   (insert (list var-y var-ans))

	   ;; remember the retval is the new var
	   var-ans)

	  ;; if we couldn't unify, return nil.
	  (t nil))))

;; Takes 2 lists of slots and vals from a noun-objects [basically the cdr
;; of (:DESCRIPTION (:STATUS X) (:VAR Y) ... )  ].
;;
;; we have specialized unifiers for the obligatory slots. :LEX is not obligatory,
;; and is highly dependent on :STATUS, so we unify those together.
;;
;; for :VAR, we just gensym a new variable name, and if the unification
;; fails it won't get used
;;
;; for the :CONSTRAINT slots, we have a more general unification, which flattens
;; out nested conjuncts and then compares vals by slot name.
;;
;; the retval is the resolution of the two objects, although unresolvable
;; slots will just be NIL.
(defun unify-slots (s1 s2)
  (append
   (unify-status-and-lex s1 s2)
   (list
    (list :VAR (keywordify (gentemp "V")))
    (unify-class (assoc :CLASS s1) (assoc :CLASS s2))
    (unify-sort (assoc :SORT s1) (assoc :SORT s2))
    )
   (if (or (assoc :CONSTRAINT s1) (assoc :CONSTRAINT s2))
       (list (unify-constraints (assoc :CONSTRAINT s1)
				(assoc :CONSTRAINT s2)))
     nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for unifying :STATUS and :LEX
;;

;; this is a guess at a hierarchy of what supercedes what in terms of status.
;; I didn't have a real good idea of how to put multiple inheritance in a LISP
;; structure, so I didn't bother.
;;
(let ((*status-hierarchy* 
       '(:ROOT
	 (:NAME 
	  (:DEFINITE 
	      (:INDEFINITE 
	       (:WH (:PRO))
	       (:THIS (:PRO))
	       (:THAT (:PRO)))))
	 (:DIRECT))
       ))
  
  ;; we're not checking if things are in the tree
  ;; (probably just returns nil)
  ;; very clunky- should probably be cleaned up
  ;;
  ;; returns either the more specific or nil if they're not comparable
  (defun resolve-status (s1 s2 &optional (hierarchy *status-hierarchy*))
    (if (equal s1 s2)
	s1
    (cond ((null hierarchy)
	   nil)
	  ((and (eql (car hierarchy) s1)
		(member-tree s2 (cdr hierarchy)))
	   s1)
	  ((and (eql (car hierarchy) s2)
		(member-tree s1 (cdr hierarchy)))
	   s2)

	  ;; neither status was right at the top
	  (t
	   (let ((vector (mapcar #'(lambda (subtree)
				     (and (member-tree s1 subtree)
					  (member-tree s2 subtree)))
				 (cdr hierarchy))))
	     (if (every 'null vector)
		 ;; if there is not a single subtree that holds both,
		 ;; they're either not in the tree or not comparable
		 nil
	       ;; there is a subtree that has both statuses in it, so recurse
	       (resolve-status s1 s2 (nth (position t vector) (cdr hierarchy)))))))))
	  

  ) ;; end scope *status-hierarchy*

;; :STATUS can be one of
;; (:THIS :DIRECT :THAT :WH :INDEFINITE :DEFINITE :PRO :NAME)
;;
;; :DIRECT only appears with objects of class :TIME-LOC
;;
;; we're taking the whole list of slots, as each arg, bc lex is so
;; dependent on status
;;
;; if they're the same status and they have :LEX fields, those fields *must* 
;; match.  (NOTE: we assume that only :NAME, :PRO, and :WH have :LEX fields.
;; 
;; whatever status survives keeps the :LEX field, if he has one, and if one
;; survives.  
;;
;; the retval is a list of the unified slots.  maybe just :STATUS, maybe :STATUS
;; and :LEX, or maybe a list of NIL.
(defun unify-status-and-lex (slots1 slots2)
  (let ((val1 (second (assoc :STATUS slots1)))
	(val2 (second (assoc :STATUS slots2))))
    (cond ((and (eq val1 val2)
		(member val1 '(:NAME :PRO :WH)))
	   (if (eql (second (assoc :LEX slots1))
		    (second (assoc :LEX slots2)))
	       (list (list :STATUS val1)
		     (assoc :LEX slots1))
	     (list nil)))
	  (t
	   (let ((ans (resolve-status val1 val2)))

	     ;; we know that the :STATUS fields must be different,

	     ;; if there was a resolution
	     (if ans

		 ;; if the first guy won
		 (if (eq ans val1)
		     ;; if that guy had a :LEX slot
		     (if (assoc :LEX slots1)
			 ;; make a list with the first guy's :STATUS and :LEX slots
			 (list (list :STATUS val1) (assoc :LEX slots1))
		       ;; otherwise just use his :STATUS slot
		       (list (list :STATUS val1)))

		   ;; otherwise, the second guy must've won
		   ;; if he had a :LEX slot
		   (if (assoc :LEX slots2)
		       ;; use it
		       (list (list :STATUS val2) (assoc :LEX slots2))
		     ;; otherwise, don't
		     (list (list :STATUS val2))))

	       ;; no resolution
	       (list nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unifying :CLASSes
;; 
;; 
;; return either nil or the resolved (:CLASS X) or the resolved (:CLASS (:OR ... ))
;; we return nil if the :CLASS is a :PRED.
;;
;; each arg is either (:CLASS X) or (:CLASS (:OR ... ))
(defun unify-class (c1 c2)
  (cond ((and (atom (second c1))
	      (atom (second c2)))
	 ;; they're both atoms, so let's just look for the more specific one
	 ;; (if it exists)
	 (let ((ans (more-specific (dekeywordify (second c1))
				   (dekeywordify (second c2)))))
	   (when ans
	     (list :CLASS (keywordify ans)))))

	((or (disjunct (second c1))
	     (disjunct (second c2)))
	 ;; if at least one is a disjunct of several possibilities, 
	 ;; we want the set of types
	 (let ((ans (find-common-types (de-disjunct (second c1))
				       (de-disjunct (second c2)))))
	   (when ans
	     (list :CLASS (cons :OR (mapcar 'keywordify ans))))))
	(t nil)))
	 

;; the sort slot can have one of these 3 vals:
;; (:STUFF :SET :INDIVIDUAL)
;; we simply require that the :SORT of our 2 objects be the same
(defun unify-sort (s1 s2)
  (if (equal s1 s2)
      s1
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Unification:
;;
;; we take a list of slots for each arg and compare by slot
;;
;; before we do that, though, we flatten out nested conjuncts
;; into one big list.  it is assumed that we have a list of 
;; constraints, not a tree.
;;
;; the retval is either nil or the unified list of constraints
(defun unify-constraints (c1 c2)
  (validity
   (let ((flat1 (flatten-conjuncts (second c1)))
	 (flat2 (flatten-conjuncts (second c2))))
     (mapcar #'(lambda (slot)
		 (resolve-constraint slot flat1 flat2))
	     (union (mapcar 'car flat1)
		    (mapcar 'car flat2)
		    :test 'equal	; in case of non-atomic slots
		    )))))

;; we know 'slot' appears in at least one of the constraint lists.
;; we're actually pulling out the val in case it's NIL.  which 
;; means we'll actually lose NIL values.  bad?
;;
;; although we use 'second' to access the "value" of the slot for the "standard"
;; slots, "values" here may have an arbitrary size
;;
;; the retval is either NIL or the resolved constraint
;;
;; NOTE that in most places the structure looks like (<slot> <value>). 
;; e.g. (:CLASS (:OR X Y))
;; BUT inside a constraint, we have sthg more like (<slot> <arg1> ... <argn>)
;; e.g. (:ASSOC-WITH X Y)
;; THUS we 'cdr the assoc, wheras in other places we would 'second it.
;;
;; NOTE also that the slot may be a disjunction, but that is not yet supported.
(defun resolve-constraint (slot c1 c2)
  (let ((val1 (cdr (assoc slot c1)))
	(val2 (cdr (assoc slot c2))))
    ;; if both objects have a constraint of this type
    (if (and val1 val2)
	;; and they are equal
	(if (equal val1 val2)
	    ;; just return it
	    val1
	  ;; if they're not equal, check if we can unify vars to make them equal
	  (if (not (member-tree nil (unify-over-vars val1 val2)))
	      ;; they match, so we can just use val1 for both
	      (list slot val1)
	    ;; we can't unify the vars, so return NIL
	    nil))

      ;; we didn't get answers in both, just take the one who
      ;; had a value
      (list slot (or val1 val2)))))

;; given 2 sexps, return the unification of them, where to unify a structure
;; you recursively unify the car and cdr.  when you encounter atoms, they must
;; either be equal or be variables which can be unified.
;;
;; it is also possible to have a disjunct of things.  In that case
(defun unify-over-vars (s1 s2)
  (cond ((and (null s1) (null s2))
	 nil)
	((and (atom s1) (atom s2))
	 (match-arb s1 s2))
	;; obviously, if we get here, they can't match
	((or (null s1) (null s2) (atom s1) (atom s2))
	 nil)
	;; if either are disjuncts, take what's common to both,
	;; allowing for variable unification
	((or (disjunct s1) (disjunct s2))
	 (let ((ans (find-common-vals (de-disjunct s1) (de-disjunct s2))))
	   (when ans
	     (cons :OR ans))))
	(t
	 (cons (unify-over-vars (car s1) (car s2))
	       (unify-over-vars (cdr s1) (cdr s2))))))


;; t of 1-deep lists
(defun flat-list (l)
  (or (null l)
      (every 'atom l)))

    
;; return the list if all elements are non-nil.
;; otherwise return nil.
(defun validity (list)
  (if (every 'identity list)
      list
    nil))

;; not currently used.  could be useful.
(defun remove-field (obj field)
  (remove-if #'(lambda (item)
		 (and (consp item) 
		      (eql (car item) field)))
	     obj))

;; are s1 and s2 set-equivalent?
(defun eq-sets (s1 s2 &key (test 'eq))
  (and (subsetp s1 s2 :test test)
       (subsetp s2 s1 :test test)))

;; meant to return the type of things like (:DESCRIPTION (..) ...)
;; or (:PROP ... )
(defun obj-type (n)
  (car n))

;; used in conjunction with the above to compare two objects
(defun equal-types (n1 n2)
  (eql (obj-type n1) (obj-type n2)))

;; given a list of slots, where one slot is a :VAR slot, replace every instance
;; of that :VAR value with *dummy-var*.
(defun dummy-self (slots)
  (subst-value *dummy-var* (second (assoc :VAR slots)) slots))

;; undo the above
(defun undummy-self (slots var)
  (subst-value var *dummy-var* slots))

;; is everything in the list eql?
;; amusingly, it even works on the empty list.
(defun all-eql (l)
  (reduce #'(lambda (x y)
	      (when (eql x y)
		x))
	  l :initial-value (car l)))

;; takes whatever follows :CONSTRAINT in an object description
;; and recursively turns (:AND X Y) into (X Y)
;; (we always return a list of answers so we can append so we can
;; be sure to remove all necessary levels of nesting.
(defun flatten-conjuncts (s)
  (when s
    (if (listp s)
	(if (eq (car s) :AND)
	    (apply 'append (mapcar 'flatten-conjuncts (cdr s)))
	  (list s))
      s)))
	
;; does the sexp look like (:OR ... ) ???
(defun disjunct (s)
  (and (consp s)
       (eq (car s) :OR)))

;; turns (:OR X Y) into (X Y), (X Y) into (X Y), and X into (X)
(defun de-disjunct (s)
  (cond ((disjunct s)
	 (cdr s))
	((atom s)
	 (list s))
	(t s)))

;; takes two semantic vals and returns the more specific one, nil if
;; not comparable
;; takes either symbols or keywords
(defun more-specific (s1 s2)
  (let ((sem1 (dekeywordify s1))
	(sem2 (dekeywordify s2)))
    (or (subtype 'sem sem1 sem2)
	(subtype 'sem sem2 sem1))))

;;;;; takes a path of the form (:PATH (:VAR X) (:CONSTRAINT ... ))
;;;;; and checks if it has a :FROM in its :CONSTRAINT list
;;;;;
;;;
;;;(defun has-from (p)
;;;  (member-tree :FROM p))


;; takes a list of lists and returns a list of every possibly combination
;; of their elements, in order.
;; i.e. (all-combos '((1 2) (x y))) => ((1 x) (1 y) (2 x) (2 y))
(defun all-combos (ll)
  (cond ((null (cdr ll))
	 (mapcar 'list (car ll)))
	(t
	 (apply 'append			; this flattens one level
		(mapcar
		 #'(lambda (e)		; e is our elem of the first list
		     (mapcar #'(lambda (lists) (cons e lists))
			     (all-combos (cdr ll))))
		 (car ll))))))

;; takes 2 lists of classes and returns a list of the types that
;; satisfy both lists
;;
;; we sort all the types wrt the type tree, then take out duplicates
;; (things are duplicates if they are comparable) starting from the
;; end, so we're sure to take out the more general of the pair.
;;
;; probably this should be rewritten
(defun find-common-types (l1 l2)
  (remove-duplicates
   (sort (copy-list (union l1 l2))
	 #'(lambda (x y)
	     (subtype 'sem x y)))
   :test 'more-specific
   :from-end t))

;; similar in concept to the above function, but bc of this
;; 'match-arb' function, we can write it simpler.
;;
(defun find-common-vals (l1 l2)
  (remove-if 'null
	     (mapcar #'(lambda (pair)
			 (match-arb (first pair) (second pair)))
		     (all-combos (list l1 l2)))))



