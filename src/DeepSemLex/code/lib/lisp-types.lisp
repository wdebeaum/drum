;;;; Extensions to the Lisp type system

(in-package :dsl)

(defun list-of-predicate-name (member-type)
  (let* ((*package* (find-package :dsl)) ; ugh
         (predicate-name (intern (format nil "LIST-OF-~s-P" member-type) :type-predicates)))
      (unless (fboundp predicate-name)
	(eval `(defun ,predicate-name (x) (or (null x) (and (consp x) (typep (car x) ',member-type) (,predicate-name (cdr x)))))))
      predicate-name))

(deftype list-of (member-type)
  "Dependent list type. 'list was already taken."
  (if (eq t member-type)
    'list
    `(satisfies ,(list-of-predicate-name member-type))
    ))

(defun hash-predicate-name (from to)
  (let* ((*package* (find-package :dsl)) ; ugh
	 (predicate-name (intern (format nil "HASH-FROM-~s-TO-~s-P" from to) :type-predicates)))
    (unless (fboundp predicate-name)
      (eval
	  `(defun ,predicate-name (x)
	    (when (hash-table-p x)
	      (maphash
		  (lambda (k v)
		    (unless (and (typep k ',from) (typep v ',to))
		      (return-from ,predicate-name nil)))
		  x)
	      t))))
    predicate-name))

(deftype hash (&key (from 'symbol) (to t))
  "Dependent hash-table type."
  (if (and (eq t from) (eq t to))
    'hash-table
    `(satisfies ,(hash-predicate-name from to))
    ))

(deftype alist (&key (from 'symbol) (to t))
  "Dependent assoc-list type.
   e.g. (typep '((a 1) (b 2)) '(alist :from symbol :to integer)). Dotted pairs
   are not supported, sorry."
  `(list-of (cons ,from (cons ,to null))))

(deftype disjunction (&optional (member-type t))
  `(cons (eql W::or) (list-of ,member-type)))

(deftype conjunction (&optional (member-type t))
  `(cons (eql W::and) (list-of ,member-type)))

(deftype maybe-disj (&optional (member-type t))
  `(or ,member-type (disjunction ,member-type)))

#| this causes infinite recursion :(
(deftype disj-conj (&optional (terminal-type t))
  `(or ,terminal-type
       (disjunction (disj-conj ,terminal-type))
       (conjunction (disj-conj ,terminal-type))
       ))
|#
#| this causes problems when we want to load something not in DNF
;; instead, enforce disjunctive normal form (sorta)
(deftype disj-conj (&optional (terminal-type t))
  `(or ,terminal-type
       (conjunction ,terminal-type)
       (disjunction (or ,terminal-type
                        (conjunction ,terminal-type)))))
|#
;; instead, use satisifies
(defun disj-conj-p (x &optional (terminal-type t))
  "Return t iff x is of type terminal-type, or a disjunction or conjunction
   thereof, recursively."
  (or
    (typep x terminal-type)
    (and (consp x)
         (member (car x) '(w::and w::or))
         (every (lambda (y) (disj-conj-p y terminal-type)) (cdr x))
	 )
    ))

(defun disj-conj-predicate-name (terminal-type)
  (let* ((*package* (find-package :dsl)) ; ugh
         (predicate-name (intern (format nil "DISJ-CONJ-OF-~s-P" terminal-type) :type-predicates)))
    (unless (fboundp predicate-name)
      (eval `(defun ,predicate-name (x) (disj-conj-p x ',terminal-type))))
    predicate-name))

(deftype disj-conj (&optional (terminal-type t))
  `(satisfies ,(disj-conj-predicate-name terminal-type)))

(deftype maybe (just-type)
  `(or null ,just-type))

(defmacro defclass-simple (name superclasses doc-string &body slots)
  "A simpler version of defclass that always makes accessors and initargs, uses
   slot descriptions formatted like (type name &optional doc-string initform),
   and uses a class doc string more like defun/defmacro/defmethod."
  `(finalize-inheritance ; so that we can get slot names without first making an instance
     (defclass ,name ,superclasses
       ,(mapcar
	    (lambda (simple-slot)
	      (destructuring-bind (slot-type slot-name &optional 
				   doc-string
				   (initform nil initform-supplied-p)
				   )
	          simple-slot
		  (declare (type symbol slot-name)
		           (type (maybe string) doc-string))
		(let ((complex-slot
			 `(,slot-name
			   :accessor ,slot-name
			   :initarg ,(intern (symbol-name slot-name) :keyword)
			   :type ,slot-type
			   )))
		  (when doc-string
		    (nconc complex-slot `(:documentation ,doc-string))
		    (when initform-supplied-p
		      (nconc complex-slot `(:initform ,initform)))
		    )
		  complex-slot)))
	    slots)
       (:documentation ,doc-string)
       )))

(defun class-slot-names (cls-name)
  "Get the names of the slots of a given class."
  (mapcar #'slot-definition-name (class-slots (find-class cls-name))))

