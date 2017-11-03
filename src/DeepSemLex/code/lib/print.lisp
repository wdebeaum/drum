;;;; print.lisp - methods for printing lexicon data

;;;; TODO
;;;; - standardize order of slots/relations

(in-package :dsl)

;; We print concepts and some other classes by listifying them and writing the
;; list.
(defmethods print-object ((c (or concept provenance input-text relation word morph morph-map)) s)
  (let ((*package* (find-package :ld)))
    (write (listify c) :stream s)))

;; FIXME this is also in ../converters/gloss-output-to-dsl.lisp
(defun repkg (sym &optional (new-pkg *package*))
  (intern (symbol-name sym) new-pkg))

(defmethod listify (x)
  (cond
    ;; Lists are already lists, but their items need to be listified.
    ((listp x)
      (mapcar #'listify x))
    ;; exception for quote operator
    ((member x '(dsl::quote common-lisp::quote)) x)
    ;; Convert symbols in DSL, CL, or LD packages to the current package
    ;; (usually LD or DSL).
    ((and (symbolp x)
          (member (symbol-package x)
	          (mapcar #'find-package '(dsl ld common-lisp))))
      (intern (symbol-name x)))
    ;; By default, non-lists (atoms) listify to themselves. This covers things
    ;; like explicitly packaged symbols, strings and numbers.
    (t x)
    ))

(defun slot-value-list-has-key-already-p (value-list alist-key)
  "Does value-list look like an alist entry for the given key already (t), or
   do we need to wrap it in a pair with the key (nil)?"
  (and (listp value-list)
       (symbolp (car value-list))
       (or (eq alist-key (car value-list))
	   (and (string= "OR" (symbol-name (car value-list)))
		(every (lambda (v)
			 (eq alist-key (car v)))
		       (cdr value-list)
		       )
		)
	   )
       ))

(defun listify-slots (o &optional slot-names)
    (declare (type standard-object o))
  "Get an alist corresponding to the slots and listified values of o. If a
   value's list form starts with the slot name, use that form directly rather
   than wrapping another list around it. Also do this if it's a disjunction of
   such forms."
  (mapcan (lambda (slot-name)
	    (when (and (slot-boundp o slot-name) (slot-value o slot-name)
	               (not (and (eq 'provenance slot-name)
		                 (eq *current-provenance*
				     (slot-value o slot-name))
				 ))
		       )
	      (list
		(let* ((*print-level* 0)
		       (value-list (listify (slot-value o slot-name)))
		       (alist-key (intern (symbol-name slot-name))))
		  (if (slot-value-list-has-key-already-p value-list alist-key)
		    value-list
		    (list alist-key value-list)
		    )))))
          (if slot-names
	    slot-names
	    (class-slot-names (type-of o))
	    )
	  ))

;; By default, objects listify to their type followed by an alist of their
;; slots.
(defmethod listify ((o standard-object))
  (cons (intern (symbol-name (type-of o))) (listify-slots o)))

(defmethod listify ((p provenance))
  (setf *current-provenance* p)
  (append
      (cons (intern (symbol-name (type-of p)))
	    (listify-slots p '(name version filename record-number)))
      (let (*current-provenance*) (mapcar #'listify (provenance p)))
      )
  )

(defmethods listify ((x (or input-text relation)))
  (let ((*current-provenance* *current-provenance*))
    (call-next-method)))

;; General concept listification. When *print-level* is 0, this just gets the
;; name of the concept, otherwise it gets the name followed by slots common to
;; all concepts. Subclasses of concept override this by appending to
;; (call-next-method) if it's a list.
(defmethod listify ((c concept))
  (if (and *print-level* (= 0 *print-level*) (not (anonymous-concept-p c)))
    (name c)
    (let* ((*current-provenance* *current-provenance*)
           (provenance
	     ;; need to listify up here so nested stuff sees the right
	     ;; *current-provenance*
	     (mapcar #'listify
	             (reverse
		       (if *current-provenance*
		          (remove *current-provenance* (provenance c)
		                  :test #'provenance-equalp)
			  (provenance c)
			  ))))
           relations
	   nested)
      ;; collapse relations with the same name, and separate out inheritance of
      ;; anonymous concepts so they are nested in this concept definition
      ;; instead of trying to relate to them by name
      (loop for r in (out c)
            for label = (intern (symbol-name (label r)))
	    do
	(cond
	  ((and (eq :inherit (label r))
		(or (typep (target r) '(disjunction concept))
		    (anonymous-concept-p (target r))))
		; FIXME not sure this is constrained enough
	    (push (listify (target r)) nested))
	  ((null (assoc label relations))
	    (push (list label (let ((*print-level* 0)) (listify (target r))))
	          relations))
	  (t
	    (push (let ((*print-level* 0)) (listify (target r)))
	          (cdr (assoc label relations))))
	  ))
      `(
	,(intern (symbol-name (type-of c)))
	,@(unless (anonymous-concept-p c) (list (name c)))
	,@(when (aliases c) (list (cons (repkg 'aliases) (aliases c))))
	,@(when (comment c) (list (list (repkg 'comment) (comment c))))
	,@provenance
	,@(mapcar (lambda (def)
		    (cons (repkg 'definition) (cdr (listify def))))
		  (reverse (definitions c)))
	,@(mapcar (lambda (ex)
		    (cons (repkg 'example) (cdr (listify ex))))
		  (reverse (examples c)))
	,@relations
	,@nested
	))))

(defmethods listify ((f (or sem-frame syn-sem)))
  (let ((parent-list (call-next-method))
        (*current-provenance*
	  (or (car (provenance f)) *current-provenance*)))
    (if (listp parent-list)
      (append parent-list (mapcar #'listify (maps f)))
      parent-list)))

(defmethods listify ((f (or sem-feats syn-feats)))
  (let ((parent-list (call-next-method))
        (*current-provenance*
	  (or (car (provenance f)) *current-provenance*)))
    (if (listp parent-list)
      (append parent-list (listify (features f)))
      parent-list)))

(defmethod listify ((m role-restr-map))
  `(,@(listify (if (= 1 (length (roles m))) (roles m) (list (roles m))))
    ,(listify (restriction m))
    ,@(when (optional m) (list (repkg 'optional)))
    ))

(defmethod listify ((e entailments))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (rules e))
      parent-list)))

(defmethod listify ((s semantics))
  (let ((parent-list (call-next-method))
        (*current-provenance*
	  (or (car (provenance s)) *current-provenance*)))
    (if (listp parent-list)
      (append parent-list
              (listify-slots s '(sem-frame sem-feats entailments)))
      parent-list)))

(defmethod listify ((m syn-sem-map))
  `(,(intern (symbol-name (syn-arg m)))
    ,(if (head-word m)
      (util::convert-to-package (list (listify (syn-cat m)) (head-word m)))
      (listify (syn-cat m))
      )
    ,@(when (sem-role m) (list (sem-role m)))
    ,@(when (optional m) (list (repkg 'optional)))
    ))

(defmethod listify ((s syntax))
  (let ((parent-list (call-next-method))
        (*current-provenance*
	  (or (car (provenance s)) *current-provenance*)))
    (if (listp parent-list)
      (append parent-list (listify-slots s '(template-call syn-sem syn-feats)))
      parent-list)))

(defmethod listify ((w word))
  (with-slots (first-word remaining-words particle) w
    (util::convert-to-package 
	(cond
	  (particle
	    `(word (,first-word ,@remaining-words (,particle))))
	  (remaining-words
	    `(word (,first-word ,@remaining-words)))
	  (t
	    `(word ,first-word))
	  ))))

(defmethod listify ((m morph))
  (append
      (cons (intern (symbol-name (type-of m)))
	    (listify-slots m '(pos)))
      (when (maps m)
	(list (listify (base-word m))))
      (when (irregularities m)
        (util::convert-to-package (irregularities m)))
      ))

(defmethod listify ((s sense))
  (let ((parent-list (call-next-method))
        (*current-provenance*
	  (or (car (provenance s)) *current-provenance*)))
    (if (listp parent-list)
      `(,(car parent-list)
        ,@(when (symbolp (second parent-list)) (list (second parent-list)))
        ,@(listify-slots s '(morph)) ; put morph at the top, it's important
	,@(if (symbolp (second parent-list))
	   (cddr parent-list)
	   (cdr parent-list)
	   )
        ,@(listify-slots s '(syntax semantics)))
      parent-list)))

