;;;; trips-ont-to-dsl-inh.lisp - load the entire old TRIPS ontology into the DSL library and write it back out in Lisp DSL format, with inheritance precomputed

;;;; FIXME the inheritance code here is messy and destructively modifies stuff, but a cleaner version with similar results ought to be integrated into the DSL library proper at some point

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
                       :name "trips")))
(load #!TRIPS"src;OntologyManager;om")
(load #!TRIPS"src;DeepSemLex;code;lib;defsys")
(dfc:load-component :deepsemlex)

(in-package :dsl)

(defmethod name ((x symbol)) x) ; ugh

;; not sure why this doesn't work as a function, let's try it as a macro instead
;(defun get-parent-of-type (child parent-type)
;  (first (eval-path-expression '(>inherit (when (lambda (c) (typep c parent-type)))) (list child))))
(defmacro get-parent-of-type (child parent-type)
  `(first (eval-path-expression '(>inherit (when (lambda (c) (typep c ,parent-type)))) (list ,child))))

(defun positive-variable-p (x)
  "Is x a positive variable like ?!sc ? These are variables that refuse to bind
   to nil or - ."
  (and (symbolp x)
       (let ((n (symbol-name x)))
         (and (< 2 (length n))
         (string= "?!" (subseq n 0 2))))))

(defun unmangle-wn-sense (ont-symbol)
  "Given a symbol like ONT::foo*1--02--03, return a proper WN sense symbol like
   WN::|foo%1:02:03::|. If the input isn't such a symbol, return nil instead.
   Assumes the sense isn't a satellite adjective (which has extra fields)."
  (let* ((str (symbol-name ont-symbol))
         (len (length str)))
    (when (and (> len 10) (char= #\* (char str (- len 10))))
      (let* ((lemma (subseq str 0 (- len 10)))
             (lex-sense (subseq str (- len 9)))
	     (ss-type (subseq lex-sense 0 1))
	     (lex-filenum (subseq lex-sense 3 5))
	     (lex-id (subseq lex-sense 7 9)))
      (intern
        (format nil "~(~a~)%~a:~a:~a::" lemma ss-type lex-filenum lex-id)
	:WN)))))

(defun feat-val-set (val)
  "Return the set of allowed feature value concepts given either a single
   feature value name or a disjunction thereof."
  (mapcar
    (lambda (val-name)
      (or ; ugh
        (gethash (intern (symbol-name val-name) :F) (concepts *db*))
        (gethash (intern (symbol-name val-name) :ONT) (concepts *db*))
	(unmangle-wn-sense val-name)
	(progn
	  (warn "bogus feature value: ~s" val-name)
	  val-name)
	))
    (if (typep val 'disjunction)
      (cdr val)
      (list val)
      )
    ))

(defun f-to-dsl-package (s)
  (if (string= "F" (package-name (symbol-package s)))
    (intern (symbol-name s) :dsl)
    s))

(defun set-to-maybe-disj (vals)
  "Inverse of feat-val-set; assumes vals nonempty."
  (let ((val-names (mapcar (lambda (v) (f-to-dsl-package (name v))) vals)))
    (if (= 1 (length val-names))
      (first val-names)
      `(w::or ,@val-names)
      )))

(defun normalize-sem-feat-pair (pair)
  (let ((feat (first pair))
        (val (second pair)))
    (list feat
	  (if (positive-variable-p val)
	    `(w::or
	      ;; get all possible values of this feature, except f::-
	      ,@(when (eq feat 'type) ; ugh
	          '(ont::referential-sem))
	      ,@(when (eq feat 'scale) ; ugh
	          '(ont::domain))
	      ,@(eval-path-expression
		  ;; NOTE: this assumes f::- is never a deeply-nested value in
		  ;; the hierarchy of a feature's values
		  `(,(intern (symbol-name feat) :f) <inherit (when
		     (lambda (c)
		       (let ((n (name c)))
			 (and (not (eq 'f::- n))
			      (string= "F" (package-name (symbol-package n)))
			      )))
		     )
		     name #'f-to-dsl-package)
		  (list nil))
	      )
	    (set-to-maybe-disj (feat-val-set val))))))

(defun normalize-sem-feats (sf)
  (dolist (p (features sf))
    (setf (second p) (second (normalize-sem-feat-pair p))))
  sf)

(defun subsumes-p (ancestor descendant)
  "Is ancestor an ancestor of descendant (or the same), according to >inherit?
   Also, t and ?variables always subsume, and ?!variables subsume all except
   f::-."
  (or (eq ancestor descendant) ; shortcut
      (string= "T" (symbol-name (name ancestor)))
      (and (util::variable-p ancestor)
           (not (and (positive-variable-p ancestor)
	             (eq 'f::- (name descendant)))))
      (and (typep ancestor 'concept) (typep descendant 'concept)
	(member ancestor (eval-path-expression '(* >inherit) (list descendant))
		:test #'eq))))

(defun hierarchical-intersection (as bs)
  "Like intersection, but respect >inherit relationships (as if they were is-a
   relationships)."
  (let (ret)
    (dolist (a as)
      (when (some (lambda (b) (subsumes-p b a)) bs)
        (pushnew a ret)))
    (dolist (b bs)
      (when (some (lambda (a) (subsumes-p a b)) as)
        (pushnew b ret)))
    ret))

(defun merge-sem-feat-vals (child-val parent-val)
  "Compute the effective value of a feature in a child by intersecting the
   effective parent value with the specified child value."
  (let* ((child-vals (feat-val-set child-val))
         (parent-vals (feat-val-set parent-val))
	 (effective-vals (hierarchical-intersection child-vals parent-vals)))
    (unless effective-vals
      ;(error "intersection of ~s and ~s is empty" child-val parent-val)
      (warn "~s ∩ ~s = ∅" child-val parent-val)
      (setf effective-vals child-vals) ; as evidenced by ONT::adequacy-val's F::scale feature conflicting with its ancestor ONT::domain-property's value
      )
    (set-to-maybe-disj effective-vals)))

(defun fl-type-set (sf)
    (declare (type sem-feats sf))
  "Get the set of names of feature list types (or t) from a sem-feats."
  (let ((fl-type (first (eval-path-expression '>inherit (list sf)))))
    (cond
      ((null fl-type)
        (list 'ld::t))
      ((typep fl-type '(disjunction concept))
        (mapcar #'name (cdr fl-type)))
      ((typep fl-type 'concept)
        (list (name fl-type)))
      (t
        (error "bogus feature list type: ~s" fl-type))
      )))

(defun replace-fl-type-set (sf new-flts)
    (declare (type sem-feats sf) (type list new-flts))
  "Replace the feature list type of sf (if necessary) to make its fl-type-set
   match new-flts."
  (let* ((new-flt-concepts 
	     (mapcar
	       (lambda (n)
		 (gethash n (concepts *db*)))
	       new-flts))
	 (new-flt
	   (if (= 1 (length new-flts))
	     (first new-flt-concepts)
	     `(w::or ,@new-flt-concepts))))
    ; TODO? check we're not replacing with the same thing
    ;; remove old >inherit
    (mapcar #'remove-relation (eval-path-expression '->inherit (list sf)))
    ;; add new
    (add-relation sf :inherit new-flt)))

(defun merge-sem-feats (child-sf parent-sf)
    (declare (type sem-feats child-sf parent-sf))
  "Modify child-sf by adding the non-conflicting parts of parent-sf to it /
   intersecting feature value sets."
  ;; merge fl-types
  (let* ((child-flts (fl-type-set child-sf))
         (parent-flts (fl-type-set parent-sf))
	 (new-child-flts (hierarchical-intersection child-flts parent-flts)))
    (when new-child-flts
      (replace-fl-type-set child-sf new-child-flts)))
  ;; merge features
  (loop with old-child-feats = (features child-sf)
        with parent-feats = (features parent-sf)
	with new-child-feats =
	  ;; copy each pair (and normalize values)
	  (mapcar #'normalize-sem-feat-pair old-child-feats)
	for (key parent-val) in parent-feats
	for child-pair = (assoc key new-child-feats)
	do (if child-pair
	     (setf (second child-pair)
		   (merge-sem-feat-vals (second child-pair) parent-val))
	     (push (list key parent-val) new-child-feats)
	     )
        finally (setf (features child-sf) new-child-feats)
	))

(defun copy-role-restr-map (m)
  (make-instance 'role-restr-map
      :roles (roles m)
      :restriction
        (if (typep (restriction m) 'sem-feats)
	  (normalize-sem-feats (restriction m))
	  (restriction m))
      :optional (optional m)))

(defun normalize-restriction (restr)
  "Ensure a role restriction is a sem-feats object."
  (unless (typep restr 'sem-feats)
    (let ((new-restr (make-instance 'sem-feats)))
      (unless (and (typep restr 'concept) (eq 'ld::t (name restr)))
        (add-relation new-restr :inherit restr)) ; FIXME this clutters up the :in slot of restr
      (setf restr new-restr)))
  (normalize-sem-feats restr)
  restr)

(defun denormalize-restriction (restr)
    (declare (type sem-feats restr))
  "Turn a role restriction from a sem-feats object back into a simpler
   expression if we can."
  (if (null (features restr))
    (or (first (eval-path-expression '>inherit (list restr)))
        (gethash 'ld::t (concepts *db*)))
    restr))

(defun old-sem-feat (new-feat)
  "Convert
   e.g. (dsl::foo (w::or dsl::bar dsl::baz))
   to   (f::foo (? foo f::bar f::baz))"
  (let* ((k (first new-feat))
         (new-val (second new-feat))
         (new-vals
	   (if (typep new-val 'disjunction) (cdr new-val) (list new-val)))
	 (dsl-pkg (find-package :dsl))
	 (old-vals
	   (mapcar
	     (lambda (nv)
	       (if (eq dsl-pkg (symbol-package nv))
		 (intern (symbol-name nv) :F)
		 nv))
	     new-vals)))
    (list
      (intern (symbol-name k) :f)
      (if (typep new-val 'disjunction)
	`(? ,k ,@old-vals)
	(first old-vals)
	)
      )))

(defun sem-feats-to-list (sf)
  "Convert a sem-feats concept to a typed sem list that OM can handle."
  (let ((fltype (intern (symbol-name (name (get-parent-of-type sf 'concept))) :F))
        (feats (mapcar #'old-sem-feat (features sf))))
    (cons fltype feats)))

(defun merge-role-restr-map (child-map parent-map)
    (declare (type role-restr-map child-map parent-map))
  "Modify child-map by adding the non-conflicting parts of parent-map to it.
   Also add type feature using om::best-lfs-from-sem if it is absent."
  ;; add parent's new roles
  (dolist (r (roles parent-map))
    (pushnew r (roles child-map)))
  ;; merge restrictions by normalizing them to sem-feats objects first
  (let ((child-restr (normalize-restriction (restriction child-map)))
        (parent-restr (normalize-restriction (restriction parent-map))))
    (merge-sem-feats child-restr parent-restr)
    ;; add type feature if it's missing
    (unless (assoc 'type (features child-restr))
      (let ((types (om::best-lfs-from-sem (sem-feats-to-list child-restr))))
        (cond
	  ((null types) nil)
	  ((= 1 (length types))
	    (push `(type ,@types) (features child-restr)))
	  (t
	    (push `(type (w::or ,@types)) (features child-restr)))
	  )))
    (setf (restriction child-map) (denormalize-restriction child-restr))
    )
  ;; keep child's optionality no matter what
  )

(defun merge-sem-frame (child-sf parent-sf)
    (declare (type sem-frame child-sf parent-sf))
  "Modify child-sf by adding the non-conflicting parts of parent-sf to it."
  (loop with old-child-maps = (maps child-sf)
        with parent-maps = (maps parent-sf)
	;; copy each role-restr-map
	with new-child-maps = (mapcar #'copy-role-restr-map old-child-maps)
	for parent-map in parent-maps
	for new-child-map =
	  (find-if
	    (lambda (m)
	      (intersection (roles m) (roles parent-map)))
	    new-child-maps)
	do
	  (if new-child-map
	    ;; child already has a map for this role, merge them
	    (merge-role-restr-map new-child-map parent-map)
	    ;; child doesn't have this role yet, add a copy of the parent's map
	    (push (copy-role-restr-map parent-map) new-child-maps))
	finally (setf (maps child-sf) new-child-maps)
	))

(defun do-inheritance (child parent)
    (declare (type concept child) (type (maybe concept) parent))
  (when parent
    ;; replace child's sem-feats with a version merged from parent's
    (let ((parent-sem-feats (get-parent-of-type parent 'sem-feats)))
      (if parent-sem-feats
        (let ((child-sem-feats (get-parent-of-type child 'sem-feats)))
	  (if child-sem-feats
	    (merge-sem-feats child-sem-feats parent-sem-feats)
	    (add-relation child :inherit parent-sem-feats)
	    ))
	;; no parent sem-feats, just normalize child feat vals
        (let ((child-sem-feats (get-parent-of-type child 'sem-feats)))
	  (when child-sem-feats
	    (normalize-sem-feats child-sem-feats)))
	))
    ;; replace child's sem-frame with a version merged from parent's
    (let ((parent-sem-frame (get-parent-of-type parent 'sem-frame)))
      (if parent-sem-frame
        (let ((child-sem-frame (get-parent-of-type child 'sem-frame)))
	  (if child-sem-frame
	    (merge-sem-frame child-sem-frame parent-sem-frame)
	    (add-relation child :inherit parent-sem-frame)
	    ))
	;; no parent sem-frame, just normalize child feat vals
        (let ((child-sem-frame (get-parent-of-type child 'sem-frame)))
	  (when child-sem-frame
	    (dolist (m (maps child-sem-frame))
	      (let ((r (restriction m)))
	        (when (typep r 'sem-feats)
		  (normalize-sem-feats r))))))
	))
    )
  ;; recurse on each child of child
  (loop for reln in (in child)
        when (eq :inherit (label reln))
	  do (do-inheritance (source reln) child))
  )
;(trace merge-sem-feats merge-sem-feat-vals feat-val-set hierarchical-intersection set-to-maybe-disj merge-sem-frame merge-role-restr-map)
;(trace do-inheritance)

(defun do-fltype-defaults (ont-name)
  "Apply default feature values for the feature list type of the sem-feats of
   the named ONT type. Must be called after do-inheritance so that the default
   values aren't used for inheritance and thus intersected."
  (let* ((ont-concept (gethash ont-name (concepts *db*)))
         (sf (get-parent-of-type ont-concept 'sem-feats))
         (fltype-name (symbol-name (name (get-parent-of-type sf 'concept))))
	 (fltype (gethash (intern fltype-name :F) (concepts *db*))))
    (when fltype ; we have a single fltype, not a disjunction
      (dolist (default-pair (features fltype))
	(unless (assoc (first default-pair) (features sf))
	  (push default-pair (features sf)))))))

(defun cl-user::run ()
  ;; load all TRIPS ontology/feature files
  (require-resource-version :ont)
  (require-resource-version :f)
  (let* (
         ;; get all concept names in the ONT package
         (ont-pkg (find-package :ont))
	 (f-pkg (find-package :f))
         (ont-names
	   (loop for name being the hash-keys of (concepts *db*)
	         when (member (symbol-package name) (list ont-pkg f-pkg))
		   collect name))
	 ;; set up environment for printing
         (*package* (find-package :ld))
         *current-provenance*)
    (do-inheritance (gethash 'ont::root (concepts *db*)) nil)
    (dolist (name ont-names)
      (do-fltype-defaults name))
    ;; set the current provenance and print it first so we don't print it for
    ;; each concept
    (ld::provenance ld::TRIPS)
    (format t "~s~%~%" *current-provenance*)
    ;; sort concepts alphabetically (why not?)
    (setf ont-names (sort ont-names #'string< :key (lambda (n) (format nil "~s" n))))
    ;; print all the concepts
    (dolist (name ont-names)
      (format t "~s~%~%" (listify (gethash name (concepts *db*)))))
    )
  (trips:exit 0))

