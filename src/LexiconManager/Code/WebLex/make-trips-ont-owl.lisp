;;;; make-trips-ont-owl.lisp - load TRIPS Lisp ontology data and write it back out as an OWL ontology (in Manchester syntax, .omn)
;;;; William de Beaumont
;;;; 2020-01-14

(defpackage :om (:use :cl))
(defpackage :f)
(defpackage :ont)
(in-package :om)

(defvar *om-data-dir*
   #P"../../../OntologyManager/Data/LFdata/"
   "Directory containing Lisp ontology data files.")
(defvar *lisp-file-names* '(
    ;; copied from OM defsys, :lfontology-data
    "feature-declarations"
    "feature-types"
    "root-types"
    "speech-acts"
    "predicates"
    "time-location-types"
    "physobj"
    "situation-types"
    "abstract-types"
    "property-val"
    "domain-and-attribute-types"
    "specific-situation-types"
    "social-contract"
    "music-terminology"
  )
  "Lisp ontology data files in the order they should be loaded.")
(defvar *out* nil "Output stream for writing OWL to.")

(defvar *ont-pkg* (find-package :ont))

(defvar *flt-names* nil)
(defvar *role-names* nil)
(defvar *type2children* (make-hash-table :test #'eq))

(declaim (ftype (function (t &optional t) list) get-names))

(defun write-name (sym &optional prefix)
  (cond
    ((consp sym) ; (? [!]var val1 val2 ...)
      (destructuring-bind (q var &rest vals) sym
	(unless (eq '? q)
	  (error "list where name expected, and it doesn't start with ?"))
	(let ((neg (char= #\! (char (symbol-name var) 0))))
	  (when neg (write-string "(not " *out*))
	  (cond
	    ((null vals) ; no values (only allowed in (? !var) )
	      (assert (string= "V" prefix))
	      (assert (char= #\! (char (symbol-name var) 0)))
	      (write-string "V_-" *out*))
	    ((null (cdr vals))
	      (write-name (car vals) prefix))
	    (t
	      (format *out* "(~{~a~^ or ~})" (get-names vals prefix)))
	    )
	  (when neg (write-string ")" *out*))
	  )))
    ;; turn ?!foo into (? !foo) so the previous case applies
    ((and (< 2 (length (symbol-name sym)))
          (string= "?!" (subseq (symbol-name sym) 0 2)))
      (write-name (list '? (intern (subseq (symbol-name sym) 1))) prefix))
    ; otherwise, single value
    ((or (null prefix) ; no prefix specified
         ;; or feature value that's in ONT:: instead of F::
	 (and (string= "V" prefix) (eq *ont-pkg* (symbol-package sym))))
      (write-string (string-downcase (symbol-name sym)) *out*))
    (t ; prefix specified
      (format *out* "~a_~(~a~)" prefix (symbol-name sym)))
    ))

(defun get-names (syms &optional prefix)
  (loop for sym in syms
	collect (with-output-to-string (*out*) (write-name sym prefix))))

(defun write-fv (fv)
  (write-name (car fv) "F")
  (write-string " some " *out*)
  (write-name (cadr fv) "V")
  )

(defun write-sem (sem)
  (write-string "(" *out*)
  (let* ((flt (car sem))
	 (feats (cdr sem))
	 (required (assoc :required feats))
	 (default (assoc :default feats))
	 (req-feats (if (or required default) (cdr required) feats)))
    ; TODO? what to do with default when we have it?
    (if (and (symbolp flt) (char= #\? (char (symbol-name flt) 0)))
      (write-string "root" *out*)
      (write-name (car sem) "FLT")
      )
    (loop for fv in req-feats
	  do (write-string " and " *out*)
	     (write-fv fv)
	  ))
  (write-string ")" *out*)
  )

(defun write-feature-hierarchy (parent children)
  (loop for (child . grandkids) in children
        do (write-string "Class: " *out*)
	   (write-name child "V")
	   (terpri *out*)
	   (write-string "  SubClassOf: " *out*)
	   (write-name parent "V")
	   (terpri *out*)
	   (terpri *out*)
	   (write-feature-hierarchy child grandkids)
	)
  (when (< 1 (length children))
    (format *out* "DisjointClasses: ~{~a~^, ~}~%~%"
	    (get-names (mapcar #'car children) "V")))
  )

(defmacro define-feature (name &key values name-only)
  ;; f::scale and :name-only t features can take ONT types as values, so their
  ;; range needs to be ONT::root instead of F::any-<name>
  (let* ((any-val (intern (concatenate 'string "ANY-" (symbol-name name)) :f))
         (range 
	   (cond
	     (name-only 'ont::root)
	     ((eq name 'f::scale) 'ont::domain) ; very special case
	     (t any-val)
	     ))
	 )
    (format *out* "# feature ~(~s~)~%" name)
    (unless name-only
      (write-string "Class: " *out*)
      (write-name any-val "V")
      (terpri *out*)
      (terpri *out*)
      (write-feature-hierarchy any-val values)
      )
    (write-string "ObjectProperty: " *out*)
    (write-name name "F")
    (format *out* "~%  Characteristics: Functional~%  Domain: root~%")
    (write-string "  Range: " *out*)
    (write-name range "V")
    (terpri *out*)
    (terpri *out*)
    )
  nil)

(defmacro define-feature-rule (name &key feature implies)
  (format *out* "# feature rule ~(~s~)~%" name)
  (write-string "Class: " *out*)
  (write-name (intern (concatenate 'string (symbol-name (car feature)) "_" (symbol-name (cadr feature))) :f) "FV")
  (terpri *out*)
  (write-string "  EquivalentTo: " *out*)
  (write-fv feature)
  (terpri *out*)
  (write-string "  SubClassOf: " *out*)
  (write-sem implies)
  (terpri *out*)
  (terpri *out*)
  nil)

(defmacro define-feature-list-type (name &key features defaults)
    (declare (ignore defaults)) ; TODO?
  (format *out* "# feature list type ~(~s~)~%" name)
  (pushnew name *flt-names*)
  (write-string "Class: " *out*)
  (write-name name "FLT")
  (terpri *out*)
  (dolist (f features)
    (write-string "  SubClassOf: " *out*)
    (write-name f "F")
    (write-string " some owl:Thing" *out*)
    (terpri *out*)
    )
  (terpri *out*)
  nil)

(defmacro define-type (name &key wordnet-sense-keys comment parent sem arguments definitions)
    (declare (ignore wordnet-sense-keys definitions)) ; TODO?
  (format *out* "# ONT type ~(~s~)~%" name)
  (write-string "Class: " *out*)
  (write-name name)
  (terpri *out*)
  (when comment
    (format *out* "  Annotations: rdfs:comment ~s~%" comment))
  (when parent
    (write-string "  SubClassOf: " *out*)
    (write-name parent)
    (terpri *out*)
    (push name (gethash parent *type2children*))
    )
  (when sem
    (write-string "  SubClassOf: " *out*)
    (write-sem sem)
    (terpri *out*)
    )
  (loop for (optionality role-name restriction) in arguments
        do
	   (pushnew role-name *role-names*)
	   (write-string "  SubClassOf: " *out*)
	   (write-name role-name "")
	   (write-string " only " *out*)
	   (cond
	     (restriction
	       (write-sem restriction)
	       (unless (eq :optional optionality)
		 (write-string " and " *out*)
		 (write-name role-name "")
		 (write-string " some owl:Thing" *out*)
		 )
	       )
	     (t ; no restriction
	       ;; FIXME this is meaningless; OWL's open-world assumption means anything can have any role unless we say otherwise
	       (write-string "owl:Thing" *out*))
	     )
	   (terpri *out*)
	)
  (terpri *out*)
  nil)

(with-open-file (*out* "trips-ont.omn" :direction :output :if-exists :supersede)
  (format *out* "Prefix: : <http://trips.ihmc.us/ont#>~%~%")
  (format *out* "Ontology: <http://trips.ihmc.us/ont>~%~%")
  (loop for fn in *lisp-file-names*
        for pn = (make-pathname :name fn :type "lisp" :defaults *om-data-dir*)
        do (load pn))
  (format *out* "# feature list types are disjoint~%")
  (format *out* "DisjointClasses: ~{~a~^, ~}~%~%"
	  (get-names *flt-names* "FLT"))
  (format *out* "# the children of each ONT type are disjoint~%~%")
  (loop for parent being the hash-keys of *type2children*
	  using (hash-value children)
	when (< 1 (length children))
	do (format *out* "# children of ~(~s~)~%DisjointClasses: ~{~a~^, ~}~%~%"
		   parent (get-names children)))
  (format *out* "# roles are object properties~%~%")
  (dolist (name *role-names*)
    (write-string "ObjectProperty: " *out*)
    (write-name name "")
    (terpri *out*)
    ; TODO? Characteristics: Functional? not sure that's true, since we have things like :agent1 :agent2 etc.
    (terpri *out*)
    )
  )

