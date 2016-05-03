(in-package "ONTOLOGYMANAGER")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structures for modules which use LF ontology
;;

(defstruct lf-description
  name ;; lf name
  sem  ;; sem features associated with the lf
  arguments ;; the list of sem-argument strucs associated with the LF 
  parent ;; LF parent - for reference
  children ;; list of children LF's - for reference
  kr-transforms ;; names of the rules to be used to transform to KR			
  slot-transforms ;; these are the slot transforms that use this LF
  default-kr-transforms ;; these will apply if the basic transforms don't go through
  coercions ;; the domain-independent coercions
  role-implementations ;; for each role, the list of other roles that implement it
  wordnet-sense-keys
;  (resource-mappings nil);; mappings to external resources corresponding to this ontology type
  comment
  )

(defstruct lf-coercion-operator
  lfname
  operator
  result
  resultsem)

(defstruct sem-argument
 role ;; them role name
  restriction ;; sem restriction
  optionality ;; required, essential or optional
  implements ;; tis is for extending e.g. AGENT implements Force
  )


(defstruct (featval-description 
	    (:print-function (lambda (p s k)
			       (declare (ignore k))
			       (format s "(Featval-description: :value ~S :implied ~S  "
				       (featval-description-value p) (featval-description-implied p)
				       )
			       (format s "     :arguments ~S      :parent ~S :children ~S     "
				       (featval-description-arguments p) 
				       (AND (featval-description-parent p) (featval-description-value (featval-description-parent p)))
				       (mapcar #'featval-description-value (featval-description-children p))
				       )
			       ;;			       (format s "     :declaring-types ~S :consistent-supertypes ~S" 
			       ;;       (featval-description-declaring-types p)
			       ;;       (featval-description-consistent-supertypes p)

			       (format s ")~%")
			       )
			     )
	    )
  feature-name ;; feature name
  value ;; feature value
  myimplied ;; values implied explicitly, not just inherited
  implied ;; other feature values implied - for consistency checking
  required ;; feature values required - will force feature values regardless of the type
  arguments ;; if any - sem-argument structure, description of the corresponding sem role
  parent ;; this is the "parent" value in the hierarchy
  children ;; these are the "children" values
  declaring-types ;; the types that declare the feature
  consistent-supertypes ;; the types consistent with the feature (i.e. declaring it or its parent and having all children consistent with it)
  
)

(defstruct feature-description
  name     ;; feature name
  root-value ;; the name of the root value
  defaults ;; for 1st, 2nd, third order
  values   ;; alist - names consed with featvale-descriptions
  (external nil) ;; this will be set to t if we do not want the feature in the ontology
  )

(defstruct feature-list
  type ;; the type of the list - may be a variable
  features ;; the feature list
  defaults ;; the default values to fill in after all possible inference is done
  )

(defstruct feature-type-description
  name ;; the type name
  features ;; the list of allowable features
  defaults ;; the list of features with defaults  
  )


(defstruct ling-ontology
  lf-table
  (default-lf-root nil) ;; the default root to be set on all entries
  lf-table-roots
  feature-table
  feature-type-table
  subtype-hierarchy ;; the hierarchy that can be used for subtype matching
  )


(define-condition invalid-feature-type-entry (simple-condition)
  ()
  )

(define-condition invalid-sem-spec (simple-condition)
  ()
  )

(define-condition inconsistent-feat-spec (simple-condition)
  ()
  )

(define-condition invalid-feat-spec (simple-condition)
  ()
  )

(define-condition invalid-lf-entry (simple-condition)
  ()
  )
