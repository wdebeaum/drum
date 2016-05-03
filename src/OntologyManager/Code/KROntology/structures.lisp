(in-package "ONTOLOGYMANAGER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structures for modules using KR ontology
;;
;;

(defstruct basic-class-description
  name ;; class name
  isa  ;; an isa slot
  children ;; list of children classes for reference
  lf-transforms;; list of the mapping rule names to map down to LF  
)  
	  
(defstruct (class-description (:include basic-class-description))
  slots ;; list of slot defaults for James to use
  )

(defstruct KR-ontology
  class-table
  predicate-table
  operator-table
  map-table
  slot-table ;; the table with slot transforms
  krfeature ;; a feature containing kr type if LF interface is defined
  subtype-hierarchy ;; the hierarchical table to which all the entries were added (nil if subtype is not used)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Structures for modules which use both LF and KR ontology
;;

(defstruct lf-kr-transform 
  name
  parent
  lf
  arguments
  pattern ;; the breakup pattern
  (from-frame t)
  (to-frame t) ;; when set to T, is used to transform into a frame-like structure, otherwise to predicate with unnamed arguments
  default ;; if non-nil, will be applied to this LF if the form-based transformd don't work out
  abstract ;; set for transforms that are used only for role definitions and should never be applied themselves
  sem ;; transforms that apply only to a specified sem
  )

(defstruct transform-pattern
  lf ;; the lf name
  lf-form-default ;; the class under which all LF-FORMs should be contained
  pattern ;; either the atom corresponding to resulting type or a list describing how the arguments are to be rearranged
  (oblig nil) ;; for slot transforms, if set to t, requires the presence of the slot before the transform can be applied
  (coerce nil) ;; if set to something, a coercion can be performed with the specified list of operators
  )  

(defstruct (predicate-description (:include basic-class-description))
  "A structure which contains the  argument list"
  arguments
  )

(defstruct (operator-description (:include predicate-description))
  "An operator has an argument list and a typed result"
  result
  coerce
  lf ;; will be set to something with coerce T if we want to use a specific LF in coercion
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Conditions generated in different modules
;;


(define-condition invalid-class-entry (simple-condition)
  ()
  )

(define-condition invalid-mapping-entry (simple-condition)
  ()
  )

(define-condition invalid-argument-slot-mapping (simple-condition)
  ()
  )

(define-condition invalid-expression (simple-condition)
  ()
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common functions that support both class and predicate tables
;;

(defun ontology-warn (format-string &rest args)
  ;; gf: 7/6/98: Added fresh-line in a desparate attempt to get nice messages,
  ;;             since use of PARSER-WARN is completely inconsistent.
  ;; gf: 3/17/2000: Be consistent with other TRIPS modules
  (fresh-line *error-output*)
  (princ "ontology: warning: " *error-output*)
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :warn (apply #'format nil format-string args))
  (values))



