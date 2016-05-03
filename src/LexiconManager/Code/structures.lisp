; Lexicon-structures.lisp
;; Structure definitions for Myrosia's lexicon parser
;;

(in-package "LEXICONMANAGER")

;;(declaim (special *kr-ontology* *lf-ontology*))

(defvar *trace-lexicon-building* nil)    ;; tracing control

(defun lextraceon (&key kr) 
  (setq *trace-lexicon-building* t)
  (when kr (setq ontologymanager::*trace-transform-process* t))
  )
(defun lextraceoff () (setq *trace-lexicon-building* nil)
       (setq ontologymanager::*trace-transform-process* nil))

(defun lextraceon-p () *trace-lexicon-building*)

(defun kron ()
  (setq *kr-in-lexicon* t)
  (setq *use-domain-senses* t)
  )

(defun kroff ()
  (setq *kr-in-lexicon* nil)
  (setq *use-domain-senses* nil)
  )

(defun kron-p () *kr-in-lexicon*)

;; Added by myrosia - a top level lexical DB
;;

(defstruct lexicon-db
  word-table ;; the table indexed by morphological form of a word
  synt-table
  lf-table ;; table of pos indexed by lf type; used for compatibility checks on TextTagger input
;  lf-ontology unused
;  kr-ontology unused
  base-forms ;; the list of base word forms
  )

(defstruct vocabulary-entry
  name
  particle
  remaining-words
  word
  pos
  pronunciation
  wfeats ;; a list of parser features specific to the word
  senses
  boost-word ;; t if this is a "core" word, a domain-independent word to be treated identically with the domain-specialized entries for boosting/discounting
  abbrev ;; abbreviated form, if any
  )

(defstruct meta-data
  origin
  entry-date
  mod-date
  comments
  )

(defstruct basic-sense-definition
  "A basic structure used for abstract sense definitions and implemented word sense definitions with parameters filled in"
  pos
  lf
  sem
  pref
  (nonhierarchy-lf nil)
  syntax
  (boost-word nil)
  (meta-data nil)
  (prototypical-word nil) ;; t if this word is prototypical for its ontology class; otherwise nil
)

  
(defstruct (sense-definition (:include basic-sense-definition))
  lf-parent
  lf-form
  templ
  params
  )

;; a word sense definition - all the info required to construct the word entry
(defstruct (word-sense-definition (:include basic-sense-definition))
  word
  roles
  ;; syntax-semantics mappings
  mappings
  name
  remaining-words
  kr-type
  (specialized nil)
  (transform nil)
  (coercions nil)
  (operator nil) ;; This is set if the entry is obtained by doing a LF coercion
  )
  

(defstruct syntax-template
  name
  syntax
  mappings
)

(defstruct basic-synsem-map
  "A basic structure used for syntax-semantics mappings"
  name ;; LSUBJ etc
  slot ;; corresponding slot in LF
  optional ;; true or false
  maponly ;; true if the restriction is only for mapping but not generation, e.g iobj
  )

(defstruct (synsem-map (:include basic-synsem-map))
  paramname ;; name of the parameter or nil if this is just a restriction
  default ;; the default value of the parameter if nothing is provided by the lexicon definition. Will be set to the restriction for non-parametric mappings
  required ;; the common restriction present for all parameters - nil for non-parametric mappings
  )


(defstruct (word-synsem-map (:include basic-synsem-map))
  " A structure used in word synsemmap definitions, including instantiated parameters "
  varname ;; the variable associated with the argument
  syntcat ;; the argument cat
  syntfeat ;; the syntax features to be used 
)

(defstruct lex-entry
  "This structure is somewhat different than the \"lex-entry\" struct in the parser package but they serve the same purpose"
  id
  words
  pref
  description
  boost-word
  )

(define-condition invalid-templ (simple-condition) 
  ()
)

(define-condition invalid-vocab-entry (simple-condition) 
  ()
)

(define-condition invalid-vocab-ref (simple-condition) 
  ()
)

(define-condition inconsistent-lexicon (simple-condition)
  ()
  )


(defun make-constituent-spec (name feats &key (constit-sign '%))
  "Make a constituent specification using the parser constituent sign, name and features"
  (let ((res (list constit-sign name)))
    (if feats
	(append res feats)
      res
      )
    ))


;;
;; A common useful function
;;
(defun get-arg-value (arg list)
  (second (assoc arg list))
  )

(defun get-arg-value-list (arg list)
  (cdr (assoc arg list))
  )

(defun convert-to-syntax-package (symbol)
  "Takes a symbol, and interns it into the package for syntactic symbols, currently :W"
  (intern (symbol-name symbol) :w)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; **potential share?**
;; copied from Ontology/Code for reprogramming lexicon integration with ontology

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
  )

;; defined in OntologyManager and exported
;(defstruct sem-argument
; role ;; them role name
;  restriction ;; sem restriction
;  optionality ;; required, essential or optional
;  implements ;; tis is for extending e.g. AGENT implements Force
;  )
;
#|| ;; gf: 11/6/2003: This structure defined in OntologyManager
(defstruct feature-list
  type ;; the type of the list - may be a variable
  features ;; the feature list
  defaults ;; the default values to fill in after all possible inference is done
  )
||#

(defstruct coercion-operator-description
  name ;; name of the coercion operator
  argument ;; kr-type of the class that is being coerced
  result ;; kr-type of the result after the coercion has been applied
  lf ;; lf-type of the coercion result
  sem ;; sem of the coercion result
  )

(define-condition invalid-sem-spec (simple-condition) 
  ()
)
