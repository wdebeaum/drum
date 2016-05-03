(in-package "PARSER")

;; UNIFICATION IN A TYPE HIERARCHY

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Role: "compiles" a type hierarchy for efficient unification

;; Interface functions:
;; 1. initialization (init-type-hierarchy)
;; 2. inclusion of a new "compiled" hierarchy into the global type 
;;    hierarchy represented as a hash table:
;;    (compile-hierarchy new-hierarchy)
;;    External representation of the new type hierarchy 
;;    new-hierarchy (noun/verb/preposition):
;;    - a nonleaf subtree is a list (node-label [child1 child2 ...])
;;    - a leaf is a list (leaf-label)
;;    E.g. (ANYTHING (PHYS-PHENOM) (MENTAL-PHENOM))
;;
;; 3. subtype unification:
;;    (subtype feat type1 type2)
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                                 Global Variables
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defvar *syntax-type-hierarchy* nil "the type hierarchy")
(defvar *syntax-hierarchical-features* nil)
(defvar *ontology-hierarchical-features* nil)
(defvar *syntax-hierarchies* nil "the syntactic features hierarchy")

;; Needed to support ontology loading
;;(defvar *lf-ontology* nil)
;;(defvar *kr-ontology* nil)

(defun init-type-hierarchy ()
  "A global interface function called when a new system instance needs to be loaded and hierarchical processing has to be initialized"
  (init-compiled-type-hierarchy)
  (setq *syntax-hierarchies* nil)
  )

(defun declare-syntax-hierarchical-features (ll)
  "Declares the given set of features as hierarchical"
  (setq *syntax-hierarchical-features* ll))

(defun declare-ontology-hierarchical-features (ll)
  "Declares the given set of features as hierarchical"
  (setq *ontology-hierarchical-features* ll))

(defun add-syntax-hierarchical-features (ll) 
  "Adds a given list of features to the known list of hierarchical features"
  (if (boundp '*syntax-hierarchical-features*)
      (setq *syntax-hierarchical-features* (union *syntax-hierarchical-features* ll))
    (setq *syntax-hierarchical-features* ll)
    ))

(defun add-ontology-hierarchical-features (ll) 
  "Adds a given list of features to the known list of hierarchical features"
  (if (boundp '*ontology-hierarchical-features*)
      (setq *ontology-hierarchical-features* (union *ontology-hierarchical-features* ll))
    (setq *ontology-hierarchical-features* ll)
  ))

;; features are assumed to be non-hierarchical unless explicitly
;; declared.

(defun syntax-hierarchical-feature-p (feat)
  "Determines if a given feature is hierarchical"
  (member feat *syntax-hierarchical-features*))
	
(defun ontology-hierarchical-feature-p (feat)
  "Determines if a given feature is hierarchical"
  (member feat *ontology-hierarchical-features*))

(defun init-compiled-type-hierarchy ()
  "An internal function called to initialize the compiled type hierarchy representation"
    (setq *syntax-type-hierarchy* (ontologymanager::new-type-hierarchy))
    )
#||
(defun add-type-hierarchy (h)
  "Adds a new tree to the current hierarchy"
  (ontologymanager::add-hierarchy h *syntax-type-hierarchy*)
  )
||#
;;;(defun add-kr-to-type-hierarchy (krontology)
;;;  (add-kr-ontology-to-hierarchy krontology *syntax-type-hierarchy*)  
;;;  )
;;;
;;;(defun add-ling-to-type-hierarchy (lfontology)
;;;  (add-ling-ontology-to-hierarchy lfontology *syntax-type-hierarchy*)  
;;;  )

(defun add-syntax-to-type-hierarchy (tree)
  (push tree *syntax-hierarchies*)
  (ontologymanager::add-hierarchy tree *syntax-type-hierarchy*)
  )
#||

(defun recompile-hierarchy (lfontology &optional krontology)
  (init-compiled-type-hierarchy)
  (mapcar (lambda (x) (ontologymanager::add-hierarchy x *syntax-type-hierarchy*)) *syntax-hierarchies*)
;;;  (ontology::add-ling-ontology-to-hierarchy lfontology *syntax-type-hierarchy*)
;;;  (when krontology 
;;;    (ontology::add-kr-ontology-to-hierarchy krontology *syntax-type-hierarchy*)
;;;    )
  )

||#
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                             Search/Unification
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun subtype (feat sub super)
  "If the feature is one that was declared to be hierarchical, check
type compatibility and return the most specific or nil.  Otherwise,
compare the two values with eq and return nil or their shared value."
  ;; Note: sub, super may come from different hierarchies
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond
   ((or (ontology-hierarchical-feature-p feat) (keywordp feat))
    ;; we should be conscious of lexical info here
    (let* ((sublf (ontologymanager::strip-lf-form sub ':*))
	   (superlf (ontologymanager::strip-lf-form super ':*)))
      (cond       
       ((eql superlf super)
	;; if super does not have a lexical form, then simple subtype matching on lfs works
	(or (and (ontologymanager::subtype sublf superlf)
		 sub)
	    (and (consp sublf) (eq (car sublf) 'ont::set-of)
		 (consp superlf) (eq (car superlf) 'ont::set-of)
		 (let ((xx (subtype feat (cadr sublf) (cadr superlf))))
		   (if xx (list 'ont::set-of xx))))))
       ((eql sublf sub)
	;; if super has lexical form, and sub does not, this is a definite fail
	nil)
       ((equal sub super) sub)
       (t ;; both sub and super have lexical form - they can only be subtypes if they are equal
	nil
	)
       )
      ))
   ((and (syntax-hierarchical-feature-p feat)
	 (ontologymanager::subtype-in sub super *syntax-type-hierarchy*))
    )
   ((eq feat 'w::lf) ;; special matching for lfs - ignores lexical
    ;; forms!!!  subtype is true either if two lfs are equal, or both
    ;; are the same LF-parent but one has lf-form and the other does
    ;; not
    (let* ((sublf (ontologymanager::strip-lf-form sub ':*))
	   (superlf (ontologymanager::strip-lf-form super ':*))
	   )
      (cond
       ((not (eq sublf superlf))
	nil)
       ((equal sub super)
	sub)
       ((eq sublf sub)
	superlf)
       ((eq superlf super)
	sublf)
       (t nil) ;; both have forms and different
       )
      ))
   ((eq sub super)
    sub)
   
   (t nil)
   ))
  
(defun subtype-check (a b)
  (subtype 'w::sem a b))

	
