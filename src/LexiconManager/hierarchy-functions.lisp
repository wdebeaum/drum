(in-package "LEXICONMANAGER")

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

(defvar *type-hierarchy* nil "the type hierarchy")
(defvar *hierarchical-features* nil)

;; Needed to support ontology loading
;;(defvar *lf-ontology* nil)
;;(defvar *kr-ontology* nil)

(defun init-type-hierarchy ()
  (setq *type-hierarchy* (ontology::new-type-hierarchy))
  )

(defun declare-hierarchical-features (ll)
  (setq *hierarchical-features* ll))

;; features are assumed to be non-hierarchical unless
;;  explicitly declared.

(defun hierarchical-feature-p (feat)
  (member feat *hierarchical-features*))
	

(defun add-type-hierarchy (h)
  "Adds a new tree to the current hierarchy"
  (ontology::add-hierarchy h *type-hierarchy*)
  )

(defun add-kr-to-type-hierarchy (krontology)
  (add-kr-ontology-to-hierarchy krontology *type-hierarchy*)  
  )

(defun add-ling-to-type-hierarchy (lfontology)
  (add-ling-ontology-to-hierarchy lfontology *type-hierarchy*)  
  )

(defun recompile-hierarchy (lfontology &optional krontology)
  (init-type-hierarchy)
  (ontology::add-ling-ontology-to-hierarchy lfontology *type-hierarchy*)
  (when krontology 
    (ontology::add-kr-ontology-to-hierarchy krontology *type-hierarchy*)
    )
  )


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
   ((hierarchical-feature-p feat)
    ;; we should be conscious of lexical info here
    (let* ((sublf (strip-lf-form sub ':*))
	   (superlf (strip-lf-form super ':*)))
      (cond
       ((eql superlf super)
	;; if super does not have a lexical form, then simple subtype matching on lfs works
	(and (subtype-in sublf superlf *type-hierarchy*)
	     sub))
       ((eql sublf sub)
	;; if super has lexical form, and sub does not, this is a definite fail
	nil)
       ((equal sub super) sub)
       (t ;; both sub and super have lexical form - they can only be subtypes if they are equal
	nil
	)
       )
      ))
   ((eq feat 'w::lf) ;; special matching for lfs - ignores lexical
    ;; forms!!!  subtype is true either if two lfs are equal, or both
    ;; are the same LF-parent but one has lf-form and the other does
    ;; not
    (let* ((sublf (strip-lf-form sub ':*))
	   (superlf (strip-lf-form super ':*))
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
  


	
