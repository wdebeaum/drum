;;;;
;;;; predicate-handling.lisp
;;;;
;;;; Handles kr predicate definitions
;;;; Unlike kr-types, they don't have slots, instead, they have positional arguments

(in-package "ONTOLOGYMANAGER")

;; add the entry to class table, performing inheritance inference
;; and ajusting types associated with features
;; This is a wrapper for error handling
(defun add-predicate (name table &key (isa nil) (arguments nil))  
  (handler-case (add-predicate-entry name table isa arguments)
    (invalid-class-entry (err) 
      (krontology-warn "invalid preducate specification for ~S:~%  ~A" name err)
      nil)
    ))


;; add the entry to class table, performing inheritance inference
;; and ajusting types associated with features
(defun add-predicate-entry (name table isa args)
  (let ((parentstruct nil)
	(parentargs nil)
	)
    
    (when isa
      (setf parentstruct (gethash isa table))
      (when (null parentstruct)
	(error (make-system-condition 'invalid-class-entry
				 :format-control "Invalid predicate description ~S: Unknown isa value ~S " 
				 :format-arguments (list name isa))))
      (setf parentargs (copy-list (predicate-description-arguments parentstruct)))
      )
    
    (add-predicate-description
     (make-predicate-description :name name 
				 :arguments (merge-predicate-args args parentargs)
				 :lf-transforms nil
				 :isa isa
				 :children nil)
     table)
    ))


;; takes a class description and adds it to the table as a child of
;; the parent specified in the description
(defun add-predicate-description (preddescr predtable)
  (let ((parentname (predicate-description-isa preddescr))
	(cname (predicate-description-name preddescr))
	)
    (when parentname
      (let ((parentstruct (gethash parentname predtable)))
	(when (not parentstruct)
	  (error (make-system-condition 'invalid-class-entry
				   :format-control "Unknown isa name ~S" 
				   :format-arguments (list parentname))))
	;; here we are all ready to add the child to parent's list
	(pushnew cname (predicate-description-children parentstruct))	      
	))
    (sethash cname preddescr predtable)  
    ))


;; For now, the new slots override the defaults
;; No consistency checking at present!
(defun merge-predicate-args (args parentargs)
  " Merge slot lists from class definitions"
  (when (and (> (length parentargs) 0) (> (length args) 0) (not (eql (length args) (length parentargs))))
    (error (make-system-condition 'invalid-class-entry
				  :format-control "Parent and child have different number of arguments"
				  :format-arguments nil
				  ))
    )
  (or args parentargs)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for predicates subclass matching
;;


(defun subpred (subc superc krontology)
  "Determines if sub is a subtype of super. If krontology has a hierarchy table, performs fast hierarchical matching using subtype-in. Otherwise, walks the class tree"
  (if (kr-ontology-subtype-hierarchy krontology)
      (subtype-in subc superc (kr-ontology-subtype-hierarchy krontology))
    (descendant-class subc superc (kr-ontology-predicate-table krontology))
    ))


