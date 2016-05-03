;;;;
;;;; predicate-handling.lisp
;;;;
;;;; Handles kr predicate definitions
;;;; Unlike kr-types, they don't have slots, instead, they have positional arguments

(in-package "ONTOLOGYMANAGER")

;; add the entry to class table, performing inheritance inference
;; and ajusting types associated with features
;; This is a wrapper for error handling
(defun add-operator (name table &key (isa nil) (arguments nil) (result nil) (coerce nil) (lf nil))  
  (handler-case (add-operator-entry name table isa arguments result coerce lf)
    (invalid-class-entry (err) 
      (krontology-warn "invalid preducate specification for ~S:~%  ~A" name err)
      nil)
    ))


(defun add-operator-entry (name table isa args result coerce lf)
  "add the entry to operator table, performing inheritance inference"
  (let ((parentstruct nil)
	(parentargs nil)
	(parenttype nil)
	)
    
    (when isa
      (setf parentstruct (gethash isa table))
      (when (null parentstruct)
	(error (make-system-condition 'invalid-class-entry
				 :format-control "Invalid operator description ~S: Unknown isa value ~S " 
				 :format-arguments (list name isa))))
      (setf parentargs (copy-list (operator-description-arguments parentstruct)))      
      (setf parenttype (operator-description-result parentstruct))
      )
    
    (when (and (not coerce) lf)
      (ontology-warn "The use of LF ~S in a non-coerce operator ~S will be ignored~%" lf name))
    (add-operator-description
     (make-operator-description :name name 
				 :arguments (merge-operator-args args parentargs)
				 :lf-transforms nil
				 :isa isa
				 :result (or result parenttype)
				 :children nil
				 :coerce coerce 
				 ;; Myrosia 08/19/03
				 ;; Removed the default. Now it will
				 ;; be properly handled by the
				 ;; coercion function calls depending
				 ;; on the result type
				 :lf lf
				 ;;:lf (and coerce (or lf (read-from-string "LF_ANY-SEM")))
				 )
     table)
    ))


(defun add-operator-description (descr table)
  " takes an operator description and adds it to the table as a child of
    the parent specified in the description"

  (let ((parentname (operator-description-isa descr))
	(name (operator-description-name descr))
	)
    (when parentname
      (let ((parentstruct (gethash parentname table)))
	(when (not parentstruct)
	  (error (make-system-condition 'invalid-class-entry
				   :format-control "Unknown isa name ~S" 
				   :format-arguments (list parentname))))
	;; here we are all ready to add the child to parent's list
	(pushnew name (operator-description-children parentstruct))	      
	))
    (sethash name descr table)  
    ))


;; For now, the new slots override the defaults
;; No consistency checking at present!
(defun merge-operator-args (args parentargs)
  " Merge argument descriptions from operator definitions"
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
;; Support for operators subclass matching
;;


(defun suboperator (subc superc krontology)
  "Determines if sub is a subtype of super. If krontology has a hierarchy table, performs fast hierarchical matching using subtype-in. Otherwise, walks the class tree"
  (if (kr-ontology-subtype-hierarchy krontology)
      (subtype-in subc superc (kr-ontology-subtype-hierarchy krontology))
    (descendant-class subc superc (kr-ontology-operator-table krontology))
    ))


