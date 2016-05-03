;;;;
;;;; class-handling.lisp
;;;;
;;;; Handles kr type definitions

(in-package "ONTOLOGYMANAGER")

;; add the entry to class table, performing inheritance inference
;; and ajusting types associated with features
;; This is a wrapper for error handling

(defun add-class (name table &key (isa nil) (slots nil))
  
  (handler-case (add-class-entry name table isa slots)
    (invalid-class-entry (err) 
      (krontology-warn "invalid class specification for ~S:~%  ~A" name err)
      nil)
    ))


;; add the entry to class table, performing inheritance inference
;; and ajusting types associated with features
(defun add-class-entry (name table isa slots)
  (let ((parentstruct nil)
	(parentslots nil)
	)
    
    (when isa
      (setf parentstruct (gethash isa table))
      (when (null parentstruct)
	(error (make-system-condition 'invalid-class-entry
				 :format-control "Invalid class description ~S: Unknown isa value ~S " 
				 :format-arguments (list name isa))))
      (when (not (every #'consp slots))
        (error (make-system-condition 'invalid-class-entry
				 :format-control "Invalid slot specification in description ~S: ~S " 
				 :format-arguments (list name slots))))
      (setf parentslots (copy-list (class-description-slots parentstruct)))
      )
    
    (add-class-description
     (make-class-description :name name 
			     :slots (merge-slots slots parentslots)
			     :lf-transforms nil
			     :isa isa
			     :children nil)
     table)
    ))


;; takes a class description and adds it to the table as a child of
;; the parent specified in the description
(defun add-class-description (classdescr classtable)
  (let ((parentname (class-description-isa classdescr))
	(cname (class-description-name classdescr))
	)
    (when parentname
      (let ((parentstruct (gethash parentname classtable)))
	(when (not parentstruct)
	  (error (make-system-condition 'invalid-class-entry
				   :format-control "Unknown isa name ~S" 
				   :format-arguments parentname)))
	;; here we are all ready to add the child to parent's list
	(pushnew cname (class-description-children parentstruct))	      
	))
    (sethash cname classdescr classtable)  
    ))


;; For now, the new slots override the defaults
;; No consistency checking at present!
(defun merge-slots (slots defslots)
  " Merge slot lists from class definitions"
  (let ((result slots))
    (dolist (slot defslots result)
      (pushnew slot result :test (lambda (x y) (eql (car x) (car y))))      
  )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that support converting the lf tables into trees
;; Needed for hierarchical support through subtype function in unification
;;
;;
(defun subclass (subc superc krontology)
  "Determines if sub is a subtype of super. If krontology has a hierarchy table, performs fast hierarchical matching using subtype-in. Otherwise, walks the class tree"
  (if (kr-ontology-subtype-hierarchy krontology)
      (subtype-in subc superc (kr-ontology-subtype-hierarchy krontology))
    (or (descendant-class subc superc (kr-ontology-class-table krontology))
	(descendant-class subc superc (kr-ontology-predicate-table krontology))
	)
    ))
