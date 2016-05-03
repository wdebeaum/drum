(in-package "ONTOLOGYMANAGER")

;; UNIFICATION IN A TYPE HIERARCHY

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Role: "compiles" a type hierarchy for efficient unification

;; Interface functions:
;; (new-type-hierarchy) - makes a new type hierarchy structure with a fake root
;; (add-hierarchy tree hierarchy) - adds a new tree to the given hierarchy)
;; (subtypein sub super hierarchy) - determines if sub is a subtype of super in a given hierarchy
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                                 Global Variables
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(defparameter *common-root-name* 'common-hierarchy-root)

;; A structure representing a node with information needed for the fast subtype
;; matching algorithm
(defstruct type-node
  name ;; the node name
  minindex ;; the lowest child index
  maxindex ;; the highest child index
  )

;; A type hierarchy is represented by the root node and a type hash table of entries
(defstruct type-hierarchy
  root ;; the root node
  table ;; the type table 
  )

(defun new-type-hierarchy ()
  "Makes a new type hierarchy structure with a fake root and returns it"
  (let ((rootnode (make-type-node :name *common-root-name*
				  :minindex 0 :maxindex 0))
	(typetable (make-hash-table)))
    (setf (gethash *common-root-name* typetable) rootnode)
    (make-type-hierarchy
     :root rootnode :table typetable)
    ))

	
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                              Hierarchy access functions
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun add-hierarchy-type (type index hierarchy)
  "Adds a new type to the hierarchy with a given index (initially minindex=maxindex). Returns the node"
  (let ((table (type-hierarchy-table hierarchy))
	(node (make-type-node :name type :minindex index :maxindex index))
	)
    (when (gethash type table)
      (if *trap-bad-entries*
	  (break)
	(krontology-warn "type-hash-table already contains ~A" type)
	))
    (setf (gethash type table) node)
    node
    ))

(defun get-max-hierarchy-index (hierarchy)
  "Returns the maxindex of the current hierarchy"
  (when hierarchy
    (type-node-maxindex (type-hierarchy-root hierarchy))
    ))

(defun set-max-hierarchy-index (index hierarchy)
  "Sets the maxindex of the current hierarchy"
  (when hierarchy
    (setf (type-node-maxindex (type-hierarchy-root hierarchy)) index)
    ))

(defun get-hierarchy-type (type hierarchy)
  "search for element in hash table type-hash"
   (let ((elem (gethash type (type-hierarchy-table hierarchy))))
     (when (null elem)
       (hash-table-warn  elem)
       )
     elem))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                             Build Internal Rep
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(defun add-tree-at-index (tree index hierarchy)
  "Given a tree in (root (child 1) ... (child n)) format adds it to the hierarchy starting at a given index. Returns the maxindex of the new tree"
  (let ((res index)
	(type (first tree))
	(node nil)
	(children (rest tree))	
	)
    (setq node (add-hierarchy-type type res hierarchy ))
    (dolist (child children)
      (setq res (add-tree-at-index child (+ res 1) hierarchy))
      )
    ;; Now set the maxindex of the node to the maxindex of the rightmost child
    (setf (type-node-maxindex node) res)
    res))

(defun add-hierarchy (tree hierarchy)
  "Adds the tree as the rightmost child of the common hierarchy root. Returns the new hierarchy"
  (when tree
    (let* ((cindex (get-max-hierarchy-index hierarchy))
	 (nindex (add-tree-at-index tree (+ cindex 1) hierarchy))
	 )
    ;; The above statemend added a new tree to the hierarchy
    ;; Now we adjust the maxindex
      (set-max-hierarchy-index nindex hierarchy)))
  hierarchy
  )

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Search/Unification
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defvar *warned-already* nil)

(defun hash-table-warn (x)
  (when (and (not (member x *warned-already*)) x)
    (setq *warned-already* (cons x *warned-already*))
    (krontology-warn "type-hash-table doesn't contain ~A" x)
    (if *trap-bad-entries*  (break))
    ))

(defun more-general (node1 node2)
  "determines if node1 is more general than node2"
  (if (and (<= (type-node-minindex node1) (type-node-minindex node2))
	   (>= (type-node-maxindex node1) (type-node-maxindex node2))
	   )
      t
    nil))


(defun subtype-in (sub super hierarchy)
  "Check if sub is a subtype of super in a given hierarchy, return the most specific or nil."
  (let ((psub (get-hierarchy-type sub hierarchy))
	(psuper (get-hierarchy-type super hierarchy))
	)
    (cond           
     ((eql sub super) sub)		    
     ((or (null psub) (null psuper)) nil)
     ((more-general psuper psub) sub)
     (t nil))
    ))
	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions that support converting the class and predicate tavles into trees
;; Needed for hierarchical support through subtype function in unification
;;
;;

(defun find-class-roots (table)
  (let ((result nil))
    (maphash (lambda (key val)
	       (when (null (basic-class-description-isa val))
		 (push key result)))
	     table)
    result
    ))

(defun make-class-table-tree (table root)
  "Takes a kr class/predicate table and a root name and makes a tree out of it"
  ;; now this makes another tree of LF's and attaches it to the ontology
  (let ((roots (find-class-roots table))
        (tabletrees nil))
    (setf tabletrees (mapcar (lambda (x) (make-class-subtree (gethash x table) table nil)) roots))
    ;; now consistency checking here
    (cond
     (root ;; unique root provided by the caller
      (cons root tabletrees))
     ((null tabletrees) nil)
     ;; if there's no unique root, then we must have only 1 tree in the result
     ((eql (length tabletrees) 1)
      (first tabletrees))
     (t
      (krontology-warn "No unique root found in class or predicate table. Taking the first tree as the type hierarchy. The forest is ~S " tabletrees)
      (first tabletrees))
     )
    ))


(defun make-class-subtree (cl table allow-non-root)
  "Makes a subtree for a root entry or not a non-root entries if this is
allowed."
  (when (OR allow-non-root (not (basic-class-description-isa cl)))
    (append
     (list (basic-class-description-name cl))
     (mapcar #'(lambda (x)
		 (make-class-subtree (gethash x table) table t))
	     (basic-class-description-children cl)))))


(defun descendant-class (subc superc table)
  "Returns subc if subc is a descendant of superc or nil otherwise"
  (let ((subdescr (gethash subc table))
	(superdescr (gethash superc table))
	)
    (cond 
     ((null subdescr) nil)
     ((null superdescr) nil)
     ((eql (basic-class-description-isa subdescr) superc)
      subc)
     ((basic-class-description-isa subdescr)
      (descendant-class subc (basic-class-description-isa subdescr) table))
     (t nil)
     )
  ))

(defun find-superclass-if (pred class classtable)
  "Returns the class or the first of its superclasses that satisfies a given condition. Null if no parents satisfy the condition"
  (cond 
   ((null class) nil)
   ((funcall pred class) class)   
   (t (find-superclass-if pred (basic-class-description-isa (gethash class classtable)) classtable))
   ))
