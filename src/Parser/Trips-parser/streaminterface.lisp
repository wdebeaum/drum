(in-package parser)

;; these are some functions to get the proper chart result out of the parser
;; Myrosia's attempt to get the full "get-answers" reply from the parser
(defun get-answers-for-stream ()
  ;;  (format t "In get-answers-for-stream ~%") 
  ;; we attempt to undo the constituents in the entire tree
  ;;  (mapcar (lambda (x) (maptree #'undo-entry-constit x)) (get-answers))
  ;;  (format t "Done get-answers-for-stream ~%") 
  (get-answers)
  )

;; take an entry, and return a constit without a structure in it
(defun undo-entry-constit (c)
  (format t "In undo-entry-constit ~%") 
   (cond 
    ((null c) nil)
    ((entry-p c) (undo-constits (entry-constit c)))
    (t nil)
 ))

 ;; removes the element sof the tree that satisfy the condition
;; where the tree is represented as (<root-entry> subtree1 ... subtreen)
;; if the root entry satisfies it, the entire subtree will be removed
(defun remove-in-tree (func tree)
  (cond
   ((null tree) nil) ;; termination condition
   ((funcall func (first tree)) ;; if the root satisfies the condition, we remove the subtree
    nil)
   (t ;; otherwise we check the children
    (cons (first tree)
	  (mapcar (lambda (x) (remove-in-tree func x)) (rest tree))
	  ))
   ))

;; goes through the tree represented as (root subtree1 ... subtreen)
;; applying the function
(defun maptree (func tree)
  (cond 
   ((null tree) nil) ;;termination condition
   (t ;; otherwise we apply to root and map subtrees
    (cons (funcall func (first tree))
	  (mapcar (lambda (x) (maptree func x)) (rest tree))
	  ))
   ))

(defun punctuation-constit (c)
  (eql (constit-cat (entry-constit c)) 'punc)
  )


