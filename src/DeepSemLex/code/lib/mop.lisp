(in-package :dsl)

;; import implementation-specific MOP stuff
(let ((mop-pkg
	#+abcl :mop
	#+allegro :hcl
	#+ccl :ccl
	#+clisp :clos
	#+cmu :pcl
	#+ecl :clos
        #+sbcl :sb-mop
	))
  (import (mapcar (lambda (f) (find-symbol f mop-pkg))
                  '("FINALIZE-INHERITANCE"
		    "SLOT-DEFINITION-NAME"
		    "CLASS-SLOTS"
		    ))))

;;; defmethodS

(defun cartesian-product (lists)
  "Return the list of lists of one item from each of the given lists."
  (reduce
     (lambda (new-list product-so-far)
       (mapcan
           (lambda (new-list-item)
	     (mapcar
		 (lambda (product-so-far-item)
		   (cons new-list-item product-so-far-item))
		 product-so-far
		 ))
	   new-list
           ))
     lists
     :initial-value '(nil)
     :from-end t
     ))

(defun parameter-disjunctively-specialized-p (p)
  "Is the given required method parameter specification specialized to a
   disjunctive type?"
  (and (consp p)
    (consp (second p))
    (symbolp (car (second p)))
    (string= "OR" (symbol-name (car (second p))))
    ))

(defmacro defmethods (method-name parameter-list &body body)
  "Like defmethod, but allow OR specializers in the parameter list, expanding
   to multiple defmethods, one for each combination of values from each OR."
  (let* ((disjuncts
           (loop for p in parameter-list
	         until (member p '(&optional &rest &key &allow-other-keys &aux))
		 when (parameter-disjunctively-specialized-p p)
		   collect (cdr (second p))
		 ))
	 (options (cartesian-product disjuncts))
	 )
    `(progn
      ,@(mapcar
          (lambda (option)
	    `(defmethod ,method-name 
	       ,(loop with parameter-required = t
	              for p in parameter-list
		      when (member p '(&optional &rest &key &allow-other-keys
		                       &aux))
		        do (setf parameter-required nil)
		      collect
			(if (and parameter-required
			         (parameter-disjunctively-specialized-p p))
			  (list (car p) (pop option))
			  p))
	       ,@body))
	  options)
      )))

