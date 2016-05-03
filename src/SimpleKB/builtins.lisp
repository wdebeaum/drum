;;;;
;;;; builtins.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 16 Feb 2004
;;;; Time-stamp: <Mon May  3 12:33:58 EDT 2004 ferguson>
;;;;

(in-package :simplekb)

(defun not/1 (args bindings other-goals)
  (if (prove (first args) bindings nil)
      fail
    (prove-all other-goals bindings)))

(setf (get 'not 'clauses) 'not/1)

(defun and/1 (args bindings other-goals)
  (prove-all (append args other-goals) bindings))

(setf (get 'and 'clauses) 'and/1)

(defun call/1 (args bindings other-goals)
  (prove-all other-goals
	     (prove (subst-bindings bindings (first args)) bindings)))

(setf (get 'call 'clauses) 'call/1)

(defun bagof/3 (args bindings other-goals)
  (let ((template (subst-bindings bindings (first args)))
	(goal (subst-bindings bindings (second args)))
	(bag (subst-bindings bindings (third args)))
	(matches nil))
    (let ((*results* nil)
	  (*max-solutions* nil))
      (declare (special *results* *max-solutions*))
      (prove-all (append (list goal) (list '(gather-prolog-vars))) bindings)
      (setq matches (mapcar #'(lambda (bindings)
				(subst-bindings bindings template))
			    (nreverse *results*))))
    (prove-all other-goals (unify bag matches bindings))))

(setf (get 'bagof 'clauses) 'bagof/3)
