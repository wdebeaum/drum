;;;;
;;;; util.lisp: Utilities for dealing with keyword argument lists, etc.
;;;;
;;;; James Allen, james@cs.rochester.edu, 20 Jul 1999
;;;; Timestamp: <>
;;;;
;;;; These routines are used by many different modules.
;;;;

(in-package "UTIL")

(defun sa-type (sa)
  "Returns the car of SA if its a list, else NIL."
  (and (listp sa) (car sa)))

(defun find-arg (sv-list argname)
  "Assumes SV-LIST is list of key-value pairs, and returns the value following
ARGNAME, or NIL."
  (if sv-list
      (if (eq (car sv-list) argname)
	  (cadr sv-list)
	;; Note: Using CDDR here means list better be strictly key-value pairs
	(find-arg (cddr sv-list) argname))))

(defun find-arg-in-act (act argname)
  "Assumes ACT is a list consisting of a leading atom followed by
keyword-argument pairs. Returns the argument corresponding to ARGNAME, or NIL."
  (find-arg (cdr act) argname))

(defun find-all-args-for-slot (prop role)
  (when prop
    (if (eq (car prop) role)
	(cons (cadr prop) (find-all-args-for-slot (cddr prop) role))
     (find-all-args-for-slot (cddr prop) role))))


(defun insert-arg-in-act (structure role value)
  "inserts :role value as first argument"
  (cons (car structure)
        (append (list role value) (cdr structure))))

(defun remove-arg-in-act (act argname)
  (cons (car act)
        (remove-arg (cdr act) argname)))

(defun remove-arg (sv-list argname)
  "Assumes SV-LIST is list of key-value pairs, and returns a new list whose
contents are the same as SV-LIST except that ARGNAME and its corresponding
value are removed, if they are present.
Returns two values: the updated sv-list and a list of values for the argname removed"
  (when sv-list
    (multiple-value-bind
	  (newargs removed)
	(remove-arg (cddr sv-list) argname)
      (if (eq (car sv-list) argname)
	  (values newargs (cons (cadr sv-list) removed))
	  (values (if (eq newargs (cddr sv-list))
		      sv-list
		      (list* (car sv-list) (second sv-list) newargs))
		  removed)))))

(defun remove-args (sv-list argnames)
  "same as remove-arg but for a list of roles names"
  (when sv-list
    (multiple-value-bind
	  (newargs removed)
	(remove-args (cddr sv-list) argnames)
      (if (member (car sv-list) argnames)
	  (values newargs (list* (car sv-list) (cadr sv-list) removed))
	  (values (if (eq newargs (cddr sv-list))
		      sv-list
		      (list* (car sv-list) (second sv-list) newargs)
		      )
		  removed)))))
 
(defun replace-arg-in-act (act argname newvalue)
  "Assumes ACT is a list consisting of a leading atom followed by
keyword-argument pairs. Returns a new list whose contents are the same
as ACT except that the value corresponding to ARGNAME is replaced with
NEWVALUE, if ARGNAME is present."
  (cons (car act)
	(replace-arg (cdr act) argname newvalue)))

(defun replace-arg (sv-list argname newvalue)
  "Assumes SV-LIST is list of key-value pairs, and returns a new list whose
contents are the same as SV-LIST except that the value corresponding to
ARGNAME is replaced with NEWVALUE, if ARGNAME is present."
  (if sv-list
      (if (eq (car sv-list) argname)
	  (cons argname
		(cons newvalue
		      (cddr sv-list)))
	  (if (cadr sv-list)
	      (cons (car sv-list)
		    (cons (cadr sv-list)
			  (replace-arg (cddr sv-list) argname newvalue)))
	      (append sv-list (list (list argname newvalue)))))
    (list argname newvalue)))


;;  General flatten utility function

(defun flatten (lis)
  (if lis
      (if (listp (car lis))
	  (append (flatten (car lis)) (flatten (cdr lis)))
	(cons (car lis) (flatten (cdr lis))) )
    nil ))

;;  KEYWORDS

(defvar *keyword-package* (find-package 'keyword))

(defun keywordify (item)
  (cond ((and item (symbolp item)) (intern (string item) *keyword-package*))
	((consp item) (cons (keywordify (car item))
			    (keywordify (cdr item))))
	(t item)))





