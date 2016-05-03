;;;;
;;;; misc.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 12 Aug 1999
;;;; Time-stamp: <Mon Feb  9 12:11:41 EST 2004 ferguson>
;;;;

(in-package :io)

(defun convert-to-package (x package)
  "Converts X as follows:
If X is a symbol in the current package, return the symbol with the
  same print-name in PACKAGE.
If X is a list, apply CONVERT-TO-PACKAGE recursively.
Otherwise, including if X is a symbol in another package (including
  keywords), return it.
NOTE: This is different from the version of CONVERT-TO-PACKAGE before
      20-Jan-2004, which converted all symbols to the given PACKAGE."
  (cond ((null x)
	 nil)
	((and (symbolp x) (eq (symbol-package x) *package*))
	 (intern (symbol-name x) package))
	((listp x)
	 (cons (convert-to-package (car x) package)
	       (convert-to-package (cdr x) package)))
	(t x)))

(defun get-keyword-arg (l key)
  "Returns two values: First, the element following KEY in L, or NIL if
KEY is not presnet in L. The second value returned is T if KEY was present,
otherwise NIL (to disambiguate ``KEY NIL'' from KEY missing)."
  (cond ((and (consp l) (consp (cdr l)))
	 (if (eq key (car l))
	     (values (cadr l) t)
	   (get-keyword-arg (cdr l) key)))
	(t
	 (values nil nil))))

(defun set-keyword-arg (l key new-value)
  "Returns a copy of L, replacing the value following KEY with NEW-VALUE
if KEY is present in L, otherwise appending KEY and NEW-VALUE to L."
  (multiple-value-bind (value found) (get-keyword-arg key l)
   (declare (ignore value))
   (if found
       (replace-keyword-arg l key new-value)
     (append l (list key new-value)))))

(defun replace-keyword-arg (l key new-value)
  "Returns a copy of L with the argument following KEY replaced by
NEW-VALUE if it is present."
  (cond ((null l)
	 nil)
	((and (consp l) (consp (cdr l)) (eq key (car l)))
	 (cons key (cons new-value (copy-list (cddr l)))))
	(t
	 (cons (car l) (replace-keyword-arg (cdr l) key new-value)))))
