;;;;
;;;; File: history.lisp
;;;; Creator: George Ferguson
;;;; Created: Mon Feb 12 15:42:07 2007
;;;; Time-stamp: <Tue Feb 13 12:52:56 EST 2007 swift>
;;;;

(in-package :dc)

(defvar *acts* nil
  "List of all the speech acts in our history. This should presumably be
pruned at some length...")

(setq *acts* nil) ;; start with a clean slate at load time

(defun remember-act (act)
  "Save the given act in our history."
  (push act *acts*))

(defun clear-discourse-history ()
  "clear discourse acts"
  (setq *acts* nil))
