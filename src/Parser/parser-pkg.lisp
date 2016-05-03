;;;;
;;;; File: parser-pkg.lisp
;;;; Creator: George Ferguson
;;;; Created: Wed May 19 16:03:32 2010
;;;; Time-stamp: <Fri Jan 23 11:48:26 EST 2015 jallen>
;;;;

(defpackage :parser
  (:use :common-lisp :util :ontologymanager :lexiconmanager :w)
  (:export
#|| init-var-table  ;; (init-var-table): initializes the variable readtable
	   read-value      ;; (read-value X), reads a value without resetting the current variable readtable
	   read-expression ;; (read-expression X): reads a value after resetting the variable readtable first
	   subst-in        ;; (subst-in X bl): returns a new expression with variables in X bound according to the binding list bl
	   match-vals      ;; (match-vals feature val1 val2): probably no reason to use this
	   match-with-subtyping ;; (match-with-subtyping val1 val2): use this one to unify two values, returns a binding list
	   add-to-binding-list  ;; (add-to-binding-list bl1 bl2): merges two binding lists
	   make-binding-list    ;; (make-binding-list var value): creates a binding list with var bound to val
	   var-p           ;; (var-p X), true is X is a variable||#
	   ?
	   &
	   %
	   $))
