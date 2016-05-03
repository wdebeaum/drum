;;;;
;;;; defsys.lisp for SimpleKB
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 20 Jun 2000
;;;; Time-stamp: <Thu Feb 22 11:40:15 EST 2007 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(dfc:defcomponent :simplekb
  :class dfc:trips-library
  :use (:common-lisp)
  :export (:<-
	   :kb-assert-fact
	   :kb-assert-fact-uniquely
	   :kb-assert-rule
	   :kb-prove
	   :kb-prove-all
	   :kb-retract
	   :kb-dump
	   :call
	   :bagof
	   )
  :system (:components ("auxfns"
			"patmatch"
			"unify"
			"prolog"
			"builtins"
			"api")))

;; Temporary compatibility with Test/test.lisp
(defun run ()
  (dfc:run-component :simplekb))
