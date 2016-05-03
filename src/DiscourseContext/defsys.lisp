;;;;
;;;; defsys.lisp: Defsystem for DiscourseContext
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 25 Apr 1997
;;;; Time-stamp: <Fri Apr 18 10:58:31 EDT 2008 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

;; need the lf package defined
(unless (find-package :om)
  (load #!TRIPS"src;OntologyManager;defsys"))

(unless (find-package :parser)
  (load #!TRIPS"src;Parser;defsys"))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(dfc:defcomponent :discourse-context
  :use (:common-lisp :util)
  :nicknames (:dc)
  :system (:depends-on (:util :om)
	   :components (
			"overhead"
			"history"
			"lex"
			"handlers"
			"messages"
			"test"
			"DialogManager"
			"reference"
			"refKB"
			)))

(dfc:defcomponent-method dfc:init-component :after ()
  (initialize))

;; Need IM package for im::success and im::pre-reference in reference.lisp
;; Bring in after defcomponent (defpackage) to avoid dependency loop
(unless (find-package :im)
  (load #!TRIPS"src;NewIM;defsys"))

;; Temporary compatibility with Test/test.lisp
(defun run ()
  (dfc:run-component :discourse-context))
