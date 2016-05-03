;;;;
;;;; lxm.lisp: Load the LexiconManager
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 25 Apr 1997
;;;; $Id: lxm.lisp,v 1.3 2005/08/04 17:21:04 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;LexiconManager;defsys")

(mk:load-system :lxm)
