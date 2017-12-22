;;;;
;;;; lxm.lisp: Load the LexiconManager
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 25 Apr 1997
;;;; $Id: lxm.lisp,v 1.4 2017/12/21 14:33:11 wdebeaum Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;LexiconManager;defsys")

;(mk:load-system :lxm)
(dfc:load-component :lxm)
