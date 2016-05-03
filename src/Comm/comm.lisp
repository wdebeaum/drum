;;;;
;;;; comm.lisp: Inter-module communication for TRIPS
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; $Id: comm.lisp,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Comm;defsys")

(mk:load-system :comm)
