;;;;
;;;; loader.lisp for DiscourseContext
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 12 Feb 2007
;;;; Time-stamp: <Mon Feb 12 15:50:56 EST 2007 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;DiscourseContext;defsys")

(mk:load-system :discourse-context)
