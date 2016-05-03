;;;;
;;;; loader.lisp for defcomponent/logging
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; Time-stamp: <Mon Feb  9 12:15:54 EST 2004 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;defcomponent;logging;defsys")

(mk:load-system :logging)
