;;;;
;;;; loader.lisp for SimpleKB
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 9 Feb 2004
;;;; Time-stamp: <Mon Feb  9 14:56:31 EST 2004 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;SimpleKB;defsys")

(mk:load-system :simplekb)
