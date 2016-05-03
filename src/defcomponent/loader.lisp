;;;;
;;;; loader.lisp for defcomponent
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu,  12 Jan 2004
;;;; Time-stamp: <Mon Feb  9 12:33:27 EST 2004 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;defcomponent;defsys")

(mk:load-system :defcomponent)
