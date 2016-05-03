;;;;
;;;; loader.lisp for defcomponent/io
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; Time-stamp: <Mon Feb  9 12:07:47 EST 2004 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;defcomponent;io;defsys")

(mk:load-system :io)
