;;;;
;;;; im.lisp: Load the TRIPS Interpretation manager
;;;;
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;IM;defsys")

(im::load-system)
