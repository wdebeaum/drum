;;;;
;;;; dc.lisp: Load the TRIPS DiscourseContext
;;;;
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;DiscourseContext;defsys")

(mk:load-system :discourse-context)
