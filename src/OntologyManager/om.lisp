;;
;; Nate Chambers
;;
;; Load the Ontology Manager
;;


(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;OntologyManager;defsys")

(mk:load-system :om)
