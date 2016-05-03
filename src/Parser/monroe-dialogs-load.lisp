;;;;
;;;; monroe-dialogs-load.lisp: Load parser with extra defs for dialogs
;;;;
 
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Parser;defsys")

(in-package :parser)

;; Specific to parser
(setq trips::*default-scenario-directory*
     (directory-namestring #!TRIPS"src;OntologyManager;Data;Domains;monroe;"))

(setq *scenario-object-files* '("objects" "ProperNames"))

(mk:load-system :parser)

(setq lxm::*kr-in-lexicon* t)
(setq lxm::*use-domain-senses* nil)
