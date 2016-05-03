;;;;
;;;; parser.lisp: Load the TRIPS Parser
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 25 Apr 1997
;;;; $Id: frag-parser.lisp,v 1.2 2008/04/18 14:47:21 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(setq trips::*default-scenario-directory*
  (namestring #!TRIPS"src;OntologyManager;Data;Domains;fruitcarts;"))

(load #!TRIPS"src;Parser;defsys")

;; Need IM for frag-parser (frag-attachments.lisp)
(load #!TRIPS"src;IM;defsys")

(mk:load-system :frag-parser)

(setq parser::*incremental-enabled* t)
