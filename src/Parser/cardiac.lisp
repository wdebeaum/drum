;;;;
;;;; File: Parser/cardiac.lisp
;;;; Creator: George Ferguson
;;;; Created: Thu Aug 30 11:24:16 2007
;;;; Time-stamp: <Thu Aug 30 11:26:11 EDT 2007 ferguson>
;;;;
;;;; Example of loading the parser with a non-default "scenario" (domain).
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(setq trips::*default-scenario-directory*
  (namestring #!TRIPS"src;OntologyManager;Data;Domains;cardiac;"))

(load #!TRIPS"src;Parser;defsys")

(mk:load-system :parser)
