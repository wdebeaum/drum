;;;;
;;;; File: defsys.lisp
;;;; Creator: George Ferguson
;;;; Created: Mon Jan 18 12:52:38 2010
;;;; Time-stamp: <Mon Jan 18 14:15:43 EST 2010 ferguson>
;;;;
;;;; As of Jan 2010, the default scenario uses the "default-rules.lisp"
;;;; file.
;;;;

(in-package :om)

(mk:defsystem :lf-transforms
    :source-pathname #!TRIPS"src;OntologyManager;Data;LFTransforms;default;"
    :load-only t
    :components (
		 "default-rules"
		 ))
