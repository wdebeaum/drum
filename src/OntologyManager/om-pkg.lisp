;;;;
;;;; File: om-pkg.lisp
;;;; Creator: George Ferguson
;;;; Created: Fri Jan 15 15:13:08 2010
;;;; Time-stamp: <Fri Jan 15 15:13:11 EST 2010 ferguson>
;;;;

(defpackage :ontologymanager
  (:use :common-lisp :util)
  (:nicknames :om)
  ;; gf: 11/6/2003: Need to export these structure types since the LXM uses them
  (:export feature-list
	   make-feature-list copy-feature-list
	   feature-list-type feature-list-features feature-list-defaults
	   sem-argument
	   make-sem-argument
	   sem-argument-role sem-argument-restriction sem-argument-optionality sem-argument-implements
	   )
  )

