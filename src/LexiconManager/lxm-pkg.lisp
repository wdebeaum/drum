;;;;
;;;; File: lxm-pkg.lisp
;;;; Creator: George Ferguson
;;;; Created: Thu Oct 29 16:22:50 2009
;;;; Time-stamp: <Fri Jan 15 15:15:00 EST 2010 ferguson>
;;;;

(unless (find-package :om)
  (load #!TRIPS"src;OntologyManager;om-pkg"))

(defpackage :lxm
  (:use :common-lisp :util :om)
  (:nicknames :lexiconmanager))

