;;;;
;;;; parser.lisp: Load the TRIPS Parser
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 25 Apr 1997
;;;; $Id: parser.lisp,v 1.4 2010/11/22 22:35:52 james Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Parser;defsys")

(dfc:load-component :parser)
