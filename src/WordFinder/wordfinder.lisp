;;;;
;;;; wordfinder.lisp: Load the TRIPS Word Finder
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 19 Mar 2003
;;;; $Id: wordfinder.lisp,v 1.2 2005/07/20 19:21:51 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;WordFinder;defsys")

(mk:load-system :wordfinder)
