;;;;
;;;; defsys.lisp for defcomponent/logging
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 17 Aug 1999
;;;; $Id: defsys.lisp,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(defpackage :logging2
  (:use common-lisp)
  (:export initialize
	   log-message-received
	   log-message-sent
	   log-message
	   log-format
	   chdir))

(in-package :logging2)

(mk:defsystem :logging2
    :source-pathname #!TRIPS"src;defcomponent;logging;"
    :components ("logging"))
