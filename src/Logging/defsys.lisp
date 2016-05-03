;;;;
;;;; defsys.lisp : Defsystem for the TRIPS Lisp logging routines
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 17 Aug 1999
;;;; $Id: defsys.lisp,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

;; COMM package used for hack in chdir when running internally
(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(defpackage :logging
  (:use common-lisp)
  (:export initialize
	   log-message-received
	   log-message-sent
	   log-message
	   log-format
	   chdir))

(in-package :logging)

(mk:defsystem :logging
    :source-pathname #!TRIPS"src;Logging;"
    :components ((:file "logging")))
