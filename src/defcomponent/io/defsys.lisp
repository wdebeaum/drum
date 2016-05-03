;;;;
;;;; defsys.lisp for defcomponent/io
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; $Id: defsys.lisp,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(defpackage :io
  (:use :common-lisp)
  (:export initialize
	   connect
	   send
	   recv
	   ready
	   shutdown
	   trace-comm))

(in-package :io)

(defvar *io-package* (find-package :io)
  "Useful variable to save calling FIND-PACKAGE later.")

(defvar *verbose* t
  "If true, message traffic is printed as it is sent and received. Set
to NIL if you want to trace individual connections separately (by tracing
PACKAGE::OUTPUT, probably).")

(mk:defsystem :io
    :source-pathname #!TRIPS"src;defcomponent;io;"
    :components ("misc"
		 "tracing"
		 ;; List specific transports first
		 "stdio"
		 "internal"
		 "socket"
		 ;; Then the main interface file
		 "api"))
