;;;;
;;;; defsys.lisp : Defsystem for the TRIPS Inter-module communication layer
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; $Id: defsys.lisp,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
;;;;
;;;; All TRIPS modules written in Lisp should use COMM:SEND and COMM:RECV
;;;; to exchange messages.
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(defpackage :comm
  (:use :common-lisp)
  (:export initialize connect send recv ready shutdown
	   trace-comm))

(in-package :comm)

(defvar *comm-package* (find-package "COMM")
  "Useful variable to save calling FIND-PACKAGE later.")

(defvar *verbose* t
  "If true, message traffic is printed as it is sent and received. Set
to NIL if you want to trace individual connections separately (by tracing
PACKAGE::OUTPUT, probably).")

(mk:defsystem :comm
    :source-pathname #!TRIPS"src;Comm;"
    :components ((:file "misc")
		 (:file "tracing")
		 ;; List specific transports first
		 (:file "stdio")
		 (:file "internal")
		 (:file "socket")
		 ;; Then the main interface file
		 (:file "interface")))
