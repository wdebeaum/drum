;;;;
;;;; trips.lisp for config/lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 23 Jun 1997
;;;; $Id: trips.lisp,v 1.15 2015/02/06 21:33:45 lgalescu Exp $
;;;;

(setq *load-verbose* t)

;;;
;;; TRIPS package (exports any CL extensions)
;;;
(defpackage :trips
  (:use :common-lisp)
  (:export :exit :gc :get-env :make-socket
	   :process-run-function :current-process
	   :process-suspend :process-resume :process-kill :process-find
	   :process-alive-p
	   :avoid-exiting
	   :*break-on-errors*
	   :compiler-no-in-package-warning
	   :*scenario*))

(in-package :trips)

(defvar *break-on-errors* nil
  "If non-NIL, an error in a component's main loop (RUN function) will cause
break. If NIL, the default, then an error results in a SORRY reply and the
component restarts it's main loop (which may or may not be an ok thing to do).")

;;;
;;; Set TRIPS_BASE
;;;
(defvar *TRIPS_BASE* nil
  "The root of the TRIPS filesystem. This variable is set automagically
when the \"trips.lisp\" file is loaded.")

(eval-when (:load-toplevel :execute)
  (let* ((this-device (pathname-device *load-truename*))
         (this-directory (pathname-directory *load-truename*)))
    ;; We are in TRIPS_BASE/src/config/lisp
    (setq *TRIPS_BASE*
      (make-pathname :device this-device
                     :directory (append this-directory '(:up :up :up))))
    (format *trace-output* "~&;; *TRIPS_BASE* = ~S~%" *TRIPS_BASE*)))

;;;
;;; Load filesystem-specific pathname code
;;;
(load
 (make-pathname :device (pathname-device *TRIPS_BASE*)
		:directory (append (pathname-directory *TRIPS_BASE*)
				   '("src" "config" "lisp"
				     #+(and :mcl (not :openmcl)) "macos"
				     #+:mswindows "windows"
 				     #+(and
					(not (and :mcl (not :openmcl)))
					(not :mswindows)) "unix"))
		:name "pathname"))

;;;
;;; Define #! as shorthand for MAKE-TRIPS-PATHNAME
;;;
(defun sharp-bang-macro-char-handler (stream subchar arg)
  "Handles dispatching macro character #!.
Reads a token from STREAM, then decides what to do.
If the token is `TRIPS' (case-insensitive), then reads the following string
and calls MAKE-TRIPS-PATHNAME to parse it and return a pathname.
Otherwise signals an error."
  (declare (ignore subchar arg)) 
  (let ((token (read stream t nil t)))
    (if (and *read-suppress* (null token))
	;; gf: 13 Apr 2010: #- sets *READ-SUPRESS* to T and READ returns NIL
	(values)
	;; lg: 06 Feb 2015: in ANSI CL #'string-equal is case-insensitive
	;;     leaving this here out of an abundance of caution
	(let ((token-str (string-upcase (string token))))
	(cond
	  ((string-equal token-str "TRIPS")
	   ;; gf: 14 Aug 2006: Wrapped with VALUES for ecl
	   (values (make-trips-pathname (read stream t nil t))))
	  (t
	   (error "Unrecognized token following #!: ~S" token)))))))

(set-dispatch-macro-character #\# #\! #'sharp-bang-macro-char-handler)

;;;
;;; Load platform-specific code
;;;
(load
 (make-pathname :device (pathname-device *TRIPS_BASE*)
                :directory (append (pathname-directory *TRIPS_BASE*)
				   '("src" "config" "lisp"
				     #+:allegro "allegro"
				     #+(and :mcl (not :openmcl)) "mcl"
				     #+(and :openmcl (not :ccl)) "openmcl"
				     #+:ccl "ccl"
				     #+:cmu "cmucl"
				     #+:ecl "ecl"
				     #+:sbcl "sbcl"
				     #+:abcl "abcl"
				     #+:clisp "clisp"
				     ))
		:name "extensions"))

;;;
;;; Load portable defsystem
;;;
(load #!TRIPS"src;config;lisp;defsystem;defsystem")

;;;
;;; Specify scenario
;;;
(defvar *scenario* '|default|
  "Scenario to load when system is loaded.
This variable's value is used in pathnames when loading the scenario,
so watch its case in case-insensitive Lisps (and case-sensitive filesystems).
This value may be overriden in a system specification file.")
