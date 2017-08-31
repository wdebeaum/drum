;;;;
;;;; File: system.lisp
;;;; Creator: George Ferguson
;;;; Created: Thu Jul 12 20:09:24 2007
;;;; Time-stamp: <Mon Aug 18 22:04:32 EDT 2008 ferguson>
;;;;
;;;; Code for defining, loading, and running TRIPS systems.
;;;; Load this file, then define your system with DEF-TRIPS-SYSTEM,
;;;; load it with LOAD-TRIPS-SYSTEM, and run it with RUN-TRIPS-SYSTEM.
;;;; These have to be separate functions to support running in both
;;;; test mode (see test.lisp) and for real from a dump.
;;;;
;;;; See comments in RESTART-TRIPS-SYSTEM about verbose logging.
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

;; These next two only needed since we need to call them in RESTART-TRIPS-SYSTEM
(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :io)
  (load #!TRIPS"src;defcomponent;io;defsys"))

(in-package :trips)

(export '(def-trips-system
	   load-trips-system
	   run-trips-system
	   restart-trips-system
	   reload-trips-component
	   restart-trips-component
	   system-image-mode
	   ))

(defvar *trips-systems* nil
  "List of TRIPS systems defined using DEF-TRIPS-SYSTEM.")

(defvar *trips-system* nil
  "The name of the currently-loaded TRIPS system (if any).")

(defmacro def-trips-system (name &rest definition-body)
  "Record DEFINITION-BODY as the definition of the TRIPS system named NAME."
  `(setq *trips-systems* (acons ,name ',definition-body *trips-systems*)))

(defun load-trips-system (&optional (name (caar trips::*trips-systems*)))
  "Load the TRIPS system whose definition is named NAME.
The default is to load the first (usually the only) system defined by
DEF-TRIPS-SYSTEM."
  (let ((dfc::*component* nil) ; bind this so we don't just set the global var
	(definition (cdr (assoc name *trips-systems*))))
    (when (null definition)
      (error "No TRIPS system with name ~S" name))
    (setq *trips-system* name)
    (loop for def in definition
	  do (load-trips-component def))))

(defun load-trips-component (def)
  "Load the TRIPS component defined by DEF."
  (let ((type (pop def))
	(name (pop def)))
    (format *trace-output* "~&;; loading ~A: ~A~%" type name)
    (finish-output *trace-output*)
    (ecase type
      (:old-trips-component
       (let ((path (pop def)))
	 (load (make-pathname :directory (pathname-directory path)
			      :name "defsys"))
	 (mk:load-system name)))
      (:dfc-component
       (let ((path (pop def)))
	 (load (make-pathname :directory (pathname-directory path)
			      :name "defsys"))
	 (dfc:load-component name)))
      )))

(defun load-trips-parameters (&optional (name (caar trips::*trips-systems*)))
  "Load parameters from $TRIPS_BASE/etc/trips-$name/params.lisp, or
   $TRIPS_PARAMS_LISP if it's set (but only if the file exists)."
  (let ((params 
	  (or (trips:get-env "TRIPS_PARAMS_LISP")
	      (trips::make-trips-pathname
		  (format nil "etc;trips-~(~a~);params.lisp" name)))))
    (cond
      ((probe-file params)
	(format *trace-output* "~&;; loading parameters from ~S~%" params)
	(load params))
      (t
	(format *trace-output* "~&;; NOT loading parameters from ~S (file does not exist)~%" params))
      )))

(defun run-trips-system (&optional (name (caar trips::*trips-systems*)))
  "Run the TRIPS system whose definition is named NAME.
The default is to run the first (usually the only) system defined by
DEF-TRIPS-SYSTEM."
  (let ((definition (cdr (assoc name *trips-systems*))))
    (when (null definition)
      (error "No TRIPS system with name ~S" name))
    (load-trips-parameters name)
    (loop for def in definition
	  do (run-trips-component def)
	  ;; Short sleep apparently useful for something
	  (sleep 0.1))))

(defun run-trips-component (def)
  "Run the TRIPS component defined by DEF."
  (let ((type (pop def))
	(name (pop def)))
    (format *trace-output* "~&;; starting ~A: ~A~%" type name)
    (finish-output *trace-output*)
    (ecase type
      (:old-trips-component
       (let ((func (symbol-function (intern (symbol-name :run)
					    (find-package name)))))
	 (trips:process-run-function name func)))
      (:dfc-component
       (trips:process-run-function name #'dfc:run-component name))
      )))

(defvar *system-image-mode* :interactive
  "Flag set to distinguish running interactively and running from a dump.
Possible values are :INTERACTIVE and :DUMP. The default is :INTERACTIVE,
and is changed to :DUMP by RESTART-TRIPS-SYSTEM. See SYSTEM-IMAGE-MODE.")

(defun system-image-mode ()
  "Returns whether the system is running interactively (:INTERACTIVE) or
from a dump (:DUMP). This only works for images dumped and restarted using
the standard TRIPS mechanisms."
  *system-image-mode*)

(defun restart-trips-system (&optional (name (caar trips::*trips-systems*)))
  "Called when restarting a dumped image to do whatever (re-)initialization
is needed, then runs the NAMEd system (or the first one defined)."
  ;; Set delivery mode
  (setq *system-image-mode* :dump)
  (format *trace-output* "~&;; Lisp image mode is ~S~%" *system-image-mode*)
  ;; Re-initialize since environment may be different
  (format *trace-output* "~&;; Re-initializing Lisp IO...~%")
  (comm:initialize :transport :socket)
  (io:initialize :transport :socket)
  ;; Turn of verbose lisp-level logging for the dump
  ;; (Arguably shouldn't do this--should default off and turn it on
  ;; for testing, but history is against me on this one...)
  (setq comm::*verbose* nil)
  (setq io::*verbose* nil)
  ;; Force a gc (since we've just reloaded from a dump)
  (format *trace-output* "~&;; Initial Lisp gc...~%")
  (trips:gc t)
  ;; Start components
  (format *trace-output* "~&;; Starting all Lisp components...~%")
  (run-trips-system name)
  ;; Some lisps exit when the restart function exits...
  (trips:avoid-exiting))

(defun reload-trips-component (name)
  "Kill, reload, and restart the TRIPS component named NAME (in the
currently-running TRIPS system). See RESTART-TRIPS-COMPONENT."
  (restart-trips-component name :reload t))

(defun restart-trips-component (name &key (reload nil))
  "Kill, optionally reload, and restart the TRIPS component named NAME (in the
currently-running TRIPS system. Assumes that NAME is also the name of the
component's process (see RUN-TRIPS-COMPONENT)."
  (when (null *trips-system*)
    (error "No TRIPS system is running."))
  (let* ((sys (or (cdr (assoc *trips-system* *trips-systems*))
		  (error "No definition for TRIPS system: ~S" *trips-system*)))
	 (def (or (find name sys :key #'second)
		  (error "No TRIPS component named ~S in system ~S" name *trips-system*)))
	 (proc (or (trips:process-find name)
		   (error "No process for TRIPS component: ~S" name))))
    ;; Kill
    (format *trace-output* "~&;; killing ~A: ~S~%" name proc)
    (trips:process-kill proc)
    ;; Pause (not sure if this is needed)
    (sleep 1)
    ;; Optionally reload
    (when reload
      (load-trips-component def))
    ;; Restart
    (run-trips-component def)))
