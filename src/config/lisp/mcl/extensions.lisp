;;;;
;;;; extensions.lisp for config/lisp/mcl
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 29 Jul 2002
;;;; $Id: extensions.lisp,v 1.1.1.1 2005/01/14 19:47:58 ferguson Exp $
;;;;

(in-package :trips)

;;;
;;; Add nickname USER for package "COMMON-LISP-USER"
;; (Don't wipe out :cl-user specified by standard.)
;;;
(defpackage :common-lisp-user (:nicknames :user :cl-user))

;;;
;;; Define functions not part of CL
;;;
(let ((ccl:*warn-if-redefine* nil)
      (ccl:*warn-if-redefine-kernel* nil))

(defun exit (n &key quiet)
  (declare (ignore quiet))
  (error "Attempt to exit with return value ~D" n))

(defun gc (arg)
  (declare (ignore arg))
  (ccl:gc))

(defun make-socket (host port)
  #+(and :mcl (not :openmcl))
  (declare (ignore host port)) ; until I figure out what to do...
  #+openmcl
  (openmcl-socket:make-socket :remote-host host :remote-port port)
)

(defun process-run-function (name func &rest args)
  "Creates a new process named NAME and run FUNC on ARGS in it. Returns
the new process."
  (apply #'ccl:process-run-function name func args))

(defun current-process ()
  "Returns the currently-running process."
  ccl:*current-process*)

(defun process-add-arrest-reason (process object)
  "This function adds object to the list of arrest-reasons for process."
  (ccl:process-enable-arrest-reason process object))

(defun process-revoke-arrest-reason (process object)
  "This function removes object from the list of arrest reasons for process."
  (ccl:process-disable-arrest-reason process object))

) ;; End warn-if-redfine block

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This avoids having the output from the various TRIPS modules
;;; distributed over a bunch of per-process windows.
;;;

#-openmcl
(when (ccl:find-window "Listener")
  (setq *trace-output* (ccl:find-window "Listener"))
  (setq *error-output* (ccl:find-window "Listener")))

#||
;; This is what I was working on before I flashed on the above solution.
;; I'm leaving it here for possible future reference.

(defclass fixed-front-listener-terminal-io (ccl::front-listener-terminal-io)
  ())

(defmethod stream-current-listener ((s fixed-front-listener-terminal-io))
  ;; Could perhaps also get this once and store it in a variable
  (ccl:find-window "Listener"))
||#
