;;;;
;;;; extensions.lisp for config/lisp/openmcl
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 29 Jul 2002
;;;; $Id: extensions.lisp,v 1.6 2009/01/21 20:23:57 wdebeaum Exp $
;;;;
;;;; Native threads in OpenMCL 0.14 and later are indicated by the
;;;; presence of :OPENMCL-NATIVE-THREADS on *FEATURES*.
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
  (format *trace-output* "~&;; Lisp exiting~%")
  (finish-output *trace-output*)
  (ccl:quit n))

(defun get-env (var)
  (ccl::getenv var))

(defun gc (arg)
  (declare (ignore arg))
  (ccl:gc))

(defun make-socket (host port)
  (openmcl-socket:make-socket :remote-host host :remote-port port)
)

(defun process-run-function (name func &rest args)
  "Creates a new process named NAME and run FUNC on ARGS in it. Returns
the new process."
  (apply #'ccl:process-run-function name func args))

(defun current-process ()
  "Returns the currently-running process."
  ccl:*current-process*)

(defun process-suspend (process)
  "Suspends PROCESS."
  #+openmcl-native-threads
  (ccl:process-suspend process)
  #-openmcl-native-threads
  (ccl:process-enable-arrest-reason process :suspended))

(defun process-resume (process)
  "Resumes PROCESS."
  #+openmcl-native-threads
  (ccl:process-resume process)
  #-openmcl-native-threads
  (ccl:process-disable-arrest-reason process :suspended))

(defun process-alive-p (process)
  "Is the process alive (i.e. it hasn't ended and hasn't been killed)?"
  (ccl:process-active-p process))

(defun process-kill (process)
  "Kill the process."
  (ccl:process-kill process))

(defun process-find (name)
  "Returns the process with the given NAME."
  (find name (ccl:all-processes)
	:key #'(lambda (p) (slot-value p 'ccl::name))))

(defun avoid-exiting ()
  "This function is called after a dumped lisp image has been restarted.
It does whatever is appropriate to keep Lisp running (some Lisps
loop or exit when their restart function completes).
For OpenMCL, this means sleeping for a very, very, very long time."
  ;; OpenMCL loops its restart function
  (format *trace-output* "~&;; Initial thread sleeping practically forever~%")
  (finish-output *trace-output*)
  (sleep 999999))

) ;; End warn-if-redfine block
