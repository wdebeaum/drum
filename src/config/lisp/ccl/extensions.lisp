;;;;
;;;; File: config/lisp/ccl/extensions.lisp
;;;; Creator: George Ferguson
;;;; Created: Tue Jan 19 15:49:44 2010
;;;; Time-stamp: <Tue Jan 19 15:53:19 EST 2010 ferguson>
;;;;
;;;; Based on openmcl/defs.mk, which in turn derived from mcl/defs.mk
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
  (ccl:process-suspend process))

(defun process-resume (process)
  "Resumes PROCESS."
  (ccl:process-resume process))

(defun process-alive-p (process)
  "Is the process alive (i.e. it hasn't ended and hasn't been killed)?"
  (ccl::process-active-p process))

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
For ccl, this means sleeping for a very, very, very long time."
  ;; CCL loops its restart function (is that still true? -gf)
  (format *trace-output* "~&;; Initial thread sleeping practically forever~%")
  (finish-output *trace-output*)
  (sleep 999999))

) ;; End warn-if-redfine block
