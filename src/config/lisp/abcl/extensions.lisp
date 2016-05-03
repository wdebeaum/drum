;;;;
;;;; extensions.lisp for config/lisp/abcl
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 3 Feb 2007
;;;; Time-stamp: <Fri Feb  2 15:45:56 EST 2007 ferguson>
;;;;

(require :socket)

(in-package :trips)

;;;
;;; Add nickname USER for package "COMMON-LISP-USER"
;;; (Don't wipe out :cl-user specified by standard.)
;;; RENAME-PACKAGE in hyperspec:
;;;  http://www.lispworks.com/documentation/HyperSpec/Body/f_rn_pkg.htm
;;;
(rename-package (find-package :common-lisp-user)
		:common-lisp-user '(:user :cl-user))

;;;
;;; Define functions not part of CL
;;;
(defun exit (n &key (quiet t))
  (declare (ignore n quiet))
  (ext:quit))

(defun get-env (var)
  (format *error-output* "~&GET-ENV not supported in abcl~%")
  nil)

(defun gc (arg)
  (declare (ignore arg))
  (ext:gc))

(defun make-socket (host port)
  (let* ((s (ext:make-socket host port)))
    (ext:get-socket-stream s)))

(defun process-run-function (name func &rest args)
  "Creates a new process named NAME and run FUNC on ARGS in it. Returns
the new process."
  (threads:make-thread #'(lambda ()
		       (apply func args))
		   :name name))

(defun current-process ()
  "Returns the currently-running process."
  (threads:current-thread))

(defun process-suspend (process)
  "Suspends PROCESS."
  (declare (ignore process))
  (error "~S not implemented in src/config/lisp/abcl" :process-suspend))

(defun process-resume (process)
  "Resumes PROCESS."
  (declare (ignore process))
  (error "~S not implemented in src/config/lisp/abcl" :process-resume))

(defun avoid-exiting ()
  "This function is called after a dumped lisp image has been restarted.
It does whatever is appropriate to keep Lisp running (some Lisps
loop or exit when their restart function completes).
For CMUCL, this means suspending the current (initial) process."
  (error "~S not implemented in src/config/lisp/abcl" :avoid-exiting))

