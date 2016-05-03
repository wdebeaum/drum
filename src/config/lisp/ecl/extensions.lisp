;;;;
;;;; extensions.lisp for config/lisp/ecl
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 13 Jun 2006
;;;; $Id: extensions.lisp,v 1.1 2006/11/01 19:02:04 ferguson Exp $
;;;;

;; As of 16 Jun 2006, needs --with-tcp during ecl configure
(require 'sockets)

(in-package :trips)

;;;
;;; Define functions not part of CL
;;;

(defun exit (n &key quiet)
  (declare (ignore quiet))
  (quit n))

(defun get-env (var)
  (si:getenv var))

(defun gc (arg)
  (declare (ignore arg))
  (si:gc t))

(defun make-socket (host port)
  "Returns a bidirectional stream connected to the given host:port."
  (let ((addr (sb-bsd-sockets:host-ent-address
	       (sb-bsd-sockets:get-host-by-name host)))
	(sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect sock addr port)
    (sb-bsd-sockets:socket-make-stream sock)))

(defun process-run-function (name func &rest args)
  "Creates a new process named NAME and run FUNC on ARGS in it. Returns
the new process."
  (apply #'mp:process-run-function name func args))

(defun current-process ()
  "Returns the currently-running process."
  mp:*current-process*)

(defun process-suspend (process)
  "Suspends PROCESS."
  (error "not implemented"))

(defun process-resume (process)
  "Resumes PROCESS."
  (error "not implemented"))

(defun avoid-exiting ()
  "This function is called after a dumped lisp image has been restarted.
It does whatever is appropriate to keep Lisp running (some Lisps
loop or exit when their restart function completes).
For ECL, this has not been investigated yet."
  t)
