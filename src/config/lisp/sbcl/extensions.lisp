;;;;
;;;; extensions.lisp for config/lisp/sbcl
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 30 Jan 2007
;;;; Time-stamp: <Sun Nov 16 12:24:20 CST 2014 lgalescu>
;;;;

(require :sb-bsd-sockets)

(in-package :trips)

;;;
;;; Add nickname USER for package "COMMON-LISP-USER"
;; (Don't wipe out :cl-user specified by standard.)
;;;
(defpackage :common-lisp-user (:nicknames :user :cl-user))

;;;
;;; Define functions not part of CL
;;;
(defun exit (n &key (quiet t))
  #+#.(cl:if (cl:find-symbol "EXIT" :sb-ext) '(and) '(or))
  ;; Assuming process exit is what is desired -- if thread termination
  ;; is intended, use SB-THREAD:ABORT-THREAD instead.
  (sb-ext:exit :code n :abort quiet)
  #-#.(cl:if (cl:find-symbol "EXIT" :sb-ext) '(and) '(or))
  (sb-ext:quit :unix-status n :recklessly-p quiet))

(defun get-env (var)
  (sb-ext:posix-getenv (string var)))

(defun gc (arg)
  (declare (ignore arg))
  (sb-ext:gc))

(defun make-socket (host port)
  (let* ((addr (sb-bsd-sockets:host-ent-address
		(sb-bsd-sockets:get-host-by-name host)))
	 (s (make-instance 'sb-bsd-sockets:inet-socket
			   :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect s addr port)
    (sb-bsd-sockets:socket-make-stream s :input t :output t)))

(defun process-run-function (name func &rest args)
  "Creates a new process named NAME and run FUNC on ARGS in it. Returns
the new process."
  (sb-thread:make-thread #'(lambda ()
			     (apply func args))
		   :name (string name)))

(defun current-process ()
  "Returns the currently-running process."
  sb-thread:*current-thread*)

(defun process-suspend (process)
  "Suspends PROCESS."
  (declare (ignore process))
  (error "~S not implemented in src/config/lisp/sbcl" :process-suspend))

(defun process-resume (process)
  "Resumes PROCESS."
  (declare (ignore process))
  (error "~S not implemented in src/config/lisp/sbcl" :process-resume))

(defun process-kill (process)
  (sb-thread:terminate-thread process))

(defun process-find (name)
  "Returns the process with the given NAME."
  (find name (sb-thread:list-all-threads)
	:key #'(lambda (p) (slot-value p 'sb-thread::name))))

(defun process-alive-p (process)
  "Is the process alive (i.e. it hasn't ended and hasn't been killed)?"
  (sb-thread:thread-alive-p process))

(defun avoid-exiting ()
  "This function is called after a dumped lisp image has been restarted.
It does whatever is appropriate to keep Lisp running (some Lisps
loop or exit when their restart function completes).
For CMUCL, this means suspending the current (initial) process."
  (sleep 999999)
;  (error "~S not implemented in src/config/lisp/sbcl" :avoid-exiting)
  )

;;;
;;; The sbcl compiler is just way too verbose.
;;;
;;; See: http://www.sbcl.org/manual/Controlling-Verbosity.html
;;;
;;; But note that muffling sb-ext:compiler-note doesn't muffle some things
;;; labelled ``note'' (such as unreachable code deletion), despite the
;;; description at http://www.sbcl.org/manual/Diagnostic-Severity.html
;;;
(declaim (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
