;;;;
;;;; extensions.lisp for config/lisp/cmucl
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 30 May 2003
;;;; $Id: extensions.lisp,v 1.8 2008/11/26 17:28:05 wdebeaum Exp $
;;;;

(in-package :trips)

;; Make compiler more aggressive
;; The CMUCL manual says:
;;   "speed 3 enables some optimizations that hurt debuggability"
(declaim (optimize (speed 3)))

;; Tune garbage collector:
;; - Default for -dynamic-space-size is 512 MB (max 1.68GB on linux with
;;   generational GC)
;; - Default for gc threshold is 2000000 (~2MB)
;; - Prior to 2004-20-03, we set this to 64000000 (~64MB)
(setq ext:*bytes-consed-between-gcs* (* 64 1024 1024))

;;;
;;; Add nickname USER for package "COMMON-LISP-USER"
;; (Don't wipe out :cl-user specified by standard.)
;;;
(defpackage :common-lisp-user (:nicknames :user :cl-user))

;;;
;;; Define functions not part of CL
;;;
(defun exit (n &key (quiet t))
  (declare (ignore n))
  ;; cmucl quit takes `recklessly-p', which we'll say is the same as `quiet'
  (ext:quit quiet))

(defun get-env (var)
  (cdr (assoc (intern (string var) (find-package :keyword))
              ext:*environment-list*)))

(defun gc (arg)
  (declare (ignore arg))
  (extensions:gc))

(defun make-socket (host port)
  (system:make-fd-stream 
   (extensions:connect-to-inet-socket host port)
   :input t :output t :auto-close t)
   )

(defun process-run-function (name func &rest args)
  "Creates a new process named NAME and run FUNC on ARGS in it. Returns
the new process."
  ;; In CMUCL, it appears that MAKE-PROCESS also runs it
  ;; (According to code/multi-proc.lisp, we could specify ``:run-reasons nil''
  ;; to get the Allegro behavior of make-process.)
  (mp:make-process #'(lambda ()
		       (apply func args))
		   :name (string name)))

(defun current-process ()
  "Returns the currently-running process."
  (mp:current-process))

(defun process-suspend (process)
  "Suspends PROCESS."
  (mp:process-add-arrest-reason process :suspended))

(defun process-resume (process)
  "Resumes PROCESS."
  (mp:process-revoke-arrest-reason process :suspended))

(defun process-alive-p (process)
  "Is the process alive (i.e. it hasn't ended and hasn't been killed)?"
  (member process (mp:all-processes)))

(defun process-kill (process)
  "Kill the process."
  (mp:destroy-process process))

(defun avoid-exiting ()
  "This function is called after a dumped lisp image has been restarted.
It does whatever is appropriate to keep Lisp running (some Lisps
loop or exit when their restart function completes).
For CMUCL, this means suspending the current (initial) process."
  ;; NC: workaround for cmucl 99% cpu usage on idle processes
  ;; Threads in cmucl dumps hog the entire CPU, as if they are
  ;; constantly polling for messages.  This command seems to
  ;; start a new 'top-level' thread that takes charge when the lisp
  ;; threads are idle, reducing CPU load to ~0%
  (mp::startup-idle-and-top-level-loops)
  (format *trace-output* "~&;; Suspending initial thread~%")
  (process-suspend (current-process)))

#||
(trace
 mp:make-process
 mp:process-add-arrest-reason
 mp:process-revoke-arrest-reason)
||#
