;;;;
;;;; extensions.lisp for config/lisp/allegro
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 13 Sep 2002
;;;; $Id: extensions.lisp,v 1.7 2008/11/26 17:28:05 wdebeaum Exp $
;;;;

;; Not sure where this came from - gf 3/17/2007
(require :acldns)

(in-package :trips)

(defun exit (n &key quiet)
  (excl:exit n :quiet quiet))

(defun get-env (var)
  (sys:getenv var))

(defun gc (arg)
  (excl:gc arg))

(defun make-socket (host port)
  (ACL-SOCKET:make-socket :remote-host host :remote-port port))

(eval-when (:compile-top-level :load-top-level :execute)
  (require ':process))

(defun process-run-function (name func &rest args)
  "Creates a new process named NAME and run FUNC on ARGS in it. Returns
the new process."
  (apply #'mp:process-run-function (string name) func args))

(defun current-process ()
  "Returns the currently-running process."
  sys:*current-process*)

(defun process-suspend (process)
  "Suspends PROCESS."
  (mp:process-add-arrest-reason process :suspended))

(defun process-resume (process)
  "Resumes PROCESS."
  (mp:process-revoke-arrest-reason process :suspended))

(defun process-kill (process)
  "Kill the process."
  (mp:process-kill process))

(defun process-alive-p (process)
  "Is the process alive (i.e. it hasn't ended and hasn't been killed)?"
  (mp:process-alive-p process))

(defun avoid-exiting ()
  "This function is called after a dumped lisp image has been restarted.
It does whatever is appropriate to keep Lisp running (some Lisps
loop or exit when their restart function completes).
For Allegro, nothing has to be done."
  t)

;; In v7.0, allegro introduced a PROLOG package, which gets autoloaded
;; Per bug report spr29907, this will fix it using internal APIs,
;; which of course may break in the future...
(setq excl::*autoload-package-name-alist*
      (delete :prolog excl::*autoload-package-name-alist* :key #'cdr))
