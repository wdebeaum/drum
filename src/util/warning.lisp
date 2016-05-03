;;;;
;;;; warning.lisp
;;;;
;;;; Time-stamp: <Mon Mar 12 17:16:00 EST 2001 myros>
;;;;

(in-package "UTIL")

(defun util-warn (format-string &rest args)
  "Prints a warning message from the util routines."
  (princ "util: warning: " *error-output*)
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :warn (apply #'format nil format-string args)))



