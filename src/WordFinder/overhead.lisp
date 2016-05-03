(in-package "WORDFINDER")

;; globals
(defvar *debug* nil)

(defun print-debug (msg &rest rest)
  (if *debug*
      (apply #'format (cons 't (cons msg rest)))))

(defun wordfinder-warn (format-string &rest args)
  "Prints a warning message from the wordfinder routines."
  (format *error-output* "~&wordfinder: warning: ")
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :warn (apply #'format nil format-string args)))