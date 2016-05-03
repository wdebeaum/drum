;;;;
;;;; warning.lisp
;;;;
;;;; Time-stamp: <Mon Jan 19 14:00:32 EST 2004 ferguson>
;;;;

(in-package "ONTOLOGYMANAGER")

(defun krontology-warn (format-string &rest args)
  "Prints a warning message from the ontology routines."
  (format *error-output* "~&krontology: warning: ")
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :warn (apply #'format nil format-string args)))

(defun krontology-error (format-string &rest args)
  (fresh-line *error-output*)
  (format *error-output* "krontology: ERROR: ")
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :error (apply #'format nil format-string args))
  (values))


