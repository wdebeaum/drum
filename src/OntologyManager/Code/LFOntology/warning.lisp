;;;;
;;;; warning.lisp
;;;;
;;;; Time-stamp: <Mon Jan 19 14:01:36 EST 2004 ferguson>
;;;;

(in-package "ONTOLOGYMANAGER")

(defun lfontology-warn (format-string &rest args)
  "Prints a warning message from the ontology routines."
  (format *error-output* "~&lfontology: warning: ")
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :warn (apply #'format nil format-string args)))
