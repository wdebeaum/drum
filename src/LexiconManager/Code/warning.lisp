(in-package "LEXICONMANAGER")

;; copied from parser-warn

(defun lexiconmanager-warn (format-string &rest args)
  ;; gf: 7/6/98: Added fresh-line in a desparate attempt to get nice messages,
  ;;             since use of PARSER-WARN is completely inconsistent.
  ;; gf: 3/17/2000: Be consistent with other TRIPS modules
  (fresh-line *error-output*)
  (princ "lxm: warning: " *error-output*)
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :warn (apply #'format nil format-string args))
  (values))

;; globals
(defvar *debug* nil)

(defun print-debug (msg &rest rest)
  (if *debug*
      (apply #'format (cons 't (cons msg rest)))))
