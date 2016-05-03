;;;;
;;;; warning.lisp
;;;;
;;;; Time-stamp: <Fri Jan 23 10:06:43 EST 2015 jallen>
;;;;

(in-package "IM")

(defun parser-warn (format-string &rest args)
  ;; gf: 7/6/98: Added fresh-line in a desparate attempt to get nice messages,
  ;;             since use of PARSER-WARN is completely inconsistent.
  ;; gf: 3/17/2000: Be consistent with other TRIPS modules
  (fresh-line *error-output*)
  (princ "parser: warning: " *error-output*)
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :warn (apply #'format nil format-string args))
  (values))

(defun parser-error (format-string &rest args)
  (fresh-line *error-output*)
  (princ "parser: ERROR: " *error-output*)
  (apply #'format *error-output* format-string args)
  (fresh-line *error-output*)
  (finish-output *error-output*)
  (LOGGING:log-message :error (apply #'format nil format-string args))
  (values))


