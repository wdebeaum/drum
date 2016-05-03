;; ----------------------------------------
;; Printing
;; ----------------------------------------

(in-package :dc)

;; globals
(defvar *debug* nil)

(defun print-error (msg &rest rest)
  (format t "DC ERROR: ")
  (apply #'format (cons 't (cons msg rest))))

(defun print-warning (msg &rest rest)
  (format t "DC WARNING: ")
  (apply #'format (cons 't (cons msg rest))))

(defun print-debug (msg &rest rest)
  (if *debug*
      (apply #'format (cons 't (cons msg rest)))))
