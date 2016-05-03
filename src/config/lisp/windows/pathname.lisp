;;;;
;;;; pathname.lisp for windows
;;;;
;;;; William de Beaumont, wdebeaum@cs.rochester.edu, 11 Sep 2008
;;;; $Id: pathname.lisp,v 1.1 2008/09/11 18:44:55 wdebeaum Exp $
;;;;

(in-package :trips)

;;;
;;; Pathnames in TRIPS are specified with a semicolon separator
;;; (like the syntax of logical pathnames, but we have given up on
;;; logical pathnames themselves as being too restrictive). These
;;; routines look after mapping that syntax onto the underlying
;;; operating system's conventions.
;;;
(let* ((separator #\\))
  (defun make-trips-pathname (path)
    "Create a pathname for referencing PATH in the TRIPS directory tree."
    (setq path (substitute separator #\; path))
    (parse-namestring (format nil "~A~A" *TRIPS_BASE* path))))

