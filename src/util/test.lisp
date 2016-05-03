(unless (boundp 'user::*TRIPS_BASE*)
  (load (make-pathname :directory '(:relative "config") :name "version")))

(unless (find-package "UTIL")
  (load "TRIPS:util;util"))
		      
(defpackage "DM"
  (:use "COMMON-LISP" "UTIL"))

(in-package "DM")

(defun foo ()
  (find-arg-in-act '(ask :sender s :receiver r :content c) :receiver))
