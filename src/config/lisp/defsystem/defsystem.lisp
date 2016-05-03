;;;;
;;;; defsystem.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 13 Sep 2002
;;;; $Id: defsystem.lisp,v 1.5 2010/01/18 19:06:29 ferguson Exp $
;;;;

(eval-when (:load-toplevel :execute)
  (let* ((this-directory (pathname-directory *load-pathname*)))
    (load (make-pathname :directory (append this-directory
					    (list "defsystem-3.6i"))
			 :name "defsystem"))))

(in-package :mk)

;; Override default filename mappings
;; Allegro in particular defaults to ".cl"
(setq *filename-extensions*
      (cons "lisp"
	    (pathname-type (compile-file-pathname "foo.lisp"))))

;; And have to redo this snip of code that gets run when the above was loaded
;; Also modify COMPILE-FILE and LOAD to flush printing
(define-language :lisp
  :compiler #'(lambda (&rest args)
		(apply #'compile-file args)
		(fresh-line t)
		(finish-output t)
		(fresh-line *error-output*)
		(finish-output *error-output*)
		(format *trace-output* "~&;Compiled ~S~%" (first args))
		(finish-output *trace-output*))
  :loader #'(lambda (&rest args)
	      (apply #'load args)
		(fresh-line t)
		(finish-output t)
		(fresh-line *error-output*)
		(finish-output *error-output*)
		(format *trace-output* "~&;Loaded ~S~%" (first args))
		(finish-output *trace-output*))
  :source-extension (car mk::*filename-extensions*)
  :binary-extension (cdr mk::*filename-extensions*))

;; Very handy to set this... (default is :query)
(setq *compile-during-load* t)

;;;
;;; CLOS wrapper for OPERATE-ON-SYSTEM allows extension in defsys
;;;
(let ((original-oos (symbol-function 'mk:operate-on-system)))
  (fmakunbound 'mk:operate-on-system)
  (defgeneric mk:operate-on-system (name operation &rest args))
  (defmethod mk:operate-on-system (name operation &rest args)
    "CLOS wrapper for MK:OPERATE-ON-SYSTEM."
    (apply original-oos name operation args)))
