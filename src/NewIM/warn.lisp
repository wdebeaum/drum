;;;;
;;;; warn.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 24 Sep 2003
;;;; Time-stamp: <Fri Jan 23 09:41:04 EST 2015 jallen>
;;;;

(in-package :im)

(defvar *step* nil)
(defvar *trace-level* nil)
(defvar *rule-to-trace* nil) ;; set this to the ID you want more information about

(defun set-debug-level (&key level)
  (case level
    (off (trace-off))
    (debug (trace-on 1))
    (otherwise (trace-on 2))))

(defun trace-on (n)
  (if (numberp n)
      (setq *trace-level* n)
    (im-warn "Step level must be a number")))

(defun trace-off nil
  (setq *trace-level* nil)
  (setq *step* nil))

(defun trace-rule (id)
  (setq *rule-to-trace* id)
  (trace-on 4))

(defun untrace-rule nil
  (setq *rule-to-trace* nil))

(defun im-step-off nil
  (setq *step* nil))

(defun im-step-on (n)
  (setq *step* t)
  (trace-on n))

(defun im-warn (&rest args)
  (let ((msg (apply #'format nil args)))
    (format *trace-output* "~&im: ~A~%" msg)
    (logging:log-message :trace msg)
    nil))

(defun trace-msg (n &rest args)
  (when *trace-level*
    (when (>= *trace-level* n)
      (let ((msg (apply #'format (cons nil args))))
	(format t "~%IM: ~A" (concatenate 'string (case n (1 "") (2 "   ") (3 "     ") (4 "       "))
			       msg))
	
	(when *step* 
	  (format t "~%IM:     at level ~S, change?:" *trace-level*)
	  (let ((x (read-line)))
	    (if (not (string= x ""))
		(let ((new (read-from-string x)))
		  (if (numberp new) (setq *trace-level* new)
		    (eval x))))))))
    (values)))

(defun trace-pprint (n msg x)
  (when (and *trace-level* (>= *trace-level* n))
    (format t msg)
    (pprint x))
  (values))


(defun print-mnode (mn)
  (list 'MNODE :id (mnode-rule-id mn) :PATTERN (mnode-pattern mn) :BNDGS (mnode-bndgs mn) ))

(defun print-frontier (f)
  (cond ((mnode-p f)
	 (format t "~% ~S :pattern ~S" (mnode-rule-id f) (subst-in (mnode-pattern f) (mnode-bndgs f))))
	((consp f)
	 (mapcar #'print-frontier f))
	))

(defun debug-rules nil
  (mapcar #'print-frontier *remaining-frontier*))

(defun log-msg (content index)
  (output `(TELL :SENDER IM ;;:RECEIVER SYSTEM-LOG
		 :CONTENT ,(insert-BA-vars content) :INDEX ,index))
  (values))

