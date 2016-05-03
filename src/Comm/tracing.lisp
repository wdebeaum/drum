;;;;
;;;; tracing.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 17 Aug 1999
;;;; Time-stamp: <Tue Aug 17 17:15:02 EDT 1999 ferguson>
;;;;

(in-package "COMM")

(defvar *trace-funcs*
    '(
      block-process wakeup-process
      new-client
      add-listener-for-client
      add-message-for-client get-message-for-client
      im im-selective-broadcast
      send recv
      ))

(let ((tracing nil))
  (defun trace-comm (&optional (yes-or-no :toggle))
    "With argument T, enable tracing of *TRACE-FUNCS*, with arg NIL, disable
tracing, otherwise toggle tracing."
    (setq tracing (or (eq yes-or-no t)
		      (and (eq yes-or-no :toggle) (not tracing))))
    (format *trace-output* "Tracing ~A~%" (if tracing "enabled" "disabled"))
    (if tracing
	(eval (cons 'trace *trace-funcs*))
      (eval (cons 'untrace *trace-funcs*)))))
