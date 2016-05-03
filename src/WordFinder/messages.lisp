;;;;
;;;; messages : Message-passing interface to the word finder
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 19 Mar 2003
;;;; Time-stamp: <Fri May 13 11:10:28 EDT 2005 swift>
;;;;

(in-package :wordfinder)

(defvar *module-name* 'wordfinder
  "Name under which to register this module when we startup. This is also
used to find the package we should be in runtime when reading messages,
and the name of the logfile used to log messages to and from this module.")

(defun run ()
  "Main loop for the router."
  ;; Open communications for this module
  (COMM:connect *module-name*)
  ;; Make sure we're in the right package during i/o
  (let ((*package* (find-package :wordfinder))
	(LOGGING::*log-filename* (format nil "~A.log" *module-name*))
	(LOGGING::*log-stream* nil))
    (LOGGING:chdir ".")
    (output `(register :name ,*module-name* :group system))
    (output '(tell :content (module-status ready)))
    (catch :exit
      (with-simple-restart (abort (format nil "Terminate ~A main loop" *module-name*))
	(loop
	  (catch :main-loop
	    (with-simple-restart (abort (format nil "Restart ~A main loop" *module-name*))
	      ;; Clean some garbage while we have the chance
	      ;; (excl:gc t)
	      ;; Now read a message and deal with it, trapping any errors
	      (let ((msg (input)))
		(if (null msg)
		    (throw :exit 0)	; EOF => exit
		  (handler-bind
		      ((error #'(lambda (err)
				       (indicate-error (symbol-name *module-name*) err msg))))
		    (receive-message msg)))))))))))

(defun indicate-error (module err msg)
  "Indicates that Lisp error ERR has occurred in module MODULE.
If USER::*BREAK-ON-ERRORS* is bound and is non-NIL, this signals a
continuable error resulting in a toplevel break.
Otherwise simply prints the error to *ERROR-OUTPUT* and generates an ERROR
message in reply to MSG."
  (declare (special USER::*break-on-errors*))
  (unless (and (boundp 'USER::*break-on-errors*) USER::*break-on-errors*)
    (format *error-output* "~A error: ~A~%" module err)
    (reply msg 'sorry :comment (princ-to-string err))
    ;; Throw to avoid signalling the error
    (throw :main-loop nil)))

(defun input ()
  "Input, log, and return the next message for this module."
  (let ((msg (COMM:recv *module-name*)))
    (LOGGING:log-message-received msg)
    msg))

(defun receive-message (msg)
  "Dispatches message MSG for processing."
  (case (car msg)
    (request				; REQUEST
     (receive-request msg))
    (tell				; TELL
     (receive-tell msg))
    ((error sorry)			; ERROR or SORRY (ignored)
     t)
    (otherwise				; Unknown performative!
     (reply msg 'error
	    :comment (format nil "unknown verb in message: ~S" msg)))))

(defun receive-request (msg)
  "Handles REQUEST messages received by the router."
  (let ((content (get-keyword-arg msg :content)))
    (case (first content)
      (exit
       (throw :exit (or (second content) 0)))
      (eval
       (eval (second content)))
      ((hide-window show-window)	; ignore
       nil)
      (chdir
       (LOGGING:chdir (second content)))
      ;; Special handling code for unknown words here
      (unknown-word
       (let ((word (get-keyword-arg content :word)))
      (reply msg 'reply :content (lookup-unknown-word word))))
      ;; End special handling code
      (otherwise
       (reply msg 'error
	      :comment (format nil "bad request: ~A" (first content)))))))

(defun receive-tell (msg)
  "Handles TELL messages received by the router."
  (let ((content (get-keyword-arg msg :content)))
    (case (first content)
      ((start-conversation end-conversation) ; ignore
       nil)  
     (otherwise
       (reply msg 'error
	      :comment (format nil "bad tell: ~A" (car content)))))))

(defun reply (msg verb &rest parameters)
  "Generate a reply to MSG of type VERB with given PARAMETERS.
The receiver of the message will be the sender of the MSG, and
any :reply-with will be used as :in-reply-to in the reply."
  (let ((sender (get-keyword-arg msg :sender))
	(use-reply-with (find :reply-with msg))
	(reply-with (get-keyword-arg msg :reply-with)))
    (output (append (list verb :receiver sender)
		    (when use-reply-with
		      (list :in-reply-to reply-with))
		    parameters))))

(defun output (msg)
  "Output (and log) message MSG. Returns the message that was sent."
  (COMM:send *module-name* msg)
  (LOGGING:log-message-sent msg)
  msg)

(defun get-keyword-arg (l key)
  "Returns element following KEY in L, or NIL."
  (cond ((and (consp l) (consp (cdr l)))
	 (if (eql key (car l))
	     (cadr l)
	   (get-keyword-arg (cdr l) key)))
	(t
	 nil)))
