;;;;
;;;; trips-agent.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 30 Mar 2001
;;;; Time-stamp: <Fri Jan 15 14:17:59 EST 2010 ferguson>
;;;;

(in-package :defcomponent)

(defvar *standard-handlers* nil
  ;; This list is initialized later, but is used in the defclass below
  "Alist of standard message handlers for TRIPS components.")

(defclass trips-agent (trips-component)
  ((handlers :initform *standard-handlers*)
   (subscriptions :initform nil)
   (replyq :initform nil)
   (cancellation-patterns :initform nil))
  (:documentation "Base class for TRIPS agent components."))

;;;
;;; Methods for TRIPS-AGENTs
;;;

(defgeneric init-component (m))

(defmethod init-component ((m trips-agent))
  "Default INIT-COMPONENT method: Calls CONNECT-COMPONENT, REGISTER-COMPONENT,
and COMPONENT-IS-READY."
  (connect-component m)
  (register-component m)
  (component-is-ready m))

(defmethod init-component ((name t))
  "Looks up the component named NAME and calls INIT-COMPONENT on it.
Signals an error if the component is not found."
  (init-component (find-component-or-die name)))

(defgeneric connect-component (m &optional group))

(defmethod connect-component ((m trips-agent) &optional (group 'system))
  "Default CONNECT-COMPONENT method: Establishes connection to the TRIPS
message-passing system."
  (declare (ignore group))
  (with-slots (name) m
    (io:connect name))	      
  t)

(defgeneric register-component (m &optional group))

(defmethod register-component ((m trips-agent) &optional (group 'system))
  "Default REGISTER-COMPONENT method: Sends the REGISTER message to the
Facilitator. Also sends any SUBSCRIBE messages specified by calls to
DEFCOMPONENT-HANDLER."
  (with-slots (name subscriptions) m
    (component-send-msg m (desymbolize `(register :name ,(intern name) :group ,group)))
    (loop for s in subscriptions
	do (component-send-msg m (desymbolize `(subscribe :content ,s)))))
  t)

(defgeneric component-is-ready (m))

(defmethod component-is-ready ((m trips-agent))
  "Default COMPONENT-IS-READY method: Sends the COMPONENT-STATUS message to the
Facilitator."
  (with-slots (name) m
    (component-send-msg m (desymbolize
			   `(tell :content (component-status :who ,(intern name)
							     :what ready)))))
  t)

(defun desymbolize (x)
  "Adjusts all non-keyword symbols in input X to be in current package.
NOTE: This function is different from IO:CONVERT-TO-PACKAGE, which only
      changes some symbols. This function unequivocally converts symbols
      to *PACKAGE*."
  (cond ((null x)
	 nil)
	((and (symbolp x) (not (keywordp x)))
	 (intern (symbol-name x) *package*))
	((listp x)
	 (reuse-cons (desymbolize (car x)) (desymbolize (cdr x)) x))
	(t x)))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defvar *pending-messages* nil
  "List of messages waiting to be handled (used in SEND-AND-WAIT).
This variable must be LET on a per-process basis to make sure that messages
don't get to other modules/processes.")

(defgeneric component-read-msg (m))

(defmethod component-read-msg ((m trips-agent))
  "Reads the next message for the given component, and logs the receipt."
  (with-slots (name) m
   (let ((msg (io:recv name)))
     (logging2:log-message-received msg)
     msg)))

(defgeneric component-next-msg (m))

(defmethod component-next-msg ((m trips-agent))
  "Returns the next message for the given component, either from the list
of pending messages or by reading the next message."
  (cond
   ;; If anything pending...
   (*pending-messages*
    ;; ...Then return the next one
    (let ((msg (pop *pending-messages*)))
      (logging2:log-message :note (list :processing-deferred-message msg))
      msg))
   (t
    ;; Otherwise go ahead and read a message
    (component-read-msg m))))

(defgeneric run-component (m))

(defmethod run-component ((m trips-agent))
  "Default RUN-COMPONENT method: First calls INIT-COMPONENT. Then reads
messages and dispatches them, with suitable logging, error handling,
and restarts."
  (with-slots (name package) m
    (let ((*package* package)
	  (*component* m)
	  (*pending-messages* nil)
	  (logging2::*log-filename* (format nil "~A.log" name))
	  (logging2::*log-stream* nil))
      (logging2:chdir ".")
      (init-component m)
      (catch :exit
	(with-simple-restart (abort (format nil "Terminate ~A main loop" name))
	  (loop
	    (catch :main-loop
	      (with-simple-restart (abort (format nil "Restart ~A main loop" name))
		(let ((msg (component-next-msg m)))
		  (if (null msg)
		      (throw :exit 0)	; EOF => exit
		    (handler-bind
			((error #'(lambda (err)
				    (indicate-error m err msg))))
		      (component-receive-msg m msg))))))))))))

(defmethod run-component ((name t))
  "Looks up the component named NAME and calls RUN-COMPONENT on it.
Signals an error if the component is not found."
  (run-component (find-component-or-die name)))

(defvar *break-on-error* nil
  "If true, signal a continuable error whenever an error condition is
thrown during processing. This is good for debugging. But for production
code, this should be NIL, which causes an ERROR reply to be generated
instead.")

(defgeneric indicate-error (m err msg))

(defmethod indicate-error ((m trips-agent) err msg)
  "Indicates that Lisp error ERR has occurred in component COMPONENT.
If *BREAK-ON-ERROR* is bound and is non-NIL, this signals a
continuable error resulting in a toplevel break.
Otherwise simply prints the error to *ERROR-OUTPUT* and generates an ERROR
message in reply to MSG."
  (with-slots (name) m
    (unless (and (boundp *break-on-error*) *break-on-error*)
      (let ((errstr (princ-to-string err)))
	(format *error-output* "~A error: ~A~%" name errstr)
	(logging2:log-message :error errstr)
	(component-reply-to-msg m msg (desymbolize 'sorry) :comment errstr))
      ;; Throw to avoid signalling the error
      (throw :main-loop nil))))

(define-condition trips-message-error (error)
  ((comment :initform nil)))

(defun signal-message-error (fmt &rest args)
  "Signals a MESSAGE-ERROR condition with comment produced from applying
FORMAT to FMT and optional ARGS."
  (let ((c (make-condition 'trips-message-error))
	(text (apply #'format nil fmt args)))
    (setf (slot-value c 'comment) text)
    (error c)))

(defgeneric component-receive-msg (m msg))

(defmethod component-receive-msg ((m trips-agent) msg)
  "Establishes a handler for MESSAGE-ERRORs and calls COMPONENT-DISPATCH-MESSAGE
to start processing the message MSG. If a MESSAGE-ERROR is signalled during
execution, it will result in an ERROR reply to MSG being generated."
  (handler-case (component-dispatch-message m msg)
    (trips-message-error (c)
      (with-slots (comment) c
	(component-reply-to-msg m msg 'error :comment comment)))))
	
(defgeneric component-send-msg (m msg))

(defmethod component-send-msg ((m trips-agent) msg)
  "Send (and log) message MSG. Returns the message that was sent."
  (with-slots (name) m
    (io:send name msg)
    (logging2:log-message-sent msg)
    msg))

(defun send-msg (msg)
  "Use the current *COMPONENT* and call COMPONENT-SEND-MSG."
  (component-send-msg *component* msg))

(defgeneric component-reply-to-msg (m msg verb &rest parameters))

(defmethod component-reply-to-msg ((m trips-agent) msg verb &rest parameters)
  "Generate a reply to MSG of type VERB with given PARAMETERS.
The receiver of the message will be the sender of the MSG, and
any :reply-with will be used as :in-reply-to in the reply."
  (component-send-msg m (make-reply-msg msg verb parameters)))

(defun reply-to-msg (msg verb &rest parameters)
  "Use the current *COMPONENT* and call COMPONENT-REPLY-TO-MSG."
  (apply #'component-reply-to-msg *component* msg verb parameters))

(defun make-reply-msg (msg verb parameters)
  "Return a message consisting of VERB and PARAMETERS, where additionally the
:receiver is the :sender of MSG, and if MSG contains a :reply-with tag, then
the returned message will use it as the :in-reply-to parameter."
  (let ((sender (get-keyword-arg msg :sender))
	(use-reply-with (find :reply-with msg))
	(reply-with (get-keyword-arg msg :reply-with)))
    (append (list verb :receiver sender)
	    (when use-reply-with
	      (list :in-reply-to reply-with))
	    parameters)))

(defun get-keyword-arg (l key)
  "Returns element following KEY in L, or NIL."
  (cond ((and (consp l) (consp (cdr l)))
	 (if (eq key (car l))
	     (cadr l)
	   (get-keyword-arg (cdr l) key)))
	(t
	 nil)))

(defun send-and-wait (msg)
  "Sends MSG. Then reads messages until the reply is received, queueing
anything that arrives in between for later handling. Returns the :content
of the reply as the value of this function."
  (let ((tag (get-keyword-arg msg :reply-with)))
    ;; Make sure we have a reply-with tag
    (when (not tag)
      (setq tag (gentemp "IO-"))
      (setq msg (append msg (list :reply-with tag))))
    ;; Send our message
    (component-send-msg *component* msg)
    ;; Process messages
    (logging2:log-message :note (list :waiting-for-reply))
    (loop
     ;; Read a message
     do (setq msg (component-read-msg *component*))
     ;; Handle EOF
     (when (null msg)
       (throw :exit 0))
     ;; Handle bogus msg
     (when (not (listp msg))
       (error 'message-error :comment "message is not a list"))
     ;; Otherwise see if we want it
     (cond
      ((eq (get-keyword-arg msg :in-reply-to) tag)
       ;; Yes, it's our reply: take the content for the answer
       (logging2:log-message :note (list :received-reply msg))
       (return-from send-and-wait (get-keyword-arg msg :content)))
      (t
       ;; No, save for later
       (logging2:log-message :note (list :deferred-message msg))
       (setq *pending-messages* (append *pending-messages* (list msg))))))))

;;;
;;; Messages with callbacks on reply
;;;

(defgeneric component-send-msg-with-continuation (m msg cont &key content-only))

(defmethod component-send-msg-with-continuation ((m trips-agent) msg cont
						 &key (content-only t))
  "Sends MSG and calls back CONT when a reply arrives."
  (with-slots (name replyq) m
    (let ((tag (gentemp name)))
      (push (list tag cont content-only) replyq)
      (component-send-msg m (append msg (list :reply-with tag))))))

(defmethod component-send-msg-with-continuation ((name t) msg cont
						 &key (content-only t))
  "Looks up the component named NAME and calls
COMPONENT-SEND-MSG-WITH-CONTINATION on it.
Signals an error if the component is not found."
  (component-send-msg-with-continuation (find-component-or-die name) msg cont :content-only content-only))

(defun send-msg-with-continuation (msg cont &key (content-only t))
  "Use the current *COMPONENT* and call COMPONENT-SEND-MSG-WITH-CONTINUATION."
  (component-send-msg-with-continuation *component* msg cont :content-only content-only))

;;;
;;; Combining REPLY-TO-MSG and SEND-MSG-WITH-CONTINUATION
;;;

(defgeneric component-reply-to-msg-with-continuation (m msg cont
							verb &rest parameters))

(defmethod component-reply-to-msg-with-continuation ((m trips-agent) msg cont
						     verb &rest parameters)
  "Replies to MSG with (VERB . PARAMETERS)and calls back CONT when a reply
arrives. The continuation gets only the content of the reply." 
  (component-send-msg-with-continuation m (make-reply-msg msg verb parameters) cont))

(defmethod component-reply-to-msg-with-continuation ((name t) msg cont
						     verb &rest parameters)
  "Looks up the component named NAME and calls
COMPONENT-REPLY-TO-MSG-WITH-CONTINUATION on it.
Signals an error if the component is not found."
  (apply #'component-send-msg-with-continuation
	 (find-component-or-die name) msg cont verb parameters))

(defun reply-to-msg-with-continuation (msg cont verb &rest parameters)
  "Use the current *COMPONENT* and call COMPONENT-REPLY-TO-MSG-WITH-CONTINUATION."
  (apply #'component-send-msg-with-continuation
	 *component* msg cont verb parameters))

(defun check-for-cancellation-msg ()
  "Reads messages until there are no more ready, and throws to :main-loop when start-conversation is received"
  (with-slots (name cancellation-patterns) *component*
    (send-msg `(,(intern "TELL") :receiver ,(intern name) :content (,(intern "END-CHECK-FOR-CANCELLATION-MSG"))))
    (let (got-cancellation-msg)
      (loop for msg = (component-read-msg *component*)
	    while (and msg (not (eql (intern "END-CHECK-FOR-CANCELLATION-MSG") (car (get-keyword-arg msg :content)))))
	    do
	  ;(format t "msg = ~S (package is ~a, current package is ~a, package when compiled was ~a)~%" msg (symbol-package (car msg)) *package* (symbol-package 'foo))
	  ;(format t "(g-k-a msg :content) = ~S~%" (get-keyword-arg msg :content))
	  (append *pending-messages* (list msg))
	  (dolist (pattern cancellation-patterns)
	    (when (match-msg-pattern pattern msg)
	      (format t "got cancellation message!~%")
	      (setq *pending-messages* (list msg))
	      (setf got-cancellation-msg t)
	      )))
      (when got-cancellation-msg (throw :main-loop nil))
      )))

(defgeneric defcomponent-cancellation-pattern (pattern &key subscribe))

(defmethod defcomponent-cancellation-pattern (pattern &key subscribe)
  "Adds message-pattern PATTERN to the cancellation-patterns slot of the value of *COMPONENT*.
These are used by CHECK-FOR-CANCELLATION-MSG.

If keyword argument SUBSCRIBE is T, then the pattern is also saved for later
subscription (in REGISTER-COMPONENT)."
  (with-slots (cancellation-patterns subscriptions) *component*
    (push pattern cancellation-patterns)
    (when subscribe
      (push pattern subscriptions))))

(defun pending-messages-matching-pattern-p (pattern)
  "Are there any messages pending which match the given pattern?"
  (consp (member pattern *pending-messages* :test #'match-msg-pattern)))

