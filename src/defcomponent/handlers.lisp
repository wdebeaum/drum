;;;;
;;;; handlers.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 30 Mar 2001
;;;; Time-stamp: <Fri Jan 15 14:17:33 EST 2010 ferguson>
;;;;

(in-package :defcomponent)

(defgeneric defcomponent-handler (pattern func &key subscribe))

(defmethod defcomponent-handler (pattern func &key subscribe)
  "Adds a handler FUNC for message-pattern PATTERN to the value of *COMPONENT*.
These are used by COMPONENT-DISPATCH-MESSAGE.

If keyword argument SUBSCRIBE is T, then the pattern is also saved for later
subscription (in REGISTER-COMPONENT)."
  (with-slots (handlers subscriptions) *component*
    (push (cons pattern func) handlers)
    (when subscribe
      (push pattern subscriptions))))

(defgeneric component-dispatch-message (m msg))

(defmethod component-dispatch-message ((m trips-agent) msg)
  "Dispatches message MSG for processing by the given component.

If MSG has an in-reply-to tag and there is a pending callback for that tag
registered with the component, then that callback is invoked.

Otherwise MSG is matched against the patterns added by
COMPONENT-ADD-MESSAGE-HANDLER and dispatched to the first one that matches.
The handler function is called with first argument MSG, and any additional
arguments corresponding to the elements of MSG matched by wildcards in the
pattern.

If no handler matches, an ERROR reply is generated (unless the message was
itself an ERROR or SORRY."
  (with-slots (replyq) m
    (let* ((tag (get-keyword-arg msg :in-reply-to))
	   (entry (assoc tag replyq))
	   (cont (second entry))
	   (content-only (third entry)))
      (when cont
	;; Remove pending callback
	(setq replyq (delete tag replyq :key #'first))
	;; Hand just the content to the callback (and we're done dispatching)
	(return-from component-dispatch-message
	  (funcall cont (if content-only (get-keyword-arg msg :content) msg))))))
  ;; Otherwise look to match messages handlers
  (with-slots (handlers) m
    (loop with (pattern func result)
	for entry in handlers
	do (setq pattern (car entry))
	   (setq func (cdr entry))
	   (setq result (multiple-value-list
			 (match-msg-pattern pattern msg)))
	   (when (car result)
	     (return-from component-dispatch-message
	       (apply func msg (cdr result)))))
    ;; If we get here, nothing matched
    ;; 4/28/2006: gf: More package crapola
    (let* ((verb (car msg))
	   (vstr (string-downcase (symbol-name verb))))
      ;;(when (not (or (eq verb 'error) (eq verb 'sorry)))
      (when (not (or (string-equal vstr "error") (string-equal vstr "sorry")))
	;; Send a SORRY reply to the original sender
	(component-reply-to-msg m msg (desymbolize 'sorry)
		:comment (format nil "no handler for message: ~S" msg))))))
;	(error (format nil "no handler for message:~%  ~S" msg))))))

(defun match-msg-pattern (pat msg)
  "Returns T if MSG matches pattern PAT, otherwise NIL.
Matching is defined as follows:
  - The element * in PAT is a wildcard that matches anything,
    otherwise non-lists must be EQUAL and lists must match recursively.
  - The element &key in PAT indicates that the remaining elements (at
    this level of embedding) are keyword/value patterns. These are
    considered to match if for each key/value pattern in PAT, the
    key appears in MSG (at this level of embedding) and the corresponding
    value in MSG matches the corresponding value in PAT.

Returns multiple values. The first value is either T or NIL, indicating
success or failure of the match. If this value is T, then additional values
represent the elements of MSG that matched wildcards (*'s) in PAT."
  (cond
   ((eq pat '*) 
    (values t msg))
   ((equal pat msg)
    (values t))
   ((and (consp pat) (consp msg))
    (cond ((eq (car pat) '&key)
	   (match-msg-keywords (cdr pat) msg))
	  (t
	   (match-msg-list pat msg))))
   (t
    (values nil))))

(defun match-msg-keywords (pat msg)
  (let ((stars nil))
    (loop
	with args = pat
	while args
	do (let* ((key (pop args))
		  (pval (pop args))
		  (mval (get-keyword-arg msg key))
		  (result (multiple-value-list
			   (match-msg-pattern pval mval))))
	     (if (not (car result))
		 (return-from match-msg-keywords nil)
	       (setq stars (append stars (cdr result))))))
    (values-list (cons t stars))))

(defun match-msg-list (pat msg)
  (let ((result1 (multiple-value-list
		  (match-msg-pattern (car pat) (car msg)))))
    (cond
     ((not (car result1))
      (values nil))
     (t
      (let ((results (multiple-value-list 
		      (match-msg-pattern (cdr pat) (cdr msg)))))
	(values-list (cons (car results)
			   (append (cdr result1) (cdr results)))))))))

;;;
;;; Standard handlers
;;;

;; NOTE: For these message patterns to be properly matched, the symbols used
;; in them must be exported from the DEFCOMPONENT package and imported into
;; the component's package (which happens automagically with the DEFCOMPONENT
;; macro). The exports are listed in defsys.lisp (arguably should be just
;; EXPORTed here).
(setq *standard-handlers*
    '(((tell &key :content (start-conversation . *)) . ignore-message)
      ((tell &key :content (end-conversation . *)) . ignore-message)
      ((request &key :content (restart . *)) . ignore-message)
      ((request &key :content (exit *)) . handle-request-exit)
      ((request &key :content (chdir *)) . handle-request-chdir)
      ((request &key :content (hide-window)) . ignore-message)
      ((request &key :content (show-window)) . ignore-message)
      ))

(defun ignore-message (&rest args)
  "This function is used to ignore a message without generating an error.
It accepts any arguments, doesn't do anything, and returns T."
  (declare (ignore args))
  t)

(defun handle-request-exit (msg &optional (exit-status 0))
  "This function is the default handler for an EXIT request. It throws
to the :EXIT tag established by RUN-COMPONENT."
  (declare (ignore msg))
  (throw :exit exit-status))

(defun handle-request-chdir (msg dirname)
  "This function is the default handler for a CHDIR request. It simply
calls LOGGING:CHDIR with the DIRNAME."
  (declare (ignore msg))
  (logging2:chdir dirname))

