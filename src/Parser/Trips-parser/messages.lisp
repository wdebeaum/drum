;;;;
;;;; messages.lisp: Message-passing interface to the parser
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 14 Apr 1998


(in-package "PARSER")

(defvar *module-name* 'parser
  "Name under which to register this module when we startup. This is also
used to find the package we should be in runtime when reading messages,
and the name of the logfile used to log messages to an from this module.")

(defvar *parser-init-settings* nil
  "the system may have put settings in here")

(defvar user::*use-texttagger* nil)

(declaim (special LOGGING::*log-filename* LOGGING::*log-stream*))

(defvar *parser-is-online* t
  "If T, messages are processed normally by the parser. If NIL, then most
messages are ignored, at least those that would cause parsing to happen (as
opposed to control messages). This variable is toggled by the ONLINE and
OFFLINE requests.")

(defvar *wait-for-speech-end* t)    ;; set t to stop parser running while speech recognition is running

(defvar *utterance-parsing-complete* nil
    "If T, the parser will not pull anything else off the agenda, although it will still
   respond to incoming messages" )

(defvar *end-of-utterance-processing-performed* T
  "If T, then the newupdatediscourse message has been sent on for the current sentence;
   starts with a value of T as an initial condition, set to nil when each sentence begins." )

(defun initialize-settings nil
  (when *parser-init-settings*
    (mapcar #'(lambda (x)
		(if (consp (car x)) (eval (car x))
		    (eval (list 'setq (car x) (cadr x)))))
	    *parser-init-settings*)
    (lg)  ;; reload the grammar so that the dependency info is recomputed
    (format t "~%Running with customized parser settings:~% ~S~%" *parser-init-settings*)))

(defun send-new-speech-act (acts)
  (let ((augmented-acts (insert-preparses acts)))
    (send-msg `(tell :content ,(if (= (number-parses-desired *chart*) 1)
				   (list 'new-speech-act (car augmented-acts))
				   (list 'new-speech-act-hyps augmented-acts)))))
    (when (eq *in-system* :plow)
    (send-msg '(request :content (trafficlight green)))) 
  )

(defun output (msg)
  #||"Output (and log) message MSG. Returns the message that was sent."
  (COMM:send *module-name* msg)
  (LOGGING:log-message-sent msg)
  msg)||#
  (send-msg msg))

(defun get-keyword-arg (l key)
  "Returns element following KEY in L, or NIL."
  (cond ((and (consp l) (consp (cdr l)))
	 (if (eq key (car l))
	     (cadr l)
	   (get-keyword-arg (cdr l) key)))
	(t
	 nil)))


(defcomponent-handler
  '(request &key :content (exit . *))
  #'(lambda (msg args)
       (throw :exit (or (second (get-keyword-arg msg :content)) 0)))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (eval . *))
  #'(lambda (msg args)
       (eval (second (get-keyword-arg msg :content))))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (chdir . *))
  #'(lambda (msg args)
        (LOGGING:chdir (second (get-keyword-arg msg :content))))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (restart . *))
  #'(lambda (msg args)
        (StartNewUtterance))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (offline . *))
  #'(lambda (msg args)
        (setq *parser-is-online* nil)
	(finish-output *error-output*))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (restart . *))
  #'(lambda (msg args)
        (setq *parser-is-online* t)
	(finish-output *error-output*))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (end-paragraph . *))
  #'(lambda (msg args)
       (output `(tell :content (paragraph-completed :id ,(get-keyword-arg args :id)))))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (word . *))
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (prefix . *))
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (started-speaking . *))
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (start-new-sentence . *))
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (stopped-speaking . *))
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (utterance . *))
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (prefer . *))
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (adjust-cost-table . *))
    #'(lambda (msg args)
	(let ((content (get-keyword-arg msg :content)))
	  (adjust-cost-table (get-keyword-arg content :mods) (get-keyword-arg content :duration))))
  :subscribe t)

(defun handle-message (msg)
  (let ((content (get-keyword-arg msg :content))
	(sender (get-keyword-arg msg :sender))
	)
    
    (when (and 
	   ;;  if texttagger is activated, we only listen for messages from it (and test for debugging) and ignore speec-in and keyboard messages,
	   ;;   otherwise we ignore messages from texttagger and allow the other sources!
	   (if  user::*use-texttagger*
	       (member  sender '(TEXTTAGGER TEST))
	       (not  (eq  sender 'TEXTTAGGER)))
	   *parser-is-online*)
    (case (first content)
      (end-paragraph
       (output `(tell :content (paragraph-completed :id ,(get-keyword-arg content :id)))))

      ((word phrase start-new-sentence start-sentence started-speaking stopped-speaking word-backto getanswers lattice prefer prefix) 
       (cache-message content))

      ;;   This initiates the parsing
      (utterance
       ;; <lgalescu 02/29/2016>: we catch errors here and signal that we failed to parse the utterance
       (handler-bind
	   ((error #'(lambda (err)
	               (let ((*package* (find-package :parser))) ; in case the error came from LXM or WF, we don't want to accidentally put parser:: on the beginning of these symbols as we write them
		         (output `(tell :content (failed-to-parse :uttnum ,(get-keyword-arg content :uttnum))))
		         ))))
	 (mapcar #'parse (get-cached-messages-and-clear))
     #|| ((utterance word phrase start-new-sentence start-sentence started-speaking stopped-speaking word-backto getanswers lattice prefer prefix) 
					; Most TELLs get passed to PARSE||#
	 (let ((result (parse content)))
	   ;; if parse returned a result, we send it out
	   (when (consp result) (send-new-speech-act result)))))

      (otherwise
       (reply msg 'error
	      :comment (format nil "bad tell: ~A" (first content))))))))
