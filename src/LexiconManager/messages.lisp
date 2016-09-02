;;;;
;;;; messages.lisp: Message-passing interface
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 14 Apr 1998

;; Modified June 2003 by Scott C. Stoness
;; Adding message-passing interface to the lexicon manager
;; Based on the above file by George Ferguson (mostly shamelessly copied)

(in-package :lexiconmanager)

(defvar *module-name* 'lexiconmanager
  "Name under which to register this module when we startup. This is also
used to find the package we should be in runtime when reading messages,
and the name of the logfile used to log messages to an from this module.")

(declaim (special LOGGING::*log-filename* LOGGING::*log-stream*))

;; This is the main control loop for the lexiconmanager when it is 
;; running as its own process.

(defun run ()
  "Main loop for the lexiconmanager"
  ;; Open communications for this module
  (COMM:connect *module-name*)
  ;; Make sure we're in the right package during i/o
  (let ((*package* (find-package *module-name*))
	(LOGGING::*log-filename* (format nil "~A.log" *module-name*))
	(LOGGING::*log-stream* nil))
    (LOGGING:chdir ".")
    ;; register with the facilitator
    (output `(register :name ,*module-name* :group system))
    ;; subscriptions
    (output '(subscribe :content (request &key :content (get-lf . *))))
    (output '(subscribe :content (request &key :content (add-names . *))))
    (output '(subscribe :content (request &key :content (add-lex-entry . *))))
    (output '(subscribe :content (request &key :content (get-aliases . *))))
    ;; ready
    (output '(tell :content (module-status ready)))
    (catch :exit
      (with-simple-restart (abort (format nil "Terminate ~A main loop" *module-name*))
	(loop
	  (catch :main-loop
	    (with-simple-restart (abort (format nil "Restart ~A main loop" *module-name*))
	      ;; (trips:gc t)
		  ;; set up the variable msg to hold the next message for this module
		  ;; calls the input() function to get the actual message.
	      (let ((msg (input)))
			;; If there is no message, freak out, because input() should have
			;; waited until there was a message.  Otherwise, handle the message
			;; with the receive-message function
		(if (null msg)
		    (throw :exit 0)	; EOF => exit
		  (handler-bind
		      ((error #'(lambda (err)
				       (indicate-error (symbol-name *module-name*) err msg))))
		    (receive-message msg)))))))))
))

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

;; This function waits for, and then logs and grabs the next message for this 
;; module.  Would have to look in the COMM package, I'd guess to figure out if 
;; this is a busy-wait or something nicer.  At this point, doesn't matter much,
;; but probably will for efficiency.  It is probably a thread suspend-interrupt
;; happening, but it is worth checking at some point.  ScS

(defun input ()
  "Input, log, and return the next message for this module."
  (let ((msg (COMM:recv *module-name*)))
    (LOGGING:log-message-received msg)
    msg))

;; should this be in utils?
(defun get-keyword-arg (l key)
  "Returns element following KEY in L, or NIL."
  (cond ((and (consp l) (consp (cdr l)))
	 (if (eq key (car l))
	     (cadr l)
	   (get-keyword-arg (cdr l) key)))
	(t
	 nil)))

(defun sanitize-for-kqml (x)
  "Return a version of x that's safer to output as KQML."
  (if (consp x)
    (cons (sanitize-for-kqml (car x)) (sanitize-for-kqml (cdr x)))
    (let* ((str (format nil "~s" x))
           (first-char (elt str 0)))
      (case first-char
        (#\(
	  #\) ; for editor balance
	  ;; turn it into a real list and then santize that
	  (sanitize-for-kqml (read-from-string str)))
	(#\#
	  (warn "double-quoting an expression that is unsafe for KQML: ~a" str)
	  str)
	(otherwise
	  x)
	))
    ))

;; This handles the various message types
;; at this point, only REQUEST, TELL, ERROR, SORRY are handled,
;; and ERROR and SORRY do nothing.
(defun receive-message (msg)
  "Dispatches message MSG for processing."
  ;; Replies are not necessarily sent using REPLY verb anymore.
  ;; Instead, if we have an in-reply-to tag ...
  (if (get-keyword-arg msg :in-reply-to)
      (receive-reply msg)
    (case (car msg)
      (request				; REQUEST  call receive-request function
       (receive-request msg))
      (tell				; TELL  call receive-tell function
       (receive-tell msg))
      ((error sorry)			; ERROR or SORRY (ignored)
       t)
      (otherwise			; Unknown performative!
       (reply msg 'error
	      :comment (format nil "unknown verb in message: ~S" msg)))))
  )

;; This function handles request messages of all sorts.  Currently supported:
;; EXIT    exits
;; EVAL    evaluates arbitrary code (should this be turfed? ScS)
;; CHDIR   changes directory (or maybe just logs the message; see below ScS)
;; RESTART does nothing
;; API-CALL handles requests to the lexicon manager through the published
;;          interface.
;; Otherwise, the handler replies with an error message.
(defun receive-request (msg)
  "Handles REQUEST messages received by the lexicon manager."
  (let ((content (get-keyword-arg msg :content))
	(*request-is-from* (get-keyword-arg msg :sender)))
    ;;(sender (get-keyword-arg msg :sender)) ;; sender not used
    (case (first content)
      (add-names  ;; add web link names dynamically to the lexicon
       (let ((wordlist (get-keyword-arg content :content))
	     (wordtype (get-keyword-arg content :type)))	              
	     (if (and wordlist wordtype)
		 (reply msg 'TELL :content (add-names :type wordtype						      
						      :content wordlist
						      ))
	       (reply msg 'error
		      :comment (format nil "wrong number of arguments"))))
	 )

      (add-lex-entry  ;; add new entry dynamically to the lexicon
       (let ((word (get-keyword-arg content :word))
	     (cat (get-keyword-arg content :cat))
	     (type (get-keyword-arg content :type)))	              
	     (if (and word cat type)
		 (reply msg 'TELL :content (add-lex-entry :word word
						      :cat cat
						      :type type
						      ))
	       (reply msg 'error
		      :comment (format nil "wrong number of arguments"))))
	 )

       (get-aliases  ;; retrieve all words in a sense that index to the same lfform
       (let ((word (get-keyword-arg content :content)))
	 (if word
	     (reply msg 'TELL :content (get-aliases :content word))
	   (reply msg 'error
		  :comment (format nil "wrong number of arguments"))))
       )
       
      ;; the lexicon-interface functions, not via API interface (so the arguments aren't quoted)
      ((get-lf
	get-word-form
	get-words-from-lf
	get-word-def
	is-defined-word)
       (let ((result (apply (first content) (rest content))))
	 (reply msg 'TELL  :content (sanitize-for-kqml result))))

      ;; Handle external API calls
      ;; we're keeping this around for now for people who still use it (e.g. Nate C.)
      ;; but it requires function arguments to be quoted & this was causing problems
      ;; for people using Java to build queries
      (API-CALL
       ;; Evaluate the command and get the result.
       (let ((result (evaluate-api-call (second content))))
	 ;; If the result is 'NOT-PUBLIC-ERROR send a reply message that 
	 ;; lets the caller know that the function they called wasn't public.
         (cond
          ((equal result 'NOT-PUBLIC-ERROR)
           (reply msg 'error
                  :comment (format nil "bad function: ~A" (second content))))
	  ;; Otherwise, reply to the message, encapsulating it in a 
	  ;; (TELL :content (API-RESULT)) message type.
          (t
           (reply msg 'TELL  :content `(API-RESULT ,result))
           )
	  )
         )
       )
            
      (exit				; exit
       (throw :exit (or (second content) 0)))
      ;;  (eval				; eval
      ;;(eval (second content)))
      (chdir				; CHDIR
       (LOGGING:chdir (second content)))
      (restart				; RESTART
       nil)
      (otherwise
       (reply msg 'error
	      :comment (format nil "bad request: ~A" (first content)))))))

;; Receive-tell handles TELL messages.  AT this point, the following 
;; messages are handled:

;; START-CONVERSATION    06/14/06 no longer ignored -- now LxM resets dynamic lexicon hash table
;; END-CONVERSATION      (ignored)
;; NEW-SCENARIO          (ignored)
;; MODIFY-SCENARIO       (ignored)
;; API-RESULT            (passed to receive-api-result handler)

;; Otherwise, a message of type ERROR is sent in reply.

(defun receive-tell (msg)
  "Handles TELL messages received by the lexiconmanager."
  (let ((content (get-keyword-arg msg :content)))
    (case (first content)
      (start-conversation 
       (clear-dynamic-lexicon))
      
      (end-conversation ; ignore
       nil)

	  ;; Added by ScS for handling API-RESULTs.  
	  ;; Basically just calls a handler function which does nothing but
	  ;; print the request and the result.  
	  
	  
      (API-RESULT (receive-api-result msg))
      (new-scenario
       nil)
      (modify-scenario
       nil)
      (otherwise
       (reply msg 'error
	      :comment (format nil "bad tell: ~A" (car content)))))))

(defun reply (msg verb &rest parameters)
  "Generate a reply to MSG of type VERB with given PARAMETERS.
The receiver of the message will be the sender of the MSG, and
any :reply-with will be used as :in-reply-to in the reply."
  (let ((sender (get-keyword-arg msg :sender))
	(reply-with (get-keyword-arg msg :reply-with)))
    (output (list* verb 
		   :receiver sender
		   :in-reply-to reply-with
		   parameters))))

(defun output (msg)
  "Output (and log) message MSG. Returns the message that was sent."
  (COMM:send *module-name* msg)
  (LOGGING:log-message-sent msg)
  msg)

;; Stuff for evaluating API-CALLS and ensuring that they are only calling
;; available public functions which have been exported from the ontology 
;; manager

;; *public-functions* is just a list of function calls that can be made 
;; using external messages.  At this point, nothing is done to check that 
;; the calls are being made with the right arguments or anything, just 
;; that the call is to a publically available function.


(defvar *public-functions*
  '(get-lf get-word-form get-words-from-lf
	get-word-def add-names add-lex-entry get-aliases))


;; This will evaluate the public function calls, and return 'NOT-PUBLIC-ERROR
;; for any functions that someone tried to call that are not in our published
;; interface (as represented by the *public-functions* variable above).

(defun evaluate-api-call (msg)
  "receives the message content from other modules
makes the function calls if the msg is valid
returns : function return value"
  ;;(format t "got ~S~%" msg)
  ;;(format t "(first msg) = ~S~%" (first msg))
  (if (member (first msg) *public-functions*)
      (eval msg)
    'NOT-PUBLIC-ERROR)
  )

(defun receive-api-result (msg)
  (let ( (answer (second (get-keyword-arg msg :content)))
		(query  (get-keyword-arg msg :in-reply-to))
		)
	(format t "Query ~S~%" query)
	(format t "Answer ~S~%" answer)
	
	)
  )

;; ScS Stuff for calling lexicon manager (pulled from ontology manager).
;; Probably want a nice function like send-lex that calls the ontology 
;; manager with whatever question I have...

;;
;; use with something nice like (send-lex '(get-lf 'truck))

(defun send-lex (msg)
  (output (list* 
			'request
			:receiver 'lexiconmanager
			:sender 'ontologymanager
			:reply-with msg
			:content (list (list 'API-CALL msg))
			)
		  )
  )


(defun test-lxm ()
  (output '(request 
			:receiver lxm
			:sender om
			:reply-with (get-lf 'w::computer)
			:content (API-CALL (get-lf 'w::computer))
			)
		  )
  )

(defun test-wf ()
  (output '(request 
			:receiver wf
			:sender lxm
			:reply-with (unknown-word 'w::frog)
			:content (API-CALL (unknown-word 'w::frog))
			)
		  )
  )


;; Directly snagged from Scott's parser, that handles synchronous callbacks 
;; as well as asynchronous.  If send-with-synchronous-continuation is never
;; called, the extra code *should* do no harm.


(defvar *reply-callbacks* nil)

(defvar *synchronous-callbacks* nil
  "List of tags of synchronous calls;  parser should pause until all have been answered")

(defun send-with-synchronous-continuation (msg cont)
  "Sends MSG, calls back CONT when a reply arrives, and halts further parser operations meanwhile."
  (send-with-continuation-generic msg cont t)
  )

(defun send-with-continuation (msg cont)
  "Sends MSG and calls back CONT when a reply arrives."
  (send-with-continuation-generic msg cont nil)
  ;;  (let ((tag (gentemp "PA")))
  ;;(push (cons tag cont) *reply-callbacks*)
  ;;(output (append msg (list :reply-with tag)))))
  )

(defun send-with-continuation-generic (msg cont synchronous)
  (let ((tag (gentemp "PA")))
	(push (cons tag cont) *reply-callbacks*)
	(when synchronous (push tag *synchronous-callbacks*))
	(output (append msg (list :reply-with tag)))
	)
  )

;; Modified July 13, 2003 by ScS
;; receive-reply now handles generic replies and also
;; API-RESULT replies.  The difference is that an API-RESULT
;; reply has API-RESULT as the car of its :content

(defun receive-reply (msg)
  (let* ((tag (get-keyword-arg msg :in-reply-to))
		 (response (get-keyword-arg msg :content))
	 (cont (cdr (assoc tag *reply-callbacks*))))
    
    (when cont
      ;; Remove pending callback
	  (setf *reply-callbacks* (delete tag *reply-callbacks* :key #'car))
	  
	  ;; Remove from synchronous wait list as well...
	  (setf *synchronous-callbacks* (delete tag *synchronous-callbacks*))
	  
	  ;; Handle various types of replies, based on first word of
	  ;; response.  If it isn't a specified reply type, then just send
	  ;; the whole response to the continuation.
	  ;; If the response is nil or not a list, also just send the whole content...
	  
	  (if (and response (listp response))
		  (case (car response) 
			(API-RESULT (funcall cont (second response)))
			;; Just hand back the content (might want it all, but...)
			(otherwise (funcall cont (get-keyword-arg msg :content)))
			)
		(funcall cont (get-keyword-arg msg :content))
		)
	  )
	)
  )


(defun send-with-no-expected-reply (msg)
  (send-with-continuation msg 
                          #'(lambda (x)
                              (handle-unexpected-response x msg))))

(defun handle-unexpected-response (content msg)
  (lexiconmanager-warn "Received unexpected reply to ~S: ~S" msg content))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
(defun send-lxm (msg &key (act 'request))
  "send a request to the LexcionManager  i.e. (send-lxm '(get-lf 'truck))"
  (output (list* 
	   act
	   :receiver 'lexiconmanager
	   :sender 'lexiconmanager
	   :reply-with msg
	   :content (list msg))))

(defun send-om (msg &key (act 'request))
  "send a request to the OntologyManager i.e. (send-om '(get-parent 'ont::move))"
  (output (list* 
	   act
	   :receiver 'ontologymanager
	   :sender 'lexiconmanager
	   :reply-with msg
	   :content (list msg))))

