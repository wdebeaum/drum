;;;;
;;;; messages.lisp: Message-passing interface
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 14 Apr 1998

;; Modified June 2003 by Scott C. Stoness
;; Adding message-passing interface to the lexicon manager
;; Based on the above file by George Ferguson (mostly shamelessly copied)

(in-package :lexiconmanager)
(in-component :lxm)

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

;; Currently supported request messages [from before dfc conversion --wdebeaum]:
;; EXIT    exits
;; EVAL    evaluates arbitrary code (should this be turfed? ScS)
;; CHDIR   changes directory (or maybe just logs the message; see below ScS)
;; RESTART does nothing
;; API-CALL handles requests to the lexicon manager through the published
;;          interface.
;; Otherwise, the handler replies with an error message.

;; add web link names dynamically to the lexicon
(defcomponent-handler
  '(request &key :content (add-names . *))
  (lambda (msg content)
    (let ((wordlist (get-keyword-arg content :content))
	  (wordtype (get-keyword-arg content :type)))	              
	  (if (and wordlist wordtype)
	      (reply-to-msg msg 'TELL :content (add-names :type wordtype
						   :content wordlist
						   ))
	    (reply-to-msg msg 'error
		   :comment (format nil "wrong number of arguments")))))
  :subscribe t)

;; add new entry dynamically to the lexicon
(defcomponent-handler
  '(request &key :content (add-lex-entry . *))
  (lambda (msg content)
    (let ((word (get-keyword-arg content :word))
	  (cat (get-keyword-arg content :cat))
	  (type (get-keyword-arg content :type)))	              
	  (if (and word cat type)
	      (reply-to-msg msg 'TELL :content (add-lex-entry :word word
						   :cat cat
						   :type type
						   ))
	    (reply-to-msg msg 'error
		   :comment (format nil "wrong number of arguments")))))
  :subscribe t)

;; retrieve all words in a sense that index to the same lfform
(defcomponent-handler
  '(request &key :content (get-aliases . *))
  (lambda (msg content)
    (let ((word (get-keyword-arg content :content)))
      (if word
	  (reply-to-msg msg 'TELL :content (get-aliases :content word))
	(reply-to-msg msg 'error
	       :comment (format nil "wrong number of arguments")))))
  :subscribe t)

;; the lexicon-interface functions, not via API interface (so the arguments aren't quoted)
(defun handle-using-lexicon-interface-function (msg args)
    (declare (ignore args))
  (let* ((*request-is-from* (get-keyword-arg msg :sender))
	 (content (get-keyword-arg msg :content))
         (result (apply (first content) (rest content))))
    (reply-to-msg msg 'TELL :content (sanitize-for-kqml result))))

(defcomponent-handler
  '(request &key :content (get-lf . *))
  #'handle-using-lexicon-interface-function
  :subscribe t)

(defcomponent-handler
  '(request &key :content (get-word-form . *))
  #'handle-using-lexicon-interface-function)
(defcomponent-handler
  '(request &key :content (get-words-from-lf . *))
  #'handle-using-lexicon-interface-function)
(defcomponent-handler
  '(request &key :content (get-word-def . *))
  #'handle-using-lexicon-interface-function)
(defcomponent-handler
  '(request &key :content (is-defined-word . *))
  #'handle-using-lexicon-interface-function)

; not bothering to convert this to dfc; presumably Nate isn't using it anymore.
; --wdebeaum
;      ;; Handle external API calls
;      ;; we're keeping this around for now for people who still use it (e.g. Nate C.)
;      ;; but it requires function arguments to be quoted & this was causing problems
;      ;; for people using Java to build queries
;      (API-CALL
;       ;; Evaluate the command and get the result.
;       (let ((result (evaluate-api-call (second content))))
;	 ;; If the result is 'NOT-PUBLIC-ERROR send a reply message that 
;	 ;; lets the caller know that the function they called wasn't public.
;         (cond
;          ((equal result 'NOT-PUBLIC-ERROR)
;           (reply msg 'error
;                  :comment (format nil "bad function: ~A" (second content))))
;	  ;; Otherwise, reply to the message, encapsulating it in a 
;	  ;; (TELL :content (API-RESULT)) message type.
;          (t
;           (reply msg 'TELL  :content `(API-RESULT ,result))
;           )
;	  )
;         )
;       )

; exit
(defcomponent-handler
  '(request &key :content (exit . *))
  (lambda (msg content)
      (declare (ignore msg))
    (throw :exit (or (first content) 0))))

; ; eval
; (defcomponent-handler
;   '(request &key :content (eval . *))
;   (lambda (msg content)
;     (eval (second content))))

; CHDIR
(defcomponent-handler
  '(request &key :content (chdir . *))
  (lambda (msg content)
      (declare (ignore msg))
    (logging2:chdir (first content))))

; RESTART
(defcomponent-handler
  '(request &key :content (restart . *))
  (lambda (msg args)
    (declare (ignore msg args))
    (clear-dynamic-lexicon)
    (setq *domain-sense-preferences-tmp* nil)))
   ;#'list) ; do (practically) nothing

;; AT this point, the following tell messages are handled [again, from before dfc --wdebeaum]:

;; START-CONVERSATION    06/14/06 no longer ignored -- now LxM resets dynamic lexicon hash table
;; END-CONVERSATION      (ignored)
;; NEW-SCENARIO          (ignored)
;; MODIFY-SCENARIO       (ignored)
;; API-RESULT            (passed to receive-api-result handler)

;; Otherwise, a message of type ERROR is sent in reply.

(defcomponent-handler
  '(tell &key :content (start-conversation . *))
  (lambda (msg args)
      (declare (ignore msg args))
      (clear-dynamic-lexicon)
      (setq *domain-sense-preferences-tmp* nil)))
      
(defcomponent-handler
  '(tell &key :content (end-conversation . *))
  #'list) ; ignore

;; Added by ScS for handling API-RESULTs.  
;; Basically just calls a handler function which does nothing but
;; print the request and the result.  
(defcomponent-handler
  '(tell &key :content (API-RESULT . *))
  (lambda (msg args)
      (declare (ignore args))
    (receive-api-result msg)))

(defcomponent-handler
  '(tell &key :content (new-scenario . *))
  #'list)

(defcomponent-handler
  '(tell &key :content (modify-scenario . *))
  #'list)


(defcomponent-handler
  '(request &key :content (lex-pref-sense . *))
  #'(lambda (msg args)
      (let* ((content (get-keyword-arg msg :content))
	     (senses (get-keyword-arg content :content))
	     (status (get-keyword-arg content :status))
	     )
	(if (eq status 'permanent)
	    (setf lxm::*domain-sense-preferences* (append senses lxm::*domain-sense-preferences*)) ; senses go before original domain-sense-preferences
	  (setf lxm::*domain-sense-preferences-tmp* senses)
	  )
	)
      )
  :subscribe t)


(defcomponent-handler
  '(tell &key :content (NEW-SPEECH-ACT . *)) 
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (NEW-SPEECH-ACT-HYPS . *)) 
  #'(lambda (msg args)
      (handle-message msg))
  :subscribe t)

#|
(defcomponent-handler
  '(tell &key :content (paragraph-completed . *)) ; could listen for failed-to-parse too, but maybe wait till we have a successful parse
  #'(lambda (msg args)
      (setf lxm::*domain-sense-preferences-tmp* nil)
      )
  :subscribe t)
|#

(defun handle-message (msg)
  (let ((content (get-keyword-arg msg :content))
	(sender (get-keyword-arg msg :sender))
	)
    (case (first content)
      ((NEW-SPEECH-ACT NEW-SPEECH-ACT-HYPS)
       (setf lxm::*domain-sense-preferences-tmp* nil)
       ))))



#||
As far as I know, none of the rest of this file is used anymore, and pretty much all of it has better ways to do it now with dfc. --wdebeaum

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
||#
