;;;;
;;;; messages.lisp: Message-passing interface to the parser
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 14 Apr 1998


;; Modified June 12, 2003 by Scott C. Stoness
;; Added comments in some of the message passing functions
;; Moved the evaluate function (re-named evaluate-api-call) and the 
;;    *public-functions* variable declaration into this file instead 
;;    of Code/public-helpers.lisp.
;; Changed the structure of incoming api-calls and results to:
;;    (REQUEST :content (API-CALL call))
;;    (TELL :content (API-RESULT result))
;;    as discussed with Nate Chambers. Uses the already present
;;    reply function to swap around sender/receiver and reply-with: etc.

(in-package "ONTOLOGYMANAGER")
(in-component :om)

#|
(defvar *module-name* 'ontologymanager
  "Name under which to register this module when we startup. This is also
used to find the package we should be in runtime when reading messages,
and the name of the logfile used to log messages to an from this module.")

(declaim (special LOGGING::*log-filename* LOGGING::*log-stream*))

(defun run ()
  "Main loop for the ontologymanager"
  ;; Open communications for this module
  (COMM:connect *module-name*)
  ;; Make sure we're in the right package during i/o
  (let ((*package* (find-package *module-name*))
	(LOGGING::*log-filename* (format nil "~A.log" *module-name*))
	(LOGGING::*log-stream* nil))
    (LOGGING:chdir ".")
    (output `(register :name ,*module-name* :group system))
    ;;    (output '(subscribe :content (request &key :content (COMMAND . *))))
    (output '(tell :content (module-status ready)))
    (catch :exit
      (with-simple-restart (abort (format nil "Terminate ~A main loop" *module-name*))
	(loop
	  (catch :main-loop
	    (with-simple-restart (abort (format nil "Restart ~A main loop" *module-name*))
	      ;; (trips:gc t)
	      (let ((msg (input)))
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
    ((ask-if ask-one ask-all)		; ASK-*
     nil)
    ((error sorry)			; ERROR or SORRY (ignored)
     t)
    (otherwise				; Unknown performative!
     (reply msg 'error
	    :comment (format nil "unknown verb in message: ~S" msg)))))

;; This function handles request messages of all sorts.  Currently supported:
;; EXIT    exits
;; EVAL    evaluates arbitrary code (should this be turfed? ScS)
;; CHDIR   changes directory (or maybe just logs the message; see below ScS)
;; RESTART does nothing
;; API-CALL handles requests to the ontology manager through the published
;;          interface.
;; Otherwise, the handler replies with an error message.

(defun receive-request (msg)
  "Handles REQUEST messages received by the parser."
  (let ((content (get-keyword-arg msg :content)))
    (case (first content)

      (transform
       (if (= 2 (length content))
	   (reply msg 'TELL :content (transform (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))
      
      (kr-atom-transform
       (if (= 2 (length content))
	   (reply msg 'TELL :content (kr-atom-transform (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (kr-role-transform
       (if (= 4 (length content))
	   (reply msg 'TELL :content (kr-role-transform (second content)
							(third content)
							(fourth content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (kr-term-transform
       (if (= 2 (length content))
	   (reply msg 'TELL :content (kr-term-transform (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (kr-terms-transform
       (if (= 3 (length content))
	   (reply msg 'TELL :content (kr-terms-transform (second content)
							 (third content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (lf-atom-transform
       (if (= 2 (length content))
	   (reply msg 'TELL :content (lf-atom-transform (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (get-transform-types
       (if (= 1 (length content))
	   (reply msg 'TELL :content (get-transform-types))
	 (reply msg 'error
		:comment (format nil "no arguments needed"))))

      (km-to-triples
       (cond ((= 2 (length content))
	      (reply msg 'TELL :content (km-to-triples (second content))))
	     ((= 3 (length content))
	      (reply msg 'TELL :content (km-to-triples (second content) (third content))))
	     (t (reply msg 'error
		       :comment (format nil "wrong number of arguments")))))

      (triples-to-km
       (cond ((= 2 (length content))
	      (reply msg 'TELL :content (triples-to-km (second content))))
	     ((= 3 (length content))
	      (reply msg 'TELL :content (triples-to-km (second content) (third content))))
	     (t (reply msg 'error
		       :comment (format nil "wrong number of arguments")))))

      (subtype
       (if (= 3 (length content))
	   (reply msg 'TELL :content (subtype (second content) (third content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (more-specific
       (if (= 3 (length content))
	   (reply msg 'TELL :content (more-specific (second content) (third content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (get-parent
       (if (= 2 (length content))
	   (reply msg 'TELL :content (get-parent (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (get-parents
       (if (= 2 (length content))
	   (reply msg 'TELL :content (get-parents (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (common-ancestor
       (if (= 3 (length content))
	   (reply msg 'TELL :content (common-ancestor (second content) (third content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (defined-p
	(let ((ont (get-keyword-arg content :ontology))
	      (cat (get-keyword-arg content :cat)))
	  (if (< 1 (length content))
	      (reply msg 'TELL :content (defined-p (second content)
					  :ontology ont
					  :cat cat))
	    (reply msg 'error
		   :comment (format nil "wrong number of arguments")))))

      (lf-sem
	(let ((none (get-keyword-arg content :no-defaults)))
	  (if (< 1 (length content))
	      (reply msg 'TELL :content (lf-sem (second content)
					  :no-defaults none))
	    (reply msg 'error
		   :comment (format nil "wrong number of arguments")))))

      (lf-arguments
       (if (= 2 (length content))
	   (reply msg 'TELL :content (lf-arguments (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (lf-coercion-operators
       (if (= 2 (length content))
	   (reply msg 'TELL :content (lf-coercion-operators (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (lf-GCB
       (if (= 3 (length content))
	   (reply msg 'TELL :content (lf-GCB (second content) (third content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (lf-feature-pattern-match
       (if (= 3 (length content))
	   (reply msg 'TELL :content (lf-feature-pattern-match (second content) (third content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (lf-feature-set-merge
       (if (= 3 (length content))
	   (reply msg 'TELL :content (lf-feature-set-merge (second content) (third content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (kr-add-to-lf
       (if (< 2 (length content))
	   ;; apply is not safe, but there are too many options to handle!
	   (reply msg 'TELL :content (apply #'kr-add-to-lf (rest content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (kr-slots
       (if (= 2 (length content))
	   (reply msg 'TELL :content (kr-slots (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (lf-info-for-class
       (if (= 2 (length content))
	   (reply msg 'TELL :content (lf-info-for-class (second content)))
	 (reply msg 'error
		:comment (format nil "wrong number of arguments"))))

      (who-am-i
       (reply msg 'TELL :content (who-am-i)))

      ;; NC - keep this around for now, in case people are using it	  
      ;; Handle an external API-CALL
      (API-CALL
       ;; Evaluate the call and establish the result
       (let ((result (evaluate-api-call (second content))))
	 ;; If the result is a NOT-PUBLIC-ERROR (e.g. the function isn't
	 ;; officially in the API), then send a message to that effect.
	 (cond
	  ((equal result 'NOT-PUBLIC-ERROR)
	   (reply msg 'error
		  :comment (format nil "bad function: ~A" (second content))))
	  ;; Otherwise, reply to the message, encapsulating it in a 
	  ;; (TELL :content (API-RESULT)) message type.
	  (t
	   (reply msg 'TELL :content `(API-RESULT ,result))
	   ))))

      
      (exit				; exit
       (throw :exit (or (second content) 0)))
      (chdir				; CHDIR
       (LOGGING:chdir (second content)))
      (restart				; RESTART
       nil)
      (otherwise
       (reply msg 'error
	      :comment (format nil "bad request: ~A" (first content)))))))

(defun receive-tell (msg)
  "Handles TELL messages received by the parser."
  (let ((content (get-keyword-arg msg :content)))
    (case (first content)
      ((start-conversation end-conversation) ; ignore
       nil)
      (new-scenario
       nil)
      (modify-scenario
       nil)
	  
	  ;; Added by ScS for handling API-RESULTs.  
	  ;; Basically just calls a handler function which does nothing but
	  ;; print the request and the result.  Need to move this out of the
	  ;; OM, because the OM never actually needs to send out calls or 
	  ;; receive answers.  Just in here for testing.  ScS
	  
      (API-RESULT 
	       (receive-api-result msg))
	  
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

(defun get-keyword-arg (l key)
  "Returns element following KEY in L, or NIL."
  (cond ((and (consp l) (consp (cdr l)))
	 (if (eq key (car l))
	     (cadr l)
	   (get-keyword-arg (cdr l) key)))
	(t
	 nil)))

;; ScS Stuff for handling ANSWERS

(defun receive-api-result (msg)
  (let ( (answer (second (get-keyword-arg msg :content)))
		(query  (get-keyword-arg msg :in-reply-to))
		)
	(format t "Query ~S~%" query)
	(format t "Answer ~S~%" answer)
	
	)
  )

;; Stuff for evaluating API-CALLS and ensuring that they are only calling
;; available public functions which have been exported from the ontology 
;; manager

;; *public-functions* is just a list of function calls that can be made 
;; using external messages.  At this point, nothing is done to check that 
;; the calls are being made with the right arguments or anything, just 
;; that the call is to a publically available function.

(defvar *public-functions*
  '(transform kr-atom-transform kr-role-value-transform
    subtype more-specific get-parent get-parents common-ancestor defined-p
    lf-sem lf-arguments lf-coercion-operators lf-GCB lf-feature-pattern-match
    lf-info-for-class lf-feature-set-merge kr-add-to-lf kr-slots))

;; Handles api-call evaluations basically by running the code that is 
;; passed in the message as long as it is a call to a function which is in 
;; the *public-functions* list.  

;; Commented out format statements.  ScS June 12.

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

;; ScS Stuff for calling lexicon manager

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

(defun test-lex ()
  (output '(request 
			:receiver lexiconmanager
			:sender ontologymanager
			:reply-with (get-lf 'truck)
			:content (API-CALL (get-lf 'truck))
			)
		  )
  )

(defun reloadme ()
  (load "../OntologyManager/messages.lisp")
  )

|#

(format *error-output* "~%~%LOADING MESSAGES~%~%")

(defcomponent-handler
    '(request &key :content (define-type . *))
    #'(lambda (msg args)
	(reply-to-msg msg 'reply
		      :content (when user::*build-ontology*
				 (if (apply #'add-new-type args) T))))
  :subscribe t)
#|
(defcomponent-handler
    '(tell &key :content (define-type . *))
    #'(lambda (msg args)
	(when user::*build-ontology* (apply #'add-new-type args)))
  :subscribe t)|#


(defmacro tell (&rest args)
  `(apply #'additional ',args))

(defun additional (&key content)
  (apply #'add-new-type (cdr content)))

(defmacro preliminary-define-type (&rest args)
  `(apply #'add-new-type ',args))

(defun add-new-type (&key (wnsense nil) (trips-type nil) (comment nil)
		       (wordnet-sense-keys nil) (parent nil)
		       (definitions nil) (arguments nil))
  (if (not (lf-sem trips-type))
      ;;  A new type, add the full definition
      (progn
	(lfontology-define-type trips-type :parent parent :wordnet-sense-keys wordnet-sense-keys
			      :comment comment
			      :arguments (mapcar #'(lambda (a)
						     (cons :required a)) arguments)
			      :definitions definitions
			      )
	(format t "~%Defined type: ~S" trips-type)
	(recompile-ontology))
      ;;  An existing type, we want to augment the existing definition with the axioms and comments
      (add-definition-to-existing-type trips-type definitions comment wnsense)
      ))

(defun add-definition-to-existing-type (type def comment wnsense)
  (let ((type-record (gethash type (ling-ontology-lf-table *lf-ontology*)))
	(simplified-def (simplify-def def type))
	(WNnewsense (symbol-name wnsense)))
    (when (and type-record simplified-def)
      (let ((new-definition (combine-defns def (lf-description-definitions type-record)))
	    (new-comment (if (lf-description-comment type-record)
			     (if comment
				 (concatenate 'string (lf-description-comment type-record)
					  ";;"
					  comment))
			     comment)))
	(print `(add-definition-to-existing-type :type ,type
						    :wnsense ,wnsense
						    :def ,simplified-def))
	#|	(send-msg `(request :content (add-definition-to-existing-type :type ,type
								      :wnsense ,wnsense
								      :def ,simplified-def)))|#
	(setf (lf-description-definitions type-record)
	      new-definition)
	(when (and WNnewsense (not (member WNnewsense (lf-description-wordnet-sense-keys type-record) :test #'equal)))
	    (setf (lf-description-wordnet-sense-keys type-record)
		  (cons WNnewsense (lf-description-wordnet-sense-keys type-record))))
	(when new-comment
	  (setf (lf-description-comment type-record)
		new-comment)))
      T)))

(defun simplify-def (def type)
  "check that we have non redundant content in the def before processing it"
  (cond ((eq (car def) 'ont::OR)
	 (let ((newdef (cons 'ONT::OR
			     (mapcar #'(lambda (x) (simplify-def x type)) (cdr def)))))
	   newdef))
     #| ;; check for redundant def
      (if (or (eq (car def) 'ont::and)
	      (member :formal def))|#
	((eq (car def) type)
	  (progn
	    (format t "~%Rejecting definition for type as redundant ~S:  ~S" type def)
	    nil))
	((and (eq (car def) 'ont::and)
	     (consp (cadr def))
	     (eq (caadr def) :*))
	 (cdr def))
	(t (simplify-def1 def)
	  
	   )))

(defun simplify-def1 (olddef)
  "remove WNsense and force"
  (when olddef
    (cond ((symbolp olddef)
	   olddef)
	  ((member (car olddef) '(ont::and ont::or))
	   (cons (car olddef)
		 (remove-if #'null (mapcar #'simplify-def1 (cdr olddef)))))
	  ((member (car olddef) '(ont::predicate))
	   nil)
	  ((consp olddef)
	   (cons (car olddef) (simplify-roles (cdr olddef))))
	  (t olddef))))

(defun simplify-roles (args)
  (cond ((member (car args) '(:wnsense :force))
	 (simplify-roles (cddr args)))
	((eq (car args) :formal)
	 ;;(format t "~% result is ~S" (simplify-def (cadr args)))
	 (list* :formal (simplify-def1 (cadr args))
		(simplify-roles (cddr args))))
	(args
	 (list* (car args) (cadr args) (simplify-roles (cddr args))))))
	  

(defun combine-defns (newdef olddef)
  (if (and newdef olddef)
      (let* ((newdefpreds (if (eq (car newdef) 'ont::or)
			      (cdr newdef)
			      (list newdef)))
	     (olddefpreds (if (eq (car olddef) 'ont::or)
			      (cdr olddef)
			      (list olddef)))
	     (reallynewpreds (remove-if #'(lambda (x) (defn-already-in-list x olddefpreds)) newdefpreds)))
	;;(format t "~%new=~S old=~S" newdefpreds olddefpreds)
       
	(if reallynewpreds
	    (cons 'ont::OR (append olddefpreds reallynewpreds))
	    olddef))
      (or newdef
	  olddef)))

(defun defn-already-in-list (newdefn olddefns)
  (some #'(lambda (x) (defn-match newdefn x nil)) olddefns))

(defun defn-match (x y bndgs)
   "True is they are identical modulo variable naming"
   (cond ((equal x y)
	  (or bndgs '((nil))))
	 ((or (null x) (null y))
	  NIL)
	 ((isvar x)
	  (let ((bound-value (cdr (assoc x bndgs))))
	    (if bound-value
		(defn-match bound-value y bndgs)
		(cons (cons x y) bndgs))))
	 ((and (consp x) (consp y))
	  (let ((newbndgs (defn-match (car x) (car y) bndgs)))
	    (if newbndgs
		(defn-match (cdr x) (cdr y) newbndgs))))
	 (t
	  nil)
	  ))

(defun isvar (x)
  (and x (symbolp x)
       (eq (first (coerce (symbol-name x) 'list))
	   #\?)))
