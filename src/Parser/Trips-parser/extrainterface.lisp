    (in-package parser)

(defvar *server-socket* nil)
(defvar *server-stream* nil)

;; this will get the command line arguments
;; figure out the transport
;; and the port if specified
(defun process-command-line-args ()
  (let ((args (make-arglist (sys:command-line-arguments)))
	)
     ;; now do the argument analysis
    ;; for now we just determine transport
    (LOGGING:log-message 'Myrosia-args args)
    (initialize-transport args)
    ))

;; this function checks transport and host/port numbers as required
(defun initialize-transport (arglist)
  (let ((transport (cdr (assoc "-transport" arglist :test #'equal)))
	(host (cdr (assoc "-host" arglist :test #'equal)))
	(port (cdr (assoc "-port" arglist :test #'equal)))
	)
    (format t "Initializing transport~%")
    ;; for now we only care about transport socket
    (cond
     ((equal transport "socket")
      (format t "Opening the socket~%")
      (COMM:initialize :transport :socket :host host :port (read-from-string port))
      (format t "Opened the socket~%")
      )
     ;; for now we only care about transport socket
     ((equal transport "socket-server")
      (setq port (or port "4445"))
      (format t "Making a server socket on port ~S~%" port)
      (setq *server-socket* (ACL-SOCKET:make-socket :connect :passive 
						    :local-port (read-from-string port)))
      (format t "Opening the server socket and waiting for the connection ~%")
      )
     )
    ))
	    
(defun make-arglist (l)
  (cond 
   ((null l) nil)
   ;; if the parameter is of the sort that needs value, we take the next thing
   ;; as a value
   ((value-parameter (first l))
    (cons (cons (first l) (second l)) (make-arglist (cddr l))))
   ;; otherwise the next parameter is a simple value
   (t (cons (cons (first l) nil) (make-arglist (cdr l))))
   ))


(defun value-parameter (s)
  (member s '("-host" "-port" "-transport") :test #'equal)
  )



;;;;; This is myrosia's main function to call the socket interface
;;;(defun run ()
;;;  "Main loop for the parser. Continues to add entries to chart until
;;;appropriate termination conditions are met. Handles new messages immediately."
;;;  ;; Initialize the module log
;;;  (LOGGING:initialize (format nil "~A.log" *module-name*))
;;;  (format t "Connecting the socket~%")
;;;  ;; a (hack) function to get transport/host/port from the command line
;;;  (LOGGING:log-message 'Myrosia "About to process command line args~%")
;;;  (process-command-line-args)
;;;
;;;  ;; Open communications for this module
;;;  (COMM:connect *module-name*)
;;;
;;;  ;; Make sure we're in the PARSER package during i/o (blech)
;;;  (let ((*package* (find-package *module-name*)))
;;;    (output `(register :name ,*module-name* :group system))
;;;    (output '(subscribe :content (tell &key :sender user-input)))
;;;    (output '(tell :content (status ready)))
;;;    (catch :exit
;;;      (with-simple-restart (abort (format nil "Terminate ~A main loop" *module-name*))
;;;	(loop
;;;	  (catch :main-loop
;;;	    (with-simple-restart (abort (format nil "Restart ~A main loop" *module-name*))
;;;	      ;; (excl:gc t)
;;;	      (let ((tmpmsg (format t "Waiting for the next event ~%")) 
;;;		    (event (get-next-event)))
;;;		(case (car event)
;;;		  (add-entry
;;;		   (add-entry-to-chart (cdr event)))
;;;		  (message
;;;		   (let ((msg (cdr event)))
;;;		     (format t "Received message: ~S ~% " msg)
;;;		     (if (null msg)
;;;			 (throw :exit 0) ; EOF => exit
;;;		       (handler-bind
;;;			   ((error #'(lambda (err)
;;;				       (indicate-error (symbol-name *module-name*) err msg))))
;;;			 (receive-message msg))))))))))))
;;;    (COMM::shutdown)
;;;    (excl:exit 0 :no-unwind t) 
;;;))


;; This is myrosia's main function that initializes the server and let's it accept connections
(defun run ()
  "Main loop for the parser. Continues to add entries to chart until
appropriate termination conditions are met. Handles new messages immediately."
  ;; Initialize the module log
  (LOGGING:initialize (format nil "~A.log" *module-name*))
  (format t "Connecting the socket~%")
  ;; a (hack) function to get transport/host/port from the command line
  (LOGGING:log-message 'Myrosia "About to process command line args~%")
  (process-command-line-args)
  
  (print user::*TRIPS_BASE*)
  (setf (logical-pathname-translations "TRIPS")
    `(("**;*.*.*" ,(format nil "~A/src/" user::*TRIPS_BASE*)))
    )  
  (cond
   ((null *server-socket*)
    ;; Open communications for this module
    (COMM:connect *module-name*)
    ;; If we're started as a client, we simply run the loop once and then shut down
    (main-run-loop)
    (COMM::shutdown)
    (excl:exit 0 :no-unwind t) 
    )
   (t ;; have a server socket - start a server
    (let ((finish nil))
      (loop until (eql finish 1)
	  do 
	    (format t "Waiting for clients to connect")
	    (setq *server-stream* (ACL-SOCKET:accept-connection *server-socket*))
	    (format t "Accepted a connection")
	    (setq finish (main-run-loop))
	    (close *server-stream*)
	    )
      (excl:exit 0 :no-unwind t) 
      ))    
   )
  )


(defun main-run-loop()
  "Returns t if the command to completely quit the server was received"
  ;; Make sure we're in the PARSER package during i/o (blech)
  (let ((*package* (find-package *module-name*)))
    (myoutput `(register :name ,*module-name* :group system))
    (myoutput '(subscribe :content (tell &key :sender user-input)))
    (myoutput '(tell :content (status ready)))
    (catch :exit
      (with-simple-restart (abort (format nil "Terminate ~A main loop" *module-name*))
	(loop
	  (catch :main-loop
	    (with-simple-restart (abort (format nil "Restart ~A main loop" *module-name*))
	      ;; (excl:gc t)
	      (let ((tmpmsg (format t "Waiting for the next event ~%")) 
		    (event (my-get-next-event)))
		(case (car event)
		  (add-entry
		   (add-entry-to-chart (cdr event)))
		  (message
		   (let ((msg (cdr event)))
		     (format t "Received message: ~S ~% " msg)
		     (cond 
		      ((null msg)
		       (throw :exit 0)	; EOF => exit
		       )
		      ((eql msg 'QUIT-SERVER)
		       (throw :exit 1))
		      (t
		       (handler-bind
			   ((error #'(lambda (err)
				       (indicate-error (symbol-name *module-name*) err msg))))
			 (receive-message msg)))
		      ))
		   ))
		)
	      ))
	  )
	))
    ))


(defun myoutput (msg)
  (if (null *server-socket*)
      (output msg)
    (send-server msg)
    )
  )


(defun send-server (msg)
  "Sends message MSG using the socket message-passing framework."
  (format *server-stream* "~S~%" msg)
  (finish-output *server-stream*)
  )

(defun recv-server () 
  "Gets a message from the server socket"
  (read *server-stream* nil nil)
  )

(defun my-get-next-event ()
  "Checks for a message, then for a pending parser action, otherwise waits
for a message. Returns a (TAG . ITEM) pair."
  (cond
   ((null *server-socket*)
    (get-next-event))
   ;; if we're here, we're in server mode - everything else down handles server stuff
   ((listen *server-stream*) ;; check if there's a message in the stream
    (cons 'message (server-input)))
   ((agenda-item-pending (entry-threshold)) ; pending parser action
    (cons 'add-entry (get-next-entry)))
   (t					; otherwise wait for input
    (cons 'message (server-input)))
   ))


(defun server-input ()
  "Input, log, and return the next message for this module."
  (let ((msg (recv-server)))
    (LOGGING:log-message-received msg)
    msg))

(defun receive-tell (msg)
  "Handles TELL messages received by the parser."
  (let ((content (get-keyword-arg msg :content)))
    (case (first content)
      (new-scenario			; NEW-SCENARIO
       (load-new-scenario (second content)))
      (modify-scenario			; MODIFY-SCENARIO
       (apply #'modify-scenario (rest content)))
      ((start-conversation end-conversation) ; start-conv/end-conv
       nil)
      (number-interps-desired		; NUMBER-INTERPS-DESIRED
       (set-number-of-interps-desired (cadr content)))
      ;; Myrosia's message to reload the grammar
      (reloadgrammar
       (format t "Calling change-scenario-directory ~S ~S ~%" (second content) (third content)) 
       (change-scenario-directory (second content) :kr-in-lexicon (third content))       
       )
      ((word started-speaking stopped-speaking word-backto utterance getanswers lattice) ; Most TELLs get passed to PARSE
       ;; Check if we're offline, and don't parse if so
       (when (not *parser-is-online*)
	 (format t "Sorry, not online yet~%")
	 (return-from receive-tell nil)	 
	 )
       ;; Otherwise parse, get result, and possibly send our own output
       (let ((result (parse :user content)))
	 (when (and (consp result) (listp (car result)))
           (myoutput `(tell :content (new-speech-act ,result))))))
      (otherwise
       (reply msg 'error
	      :comment (format nil "bad tell: ~A" (first content)))))))


