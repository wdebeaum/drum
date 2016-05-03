;;;;
;;;; internal.lisp: Override standard COMM functions to support multiple
;;;;		    modules in the same Lisp image
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; $Id: internal.lisp,v 1.4 2010/01/18 19:07:48 ferguson Exp $
;;;;
;;;; gf: 18 Jul 2000: The mapping from threads to clients used to identify
;;;;     the sender of messages without :sender is broken if a client itself
;;;;     consists of multiple threads. For now, they just have to specify
;;;;     :sender if they want it to be accurate.
;;;;
;;;; gf: 21 Jan 2004: Changed to print to and read from a string to
;;;;     avoid package problems (or at least make them the same as the
;;;;     socket version).
;;;;

(in-package :io)

(defvar *check-message-syntax* t
  "If true, verify that messages will work properly when run outside
the internal Lisp image debugging transport.")

;;;
;;; Multi-processing functions
;;;

(defun get-current-process ()
  "Returns the currently-running process."
  (trips:current-process))

(defun block-process (process)
  "Blocks a PROCESS that is waiting for input."
  (trips:process-suspend process)
  ;; Return something meaningful for tracing
  (values (get-current-process) :awakes))

(defun wakeup-process (process)
  "Wakes up a PROCESS that is waiting for input."
  (trips:process-resume process))

;;;
;;; A registry is used by the pseudo-Facilitator to route messages
;;;

;;;
;;; REGISTRY-OBJECT is the common superclass of things that can be sent to
;;;
(defclass registry-object ()
  ((name :initform nil :initarg :name :accessor registry-object-name)
   (aliases :initform nil :initarg :aliases :accessor registry-object-aliases)
   (listeners :initform nil :initarg :listeners :accessor registry-object-listeners)))

(defmethod print-object ((obj registry-object) stream)
  (if *print-readably*
      (call-next-method)
    (print-unreadable-object (obj stream :type t)
      (princ (registry-object-name obj) stream))))

(defvar *registry* nil
  "List of all registry-objects (clients and groups) known to the
pseudo-Facilitator.")
(setq *registry* nil)

(defun registry-list ()
  "Returns a list of all known registry objects. Copy this list before
destructively modifying it!"
  *registry*)

(defun add-to-registry (robj)
  "Adds registry-object ROBJ to the registry."
  (push robj *registry*))

(defun lookup-by-name (name)
  "Returns the registry-object for the given NAME.
This checks both the primary name and the list of aliases."
  (find name *registry* :test #'match-name-or-alias))

(defun match-name-or-alias (name robj)
  "Returns non-NIL if NAME matches (using STRING=) either the name of ROBJ
or one of its aliases."
  (or (string= (registry-object-name robj) name)
      (find name (registry-object-aliases robj) :test #'string=)))

(defun add-alias (robj name)
  "Adds NAME as an alias for registry-object ROBJ"
  (push name (registry-object-aliases robj)))

(defun add-listener (robj listener)
  (pushnew listener (registry-object-listeners robj)))

;;;
;;; A CLIENT has an associated messageq and (eventually) process
;;;
(defclass client (registry-object)
  ((messageq :initform nil :initarg :messageq :accessor client-messageq)
   (process :initform nil :initarg :process :accessor client-process)
   (groups :initform nil :initarg :group :accessor client-groups)
   ;; This is my lame attempt to give multiple functions for this slot
   (name :accessor client-name)
   (listeners :accessor client-listeners)))

(defun new-client (&key name process)
  "Creates and returns a new CLIENT with the given NAME and PROCESS."
  (let ((c (make-instance 'client :name name :process process)))
    (add-to-registry c)
    c))

(defun set-client-process (client process)
  "Sets the PROCESS associated with the given CLIENT."
  (setf (client-process client) process))

(defun lookup-by-process (process)
  "Returns the client structure for the given PROCESS."
  (loop for robj in *registry*
      when (and (typep robj 'client)
		(eq (client-process robj) process))
      return robj))

(defun clients-list ()
  "Returns a list of all registered clients (not including groups)."
  (remove-if-not #'(lambda (x)
		     (typep x 'client)) (registry-list)))

(defgeneric add-message (c msg &key wakeup))

(defmethod add-message ((c client) msg &key (wakeup t))
  "Adds MSG to the list of messages for client C. If keyword arg WAKEUP
is true (the default), then wakes up the client's associated process if
it is waiting for a message."
  ;;(format *trace-output* ";; add-message for client ~S~%" c)
  (setq msg (prin1-to-string msg))
  (setf (client-messageq c)
    (append (client-messageq c) (list msg)))
  (when (and wakeup (client-process c))
    (wakeup-process (client-process c))))

(defgeneric get-message (c))

(defmethod get-message ((c client))
  "Removes and returns the first element in the message queue for CLIENT.
If the queue is empty, puts the current process to sleep until a message is
added."
  ;; Should have a critical section here to avoid race once woken up...
  (loop while (not (client-messageq c))
      do (block-process (get-current-process)))
  (read-from-string (pop (client-messageq c))))

(defgeneric wakeup-client (c))

(defmethod wakeup-client ((c client))
  "Wakes up client C's process."
  (when (client-process c)
    (wakeup-process (client-process c))))

;;;
;;; A GROUP is a set of clients. Groups are organized in an inheritance tree.
;;;
(defclass group (registry-object)
  ((parent :initform nil :initarg :parent :accessor group-parent)
   (members :initform nil :initarg :members :accessor group-members)))

(defun new-group (&key name parent)
  "Creates and returns a new group with the given NAME and PARENT."
  (let ((g (make-instance 'group :name name :parent parent)))
    (add-to-registry g)
    g))

(defun find-or-create-group (name)
  "If NAME is found in the registry, returns the registry-object (hopefully
a group), otherwise creates and returns a new group."
  (or (lookup-by-name name)
      (new-group :name name)))

(defun add-client-to-group (c g)
  "Makes client C a member of group G."
  ;;(format *trace-output* ";; adding client ~S to group ~S~%" c g)
  (push c (group-members g))
  (push g (client-groups c)))

(defun is-subgroup (gp gc)
  "Returns non-NIL if group GC is a child (or grandchild, etc.) of
group GP."
  (let ((parent (group-parent gc)))
    (or (eql gp parent)
	(and parent (is-subgroup gp parent)))))

(defun client-is-in-group (c g)
  "Returns non-NIL if client C is a member of group G or any of its
subgroups."
  (some #'(lambda (gc)
	    (or (eql g gc)
		(is-subgroup g gc)))
	(client-groups c)))

(defmethod add-message ((g group) msg &key (wakeup t))
  "Sends message MSG to all members of group G."
  ;;(format *trace-output* ";; add-message for group ~S~%" g)
  (mapc #'(lambda (x)
	    (add-message x msg)) (group-members g))
  (when (group-parent g)
    (add-message (group-parent g) msg :wakeup wakeup)))

(defmethod get-message ((g group))
  (error "Can't call GET-MESSAGE on group ~S" g))

;;;
;;; These are the standard COMM functions
;;;
(defun initialize-internal (&rest args)
  "Initializes the internal messaging transport."
  (declare (ignore args))
  t)

(defun connect-internal (module)
  "Connects the internal communication channel for given MODULE."
  (declare (ignore module))
  t)

(defun send-internal (module msg)
  "Sends message MSG using the internal messaging transport."
  (declare (ignore module))
  ;; If desired, we check the message for things that will cause problems
  ;; when running with the real hub. Typically these are structures that
  ;; use a syntax that is illegal in KQML. We check here rather than in
  ;; in RECV-INTERNAL so that we can trip the error when the message is
  ;; sent rather than later.
  (when (and *check-message-syntax* (not (check-message-syntax msg)))
    (error "Illegal message syntax: ~S" msg))
  ;; We need to implement a fair amount of the smarts of the TRIPS
  ;; message-passing hub (Facilitator, formerly IM) to get this right...
  (let ((sender (get-keyword-arg msg :sender)))
    ;; If we don't have a :sender...
    (when (not sender)
      ;; ...We should add one by trying to id the client from the process
      ;; (the real facilitator does this from the incoming socket)
      (let* ((client (lookup-by-process (get-current-process))))
	;; If we have a client associated with this thread
	(if client
	    ;; Then that's what we need
	    (setq sender (intern (client-name client)))
	  ;; Otherwise we got called from a sub-thread and can't know
	  (setq sender '*unknown*))
	(setq msg (set-keyword-arg msg :sender sender))))
    ;; Possibly trace the message
    (when *verbose*
      (format *trace-output* ";; ~A sending:~%   ~S~%" sender msg))
    ;; Now check the :receiver
    (let ((receiver (get-keyword-arg msg :receiver)))
      (cond
       ;; If none given...
       ((not receiver)
	;; Then we are doing a broadcast to subscribers
	(facilitator-send-to-self-or-subscribers msg))
       ;; Otherwise...
       (t
	;; Check if it's an internal client
	(let* ((receiver-name (string-upcase (string receiver)))
	       (dest (lookup-by-name receiver-name)))
	  ;; If so...
	  (if dest
	      ;; ...Add the message to the client's queue
	      (add-message dest msg)
	    ;; ...Otherwise just say we would send this out
	    (when *verbose*
	      (format *standard-output* ";; -->~%   ~S~%" msg)))))))))

(defun recv-internal (module)
  "Returns the next message for this module using the internal messaging
transport."
  (declare (ignore module))
  (let ((client (lookup-by-process (get-current-process))))
    (if client
	(let ((msg (get-message client)))
	  (when *verbose*
	    (format *trace-output* ";; ~A received:~%   ~S~%"
		    (client-name client) msg))
	  msg)
      (error "Can't call receive message before REGISTERing"))))
    
(defun ready-internal (module)
  "Returns T if a message is waiting for this module, otherwise NIL.
Note that this cannot in general guarantee that a call to RECV will
not block."
  (declare (ignore module))
  (let ((client (lookup-by-process (get-current-process))))
    (when (and client (client-messageq client))
      t)))

(defun shutdown-internal (module)
  "Shuts down down the internal messaging transport."
  (declare (ignore module))
  t)

;;;
;;; Selective broadcasts get sent to all listeners
;;;
(defun facilitator-send-to-self-or-subscribers (msg)
  "Message MSG (which has no :receiver) is sent to any subscribers.
It is either a special message that the BB itself understands, or it will
be sent to all clients who have subscribed to messages matching it.
Note that we don't change the :receiver as the real Facilitator does..."
  ;; First check for our own messages...
  (let ((verb (symbol-name (car msg))))
    ;; string-equal ignores case
    (cond ((string-equal verb "register")
	   (facilitator-register msg))
	  ((string-equal verb "broadcast")
	   (facilitator-broadcast msg))
	  ((string-equal verb "subscribe")
	   (facilitator-subscribe msg))))
  ;; Then send to all subscribers
  (facilitator-send-to-subscribers msg))

;;;
;;; The pseudo-Facilitator processes messages intended for the
;;; Facilitator itself when internal messaging is being used.
;;;
(defun facilitator-register (msg)
  "Handles REGISTER message to Facilitator."
  (let ((name (get-keyword-arg msg :name)))
    (cond ((null name)
	   (cerror "Continue, ignoring this message"
		   "Missing :name parameter in REGISTER: ~S" msg)
	   (return-from facilitator-register nil))
	  (t
	   (let* ((name (string-upcase (string name)))
		  (client nil)
		  (current-process (get-current-process))
		  (cname (lookup-by-name name))
		  (cproc (lookup-by-process current-process)))
	     (cond ((and (null cname) (null cproc))
		    (setq client
		      (new-client :name name :process current-process)))
		   ((null cname)
		    (add-alias cproc name)
		    (setq client cproc))
		   (t
		    (set-client-process cname current-process)
		    (setq client cname)))
	     ;; Look after adding client to groups
	     (let ((groups (or (get-keyword-arg msg :groups)
			       (get-keyword-arg msg :group)
			       (get-keyword-arg msg :class))))
	       (cond ((null groups)
		      ;; Member of no groups
		      )
		     ((symbolp groups)
		      ;; Member of single group
		      (let ((g (find-or-create-group groups)))
			(add-client-to-group client g)))
		     ((listp groups)
		      ;; Member of multiple groups
		      (mapcar #'(lambda (group)
				  (let ((g (find-or-create-group group)))
				    (add-client-to-group client g))) groups)))))))))

(defun facilitator-broadcast (msg)
  "Handles BROADCAST messages by sending themto all known clients."
    (facilitator-broadcast-atomically-to-clients
     (get-keyword-arg msg :content)
     (clients-list)))

(defun facilitator-broadcast-atomically-to-clients (msg clients)
  "Sends MSG ``atomically'' to all CLIENTS. That is, the message is added
to all clients' message queues before any of them are woken up."
    (loop for c in clients
	do (add-message c msg :wakeup nil))
    (loop for c in clients
	do (wakeup-client c)))

;;
;; NOTE: This could be optimized by hashing something like the verb
;;       and car of the content before getting into matching.
;; NOTE: The matching is complicated by the need to match keyword/value
;;       pairs out of order and especially by the need to match :sender
;;       arguments using the group hierarchy.
;;
(defvar *subscriptions* nil
  "A list of (PERF-PATTERN . CLIENT) pairs, where CLIENT has SUBSCRIBEd
to messages matching PERF-PATTERN.")

(defun match-perf-pattern (pat msg)
  "Returns T if MSG matches perf-pattern PAT, otherwise NIL.
Matching is defined as follows:
  - The element * in PAT is a wildcard that matches anything,
    otherwise non-lists must be EQUAL and lists must match recursively.
  - The element &key in PAT indicates that the remaining elements (at
    this level of embedding) are keyword/value patterns. These are
    considered to match if for each key/value pattern in PAT, the
    key appears in MSG (at this level of embedding) and the corresponding
    value in MSG matches the corresponding value in PAT."
  (cond ((eq pat '*) 
	 t)
	((equal pat msg)
	 t)
	((and (consp pat) (consp msg))
	 (cond ((eq (car pat) '&key)
		(loop with args = (cdr pat)
		    with (key pval mval)
		    while args
		    do (setq key (pop args))
		       (setq pval (pop args))
		       (setq mval (get-keyword-arg msg key))
		    if (not (or (match-perf-pattern pval mval)
				(and (eq key :sender)
				     (match-perf-pattern-sender pval msg))))
		    return nil
		    finally (return t)))
	       (t
		(and (match-perf-pattern (car pat) (car msg))
		     (match-perf-pattern (cdr pat) (cdr msg))))))
	(t
	 nil)))

(defun match-perf-pattern-sender (pat msg)
  "Finds the client named by the :sender of MSG (or the client associated
with the current process if :sender is not given), and returns true if
either PAT names the same client or PAT names a group, and the sender
belongs to that group or one of its subgroups."
  (let ((g (lookup-by-name pat))
	(c (or (lookup-by-name (get-keyword-arg msg :sender))
	       (lookup-by-process (get-current-process)))))
    (or (eq g c)
	(and (typep g 'group) (client-is-in-group c g)))))

(defun facilitator-subscribe (msg)
  "Arranges for the sender of MSG to subscribe to messages matching the
content of MSG. That is, they will receive any matching messages subsequently
posted to the bulletin board."
  ;; Do convert-to-package here so that "default" symbols will match later
  (let* ((content (convert-to-package (get-keyword-arg msg :content) *io-package*))
	 (sender (get-keyword-arg msg :sender))
	 (client (or (lookup-by-name sender)
		     (lookup-by-process (get-current-process)))))
    (push (cons content client) *subscriptions*)))
    
(defun facilitator-send-to-subscribers (msg)
  "Copies MSG to any clients who have subscribed with a matching pattern."
  ;; Do convert-to-package here so that "default" symbols will match
  (let ((io-msg (convert-to-package msg *io-package*)))
    (facilitator-broadcast-atomically-to-clients
     msg
     (mapcan #'(lambda (sub)
		 (let ((pat (car sub))
		       (client (cdr sub)))
		   (when (match-perf-pattern pat io-msg)
		     (list client))))
	     *subscriptions*))))

#||
(trace facilitator-register
       facilitator-broadcast
       facilitator-bulletin-board
       bulletin-board-subscribe
       bulletin-board-broadcast
       ;;match-perf-pattern
       ;;match-perf-pattern-sender
       )
||#

;;;
;;; Sanity checking for things that won't work outside of Lisp
;;; (used in SEND-INTERNAL, controlled by *CHECK-MESSAGE-SYNTAX*).
;;;
(defun check-message-syntax (msg)
  "Returns non-NIL is MSG is syntactically acceptable. The main thing
this will catch presently is anything that prints with a #, which is
typically a structure in #S(...) notation or something with a print
function that uses #<...>. It's somewhat extravagant to print into a
string, but its the only relaible way to know what will happen with a
real transport, and it's just for debugging anyway."
  (not (find #\# (format nil "~S" msg))))
