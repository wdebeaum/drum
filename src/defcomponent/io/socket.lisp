;;;;
;;;; socket.lisp: communicate with another process through a socket
;;;;
;;;; $Id: socket.lisp,v 1.6 2006/03/29 22:33:15 nchambers Exp $
;;;;
;;;; gf: 21 Jan 2004: No more convert-to-package since we need to
;;;;      preserve some of the package prefixes in messages.
;;;;

(in-package :io)

(defvar *facilitator-host* "localhost"
  "Host at which to contact the facilitator in OPEN-SOCKET.")

(defvar *facilitator-port* 6200
  "Port at which to contact the facilitator in OPEN-SOCKET.")

(defvar *socket-table* (make-hash-table)
  "Hashtable containing per-module socket streams.")

(defun initialize-socket (&key host port &allow-other-keys)
  "Initializes the socket-based communication layer."
  (multiple-value-bind (envhost envport) (get-socket-environment)
    (setq *facilitator-host* (or host envhost *facilitator-host*))
    (setq *facilitator-port* (or port envport *facilitator-port*))
    (format *trace-output* "~&io: initialize-socket: ~A:~S~%"
	    *facilitator-host* *facilitator-port*)))

(defun get-socket-environment ()
  "Returns two values, a hostname and port number, parsed from the environment
variable TRIPS_SOCKET (in the format host:port). Returns NIL for missing
components."
  (let ((s (trips:get-env "TRIPS_SOCKET")))
    (when s
      ;; Have envvar
      (let ((i (position #\: s)))
	(if i
	    ;; Have colon
	    (if (eql i 0)
		;; Only port
		(values nil (read-from-string (subseq s 1)))
	      ;; Have both
	      (values (subseq s 0 i) (read-from-string (subseq s (1+ i)))))
	  ;; No colon: host only
	  (values s nil))))))

(defun connect-socket (module)
  "Connects the socket communication channel for given MODULE."
  (let ((stream (create-socket-stream *facilitator-host* *facilitator-port*)))
    (setf (gethash module *socket-table*) stream)))

(defun create-socket-stream (host port)
  (trips:make-socket host port))

(defun send-socket (module msg)
  "Sends message MSG using the socket message-passing framework."
  (let* ((stream (gethash module *socket-table*)))
    (when *verbose*
      (format *trace-output* ";; ~A sending:~%   ~S~%" module msg))
    (format stream "~S~%" msg)
    (finish-output stream)))

(defun recv-socket (module)
  "Returns the next message using the socket message-passing framework
or NIL on end-of-file.
Symbols in the message are interned into the current package."
  (let* ((stream (gethash module *socket-table*))
	 (msg (read stream nil nil)))
    (when *verbose*
      (format *trace-output* ";; ~A received:~%   ~S~%" module msg))
    msg))

(defun ready-socket (module)
  "Returns T if a message is waiting for this module, otherwise NIL.
Note that this cannot in general guarantee that a call to RECV will
not block."
  (let ((stream (gethash module *socket-table*)))
    (listen stream)))

(defun shutdown-socket (module)
  "Shuts down down the socket-based communication layer."
  (let ((stream (gethash module *socket-table*)))
    (close stream)))
