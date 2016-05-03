;;;;
;;;; stdio.lisp: Inter-module communication using stdio
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; Time-stamp: <Mon Feb  9 12:05:45 EST 2004 ferguson>
;;;;
;;;; gf: 21 Jan 2004: No more convert-to-package since we need to
;;;;      preserve some of the package prefixes in messages.
;;;;

(in-package :io)

(defvar *stdio-stream*
    (make-two-way-stream *standard-input* *standard-output*)
  "Stream used for stdio communication to/from the Input Manager. Defaults to
a stream based on *standard-input* and *standard-output*.")

(defun initialize-stdio (&rest args)
  "Initializes the stdio message-passing framework."
  (declare (ignore args))
  t)

(defun connect-stdio (module)
  "Connects the stdio communication channel for given MODULE."
  (declare (ignore module))
  t)

(defun send-stdio (module msg)
  "Sends message MSG using the stdio message-passing framework."
  (declare (ignore module))
  (format *stdio-stream* "~S~%" msg)
  (finish-output *stdio-stream*))

(defun recv-stdio (module)
  "Returns the next message using the stdio message-passing framework
or NIL on end-of-file."
  (declare (ignore module))
  (read *stdio-stream* nil nil))

(defun ready-stdio (module)
  "Returns T if a message is waiting for this module, otherwise NIL.
Note that this cannot in general guarantee that a call to RECV will
not block."
  (declare (ignore module))
  (listen *stdio-stream*))

(defun shutdown-stdio (module)
  (declare (ignore module))
  "Shuts down down the stdio message-passing framework."
  t)
