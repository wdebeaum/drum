;;;;
;;;; api.lisp: Generic functions to send and receive messages
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 11 Aug 1999
;;;; Time-stamp: <Tue Jun 25 12:24:06 EDT 2002 ferguson>
;;;;

(in-package :io)

(defvar *transport* :stdio
  "Defines which message-passing transport is to be used for inter-module
communication.")

(defun initialize (&rest args &key (transport *transport*) &allow-other-keys)
  "Initializes the inter-module communication layer."
  (setq *transport* transport)
  (ecase *transport*
    (:stdio
     (apply #'initialize-stdio args))
    (:internal
     (apply #'initialize-internal args))
    (:socket
     (apply #'initialize-socket args))
    ))

(defun connect (module)
  "Connects the inter-module communication channel for given MODULE."
  (ecase *transport*
    (:stdio
     (connect-stdio module))
    (:internal
     (connect-internal module))
    (:socket
     (connect-socket module))
    ))

(defun send (module msg)
  "Sends message MSG using the current transport."
  (ecase *transport*
    (:stdio
     (send-stdio module msg))
    (:internal
     (send-internal module msg))
    (:socket
     (send-socket module msg))
    ))

(defun recv (module)
  "Returns the next message for this module using the current transport,
or NIL on end-of-file.
Symbols in the message are interned into the current package."
  (ecase *transport*
    (:stdio
     (recv-stdio module))
    (:internal
     (recv-internal module))
    (:socket
     (recv-socket module))
    ))

(defun ready (module)
  "Returns T if a message is waiting for this module, otherwise NIL.
Note that this cannot in general guarantee that a call to RECV will
not block."
  (ecase *transport*
    (:stdio
     (ready-stdio module))
    (:internal
     (ready-internal module))
    (:socket
     (ready-socket module))
    ))

(defun shutdown (module)
  "Shutdowns the inter-module communication layer."
  (ecase *transport*
    (:stdio
     (shutdown-stdio module))
    (:internal
     (shutdown-internal module))
    (:socket
     (shutdown-socket module))
    ))

