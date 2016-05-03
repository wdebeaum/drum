;;;;
;;;; File: handlers.lisp
;;;; Creator: George Ferguson
;;;; Created: Mon Feb 12 15:25:02 2007
;;;; Time-stamp: <Thu Feb 15 17:18:10 EST 2007 swift>
;;;;

(in-package :dc)

(defun handle-tell-new-speech-act (msg act)
  "Called when a new-speech-act message arrives. Does not reply."
  (declare (ignore msg))
  (remember-act act))

(defun handle-request-get-latest-triple-for-lftype (msg lftype)
  "Reply to MSG with the most recent instance of the word used with the
given LFTYPE found in the utterance list."
  (let ((answer (get-latest-triple-for-lftype lftype)))
    (dfc:reply-to-msg msg 'tell :content answer)))
