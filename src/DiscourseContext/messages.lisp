;;;;
;;;; File: messages.lisp
;;;; Creator: George Ferguson
;;;; Created: Mon Feb 12 15:22:53 2007
;;;; Time-stamp: <Wed Apr 23 10:05:45 EDT 2008 ferguson>
;;;;

(in-package :dc)

(in-component :discourse-context)

(defcomponent-handler
  '(tell &key :content (new-speech-act *))
  #'(lambda (msg act)
      (handle-tell-new-speech-act msg act))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (get-latest-triple-for-lftype *))
  #'(lambda (msg lftype)
      (handle-request-get-latest-triple-for-lftype msg lftype))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (add-new-utterance-record . *))
    #'(lambda (msg args)
	(format t "~% ADDING ~S" args)
	(let ((res (apply #'add-new-utterance-record args)))
	  (reply-to-msg msg 'tell :content res)))
   :subscribe t)

;; Restart request (doesn't need to be subscribed since it's broadcast)
(defcomponent-handler
  '(request &key :content (restart . *))
  #'(lambda (msg args)
      (apply #'restart-agent args)
      ))

;; START-CONVERSATION was overloaded to mean ``restart''
;; Eventually this should be cleaned up (23 Apr 2008)
(defcomponent-handler
  '(tell &key :content (start-conversation . *))
  #'(lambda (msg args)
      (apply #'restart-agent args)
      ))

(defun restart-agent (&key &allow-other-keys)
  (clear-discourse-history))
