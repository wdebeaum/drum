;;;;
;;;; messages.lisp: Message-passing interface to the parserIM
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu


(in-package "IM")

(in-component :im)

(defcomponent-handler
  '(tell &key :content (new-speech-act . *))
  #'(lambda (msg args)
      (apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

(defcomponent-handler
    '(tell &key :content (new-speech-act-hyps . *))
    #'(lambda (msg args)
	(apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

(defcomponent-handler
    '(request &key :content (clear-speech-acts . *))
    #'(lambda (msg args)
	(apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

(defcomponent-handler
    '(request &key :content (release-pending-speech-act . *))
    #'(lambda (msg args)
	(apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

;;  Use this to get the IM to process an LF as though it came form the parser
;;  e.g., (tell :content (NEW-LFS :LFS ((ONT::SPEECHACT V38776 ONT::SA_TELL :CONTENT V38589)
;;                                      (ONT::F V38589 (:* ONT::ACTIVE-PERCEPTION W::SEE) :THEME V38604 :EXPERIENCER V38576)
;;                                      (ONT::PRO V38604 (:* ONT::REFERENTIAL-SEM W::IT) :PROFORM ONT::IT)
;;                                      (ONT::PRO V38576 (:* ONT::PERSON W::I) :PROFORM ONT::I))
;;                                :ROOT V38776 :WORDS (I SEE IT) :CHANNEL DESKTOP)))
 
(defcomponent-handler
  '(tell &key :content (new-lfs . *))
  #'(lambda (msg args)
      (apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (said . *))
  #'(lambda (msg args)
      (apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

(defcomponent-handler
 ;;capability to parse arbitrary text -- se set a low chart size as we expect mostly garbage
;;   and so at best will do phrase spotting
  '(request &key :content (text-to-akrl . *))
  #'(lambda (msg args)
      (reply-to-msg msg
		    'tell :content (apply #'text-to-akrl (list (find-arg args :content)
							       (or (find-arg args :size)
								   501)))))  
	
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (channel-event . *))
  #'(lambda (msg args)
      (apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (started-speaking . *))
  #'(lambda (msg args)
      (apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)

;;  GUI access

(defcomponent-handler
  '(tell &key :content (select . *))
  #'(lambda (msg args)
      (apply #'handle-message (list (find-arg-in-act msg :content))))
  :subscribe t)


(defcomponent-handler
  '(request &key :content (get-kb-def . *))
  #'(lambda (msg args)
      (let* ((id (get-keyword-arg args :id))
	     (result
	      (apply #'get-KB-def (list id))))
	(reply-to-msg msg 'tell :content (list 'kb-def :id id :lf (first result) :context (second result)))))
  :subscribe t)

(defun output (msg)
  (send-msg msg))


;; Restart request (doesn't need to be subscribed since it's broadcast)
(defcomponent-handler
  '(request &key :content (restart . *))
  #'(lambda (msg args)
      (apply #'restart-agent args)
      )
  :subscribe t)

;; START-CONVERSATION was overloaded to mean ``restart''
;; Eventually this should be cleaned up (23 Apr 2008)
(defcomponent-handler
  '(tell &key :content (start-conversation . *))
  #'(lambda (msg args)
      (apply #'restart-agent args)
      )
    :subscribe t)

(defcomponent-handler
  '(tell &key :content (start-paragraph . *))
  #'(lambda (msg args)
      (start-paragraph msg)
      )
    :subscribe t)

(defcomponent-handler
  '(tell &key :content (paragraph-completed . *))
  #'(lambda (msg args)
      (end-paragraph msg))
  :subscribe t)

#||
;;
;; UI message subscription
;;

(defcomponent-handler
  '(tell &key :content (ui-action-performed . *))
  #'(lambda (msg args)
      (apply #'handle-ui-action (list (find-arg-in-act msg :content))))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (cps-action-performed . *))
  #'(lambda (msg args)
      (apply #'handle-cps-action (list (find-arg-in-act msg :content))))
  :subscribe t)

;; currently, a report from an agent but this will be in a GUI message later
(defcomponent-handler
  '(tell &key :content (new-report . *))
  #'(lambda (msg args)
      (apply #'handle-gui-report-notification (list (find-arg-in-act msg :content))))
  :subscribe t)

;; this is here for testing in BOLT

(defcomponent-handler
  '(request &key :content (identify-object . *))
     #'(lambda (msg args)
	 (reply-to-msg msg 'tell :content `(description :id xx :CLASS (:* ONT::BOX W::BOX)
							:PROPERTIES ((:* ONT::RED W::RED)))))
			
   :subscribe t)
||#
