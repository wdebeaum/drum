;;;;
;;;; handlers.lisp for ChannelKB
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu,  9 Feb 2004
;;;; Time-stamp: <Fri Feb 27 14:46:13 EST 2004 ferguson>
;;;;

(in-package :channelkb)

;;(channel-opened :channel phone-1 :id "Helen")

(defun handle-channel-event (msg event-type &key channel id &allow-other-keys)
  (declare (ignore msg))
  (if (null channel)
      (format *trace-output* "channelkb: channel is ~S~%" channel)
    (case event-type
      ((channel-opened channel-closed)
       (handle-channel-open-close event-type channel id))
      ((channel-pending channel-clear)
       (handle-channel-pending-clear event-type channel id))
      (current-channel
       (kb-retract `(current-channel ?x))
       (kb-assert-fact `(current-channel ,channel)))
      (otherwise
       nil))))

(defun handle-channel-open-close (event-type channel id)
  (let ((status (case event-type
		  (channel-opened 'open)
		  (channel-closed 'closed)
		  (otherwise 'unknown))))
    (kb-retract `(channel-status ,channel ?x))
    (kb-assert-fact `(channel-status ,channel ,status))
    (when id
      (kb-retract `(channel-conversant ,channel ?x))
      (kb-assert-fact `(channel-conversant ,channel ,(intern (string id)))))))

(defun handle-channel-pending-clear (event-type channel id)
  (case event-type
    (channel-pending
     (kb-retract `(channel-clear ,channel))
     (kb-assert-fact `(channel-pending ,channel)))
    (channel-clear
     (kb-retract `(channel-pending ,channel))
     (kb-assert-fact `(channel-clear ,channel)))))

;; (channel-status ?channel ?status)
;; (channel-conversant ?channel ?who)
;; (channel-clear ?channel)
;; (channel-pending ?channel)

(defun handle-ask-channel-query (msg query)
  (multiple-value-bind (results bindings)
      ;; Could redo this to only get one solution...
      (kb-prove query)
    (let ((answer (list 'answer
			:content (when results (car results))
			:bindings (when bindings (car bindings))
			:query query)))
      (dfc:reply-to-msg msg 'tell :content answer))))

(defun handle-ask-all-channel-query (msg query)
  (multiple-value-bind (results bindings)
      (kb-prove query)
    (let ((answer (list 'answer
			:content results
			:bindings bindings
			:query query)))
      (dfc:reply-to-msg msg 'tell :content answer))))

;; Track (location-of) for purposes of communicado

(defun handle-location-of-event (who where)
  (kb-retract `(location-of ,who ?x))
  (kb-assert-fact `(location-of ,who ,where))
  ;; Check communicado and broadcast result
  (multiple-value-bind (results bindings)
      (kb-prove `(communicado ,who ?channel))
    (declare (ignore bindings))
    (let ((content (if results
		       (let* ((result (first results))
			      (channel (third result)))
			 `(:who ,who :what communicado :channel ,channel))
		     `(:who ,who :what incommunicado))))
      (send-msg `(tell
		  :content (comm-event
			    :content (comm-status ,@content)))))))
  
