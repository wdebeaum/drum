
(in-package :channelkb)

(defun test1 ()
  (dfc:component-receive-msg dfc:*component*
    '(ask :content (channel-query :content (channel-status phone ?x)))))

;;;
;;; PA
;;;

(defun test2 ()
  (dfc:component-receive-msg dfc:*component*
    '(tell :content (physical-event :content (location-of :who helen :where office)))))
  
(defun test2a ()
  (dfc:component-receive-msg dfc:*component*
    '(ask :content (channel-query :content (communicado helen)))))
  
(defun test3 ()
  (dfc:component-receive-msg dfc:*component*
    '(tell :content (physical-event :content (location-of :who helen :where elsewhere)))))

(defun test3a ()
  (dfc:component-receive-msg dfc:*component*
    '(ask :content (channel-query :content (communicado helen)))))
  
;;;
;;; clear/pending
;;;

(defun test4 ()
  (dfc:component-receive-msg dfc:*component*
    '(tell :content (channel-event
		     :content (channel-pending :channel phone)))))
  
(defun test4a ()
  (dfc:component-receive-msg dfc:*component*
    '(ask :content (channel-query :content (channel-clear phone)))))

(defun test4b ()
  (dfc:component-receive-msg dfc:*component*
    '(ask :content (channel-query :content (channel-pending phone)))))

(defun test5 ()
  (dfc:component-receive-msg dfc:*component*
    '(tell :content (channel-event
		     :content (channel-clear :channel phone)))))
  
(defun test5a ()
  (dfc:component-receive-msg dfc:*component*
    '(ask :content (channel-query :content (channel-clear phone)))))

(defun test5b ()
  (dfc:component-receive-msg dfc:*component*
    '(ask :content (channel-query :content (channel-pending phone)))))

