;;;;
;;;; messages.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu,  9 Feb 2004
;;;; Time-stamp: <Wed Apr 23 10:08:13 EDT 2008 ferguson>
;;;;

(in-package :channelkb)

(in-component :channelkb)

(defcomponent-handler
  '(tell &key :content (channel-event &key :content *))
  #'(lambda (msg event)
      (apply #'handle-channel-event msg event))
  :subscribe t)

(defcomponent-handler
  '(ask &key :content (channel-query &key :content *))
  #'(lambda (msg query)
      (handle-ask-channel-query msg query))
  :subscribe t)

(defcomponent-handler
  '(ask-all &key :content (channel-query &key :content *))
  #'(lambda (msg query)
      (handle-ask-all-channel-query msg query))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (pa-event :content (location-of &key :who * :where *)))
  #'(lambda (msg who where)
      (declare (ignore msg))
      (handle-location-of-event who where))
  :subscribe t)
