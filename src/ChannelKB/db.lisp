;;;;
;;;; db.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu,  9 Feb 2004
;;;; Time-stamp: <Mon Feb  9 14:50:00 EST 2004 ferguson>
;;;;

(in-package :channelkb)

(<- (channel-status phone closed))
(<- (channel-location phone ?where))

(<- (channel-status desktop open))
(<- (channel-location desktop office))

;; The opposite of incommunicado...
(<- (communicado ?who ?channel)
    (location-of ?who ?where)
    (channel-location ?channel ?where)
    (channel-status ?channel open))
