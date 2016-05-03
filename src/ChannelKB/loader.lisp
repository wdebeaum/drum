;;;;
;;;; loader.lisp for ChannelKB
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 9 Feb 2004
;;;; Time-stamp: <Mon Feb  9 14:56:08 EST 2004 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;ChannelKB;defsys")

(mk:load-system :channelkb)
