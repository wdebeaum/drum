;;;;
;;;; defsys.lisp for ChannelKB
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 9 Feb 2004
;;;; Time-stamp: <Mon Feb  9 21:15:07 EST 2004 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :simplekb)
  (load #!TRIPS"src;SimpleKB;defsys"))

(mk:defsystem :channelkb-code
  :source-pathname #!TRIPS"src;ChannelKB;"
  :components ("handlers"
	       "messages"))

(mk:defsystem :channelkb-data
  :source-pathname #!TRIPS"src;ChannelKB;"
  :load-only t
  :components ("db"))

(dfc:defcomponent :channelkb
  :use (:common-lisp :simplekb)
  :system (:depends-on (:simplekb :channelkb-code :channelkb-data)))

;; Temporary compatibility with Test/test.lisp
(defun run ()
  (dfc:run-component :channelkb))
