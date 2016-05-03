;;;;
;;;; defsys.lisp for defcomponent
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 12 Jun 2002
;;;; Time-stamp: <Thu Apr 24 11:47:59 EDT 2008 ferguson>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :io)
  (load #!TRIPS"src;defcomponent;io;defsys"))

(unless (find-package :logging2)
  (load #!TRIPS"src;defcomponent;logging;defsys"))

(defpackage :defcomponent
  (:nicknames :dfc)
  (:use :common-lisp)
  (:export
   ;; Classes
   :trips-component
   :trips-library
   :trips-agent
   ;; Component functions
   :defined-components
   :find-component
   :*component*
   :in-component
   ;; DEFCOMPONENT
   :defcomponent
   :defcomponent-method
   :defcomponent-handler
   :defcomponent-cancellation-pattern
   ;; Component methods
   :compile-component
   :load-component
   :init-component
   :register-component
   :component-is-ready
   :run-component
   :component-receive-msg
   :component-send-msg
   :component-send-with-continuation
   :component-reply-to-msg
   :component-dispatch-message
   :send-msg
   :send-msg-with-continuation
   :reply-to-msg
   :reply-to-msg-with-continuation
   :send-and-wait
   :check-for-cancellation-msg
   ;; Other functions
   :pending-messages-matching-pattern-p
   :signal-message-error
   :*break-on-error*
   :get-keyword-arg
   ;; Symbols used in standard messages (pending better treatment)
   :tell
   :request
   :start-conversation
   :end-conversation
   :restart
   :exit
   ;:chdir exported by logging/logging2
   ))

(mk:defsystem :defcomponent
    :source-pathname #!TRIPS"src;defcomponent;"
    :depends-on (:io :logging2)
    :components ("trips-component"
		 "trips-library"
		 "trips-agent"
		 "defcomponent"
		 "handlers"))

#||
(trace
 defcomponent
 defcomponent-method
 compile-component
 load-component
 init-component
 register-component
 component-is-ready
 run-component
 component-receive-msg
 component-send-msg
 component-send-msg-with-continuation
 component-reply-to-msg
 defcomponent-handler
 component-dispatch-message
 signal-message-error
 )
||#
