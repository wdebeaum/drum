;;;;
;;;; w::de-energize
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
	  (w::de-energize
	   (senses
	    ((LF-parent ont::change-device-state) (lf-form w::deenergize))
	    ((LF-parent ont::change-device-state) 
	     (templ agent-templ)
	     (Example "Intransitive usage: how do I deenergize "))
	    ))
))

