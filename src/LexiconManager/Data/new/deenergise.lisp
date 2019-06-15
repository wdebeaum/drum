;;;;
;;;; w::deenergise
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	  (w::deenergise
	   (senses
	    ((LF-parent ont::change-device-state) (lf-form w::deenergize))
	    ((LF-parent ont::change-device-state) 
	     (templ agent-templ)
	     (Example "Intransitive usage: how do I deenergize "))
	    ))
))

