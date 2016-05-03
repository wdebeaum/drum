;;;;
;;;; W::disconnect
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
	  (W::disconnect
	   (SENSES
	    ((LF-PARENT ONT::UNATTACH)
	     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	     (TEMPL AGENT-affected-source-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::from))))
	     )
	    ))
))

