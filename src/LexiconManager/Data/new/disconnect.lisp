;;;;
;;;; W::disconnect
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (W::disconnect
	   (SENSES
	    ((LF-PARENT ONT::UNATTACH)
	     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
	     (TEMPL AGENT-AFFECTED-SOURCE-XP-OPTIONAL-TEMPL (xp (% W::pp (W::ptype W::from))))
	     )
	    ))
))

