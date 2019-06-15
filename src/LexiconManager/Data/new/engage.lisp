;;;;
;;;; W::engage
;;;;

(define-words :pos W::V
  :words (
	  (W::engage
	   (SENSES
	    ((LF-PARENT ONT::evoke-attention)
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     )
	    ((LF-PARENT ONT::employ)
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     )
	    (
	     (LF-PARENT ONT::execute)
	     (TEMPL agent-neutral-xp-templ (xp (% w::pp (w::ptype w::in))))
	     )
	    )
	   )
	  ))

