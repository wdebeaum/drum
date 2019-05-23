;;;;
;;;; W::engage
;;;;

(define-words :pos W::V
  :words (
	  (W::engage
	   (SENSES
	    ((LF-PARENT ONT::evoke-attention)
	     (TEMPL agent-affected-xp-templ)
	     )
	    ((LF-PARENT ONT::employ)
	     (TEMPL agent-affected-xp-templ)
	     )
	    (
	     (LF-PARENT ONT::execute)
	     (TEMPL agent-neutral-xp-templ (xp (% w::pp (w::ptype w::in))))
	     )
	    )
	   )
	  ))

