;;;;
;;;; w::share
;;;;

(define-words :pos W::V 
  :words (
	  (w::share
	   (senses
	    ((lf-parent ont::share)
	     ;;  they shared the cake [with john]
	     (TEMPL AGENT-AFFECTED-AGENT1-XP-OPTIONAL-TEMPL)
	     )
	    ((lf-parent ont::share-property)
	     ;;  they shared the cake [with john]
	     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
	     )
	    ))
))

