;;;;
;;;; w::share
;;;;

(define-words :pos W::V 
  :words (
	  (w::share
	   (senses
	    ((lf-parent ont::share)
	     ;;  they shared the cake [with john]
	     (templ agent-affected-co-agent-optional-templ)
	     )
	    ((lf-parent ont::share-property)
	     ;;  they shared the cake [with john]
	     (templ neutral-neutral-templ)
	     )
	    ))
))

