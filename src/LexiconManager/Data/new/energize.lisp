;;;;
;;;; w::energize
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	  (w::energize
	   (senses
	    ((LF-parent ont::change-device-state))
	    ((lf-parent ONT::evoke-liveliness)
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     (syntax (w::resultative +))
	     (example "the film energized him")
	     (meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
	     )
	    ))
))

