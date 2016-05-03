;;;;
;;;; w::energize
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
	  (w::energize
	   (senses
	    ((LF-parent ont::change-device-state))
	    ((lf-parent ONT::evoke-excitement)
	     (templ agent-affected-xp-templ)
	     (syntax (w::resultative +))
	     (example "the film energized him")
	     (meta-data :origin cardiac :entry-date 20080508 :change-date 20090512 :comments LM-vocab)
	     )
	    ))
))

