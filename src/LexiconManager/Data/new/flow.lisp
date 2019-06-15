;;;;
;;;; w::flow
;;;;

(define-words :pos W::V 
  :words (
	  (w::flow
	   (wordfeats (W::morph (:forms (-vb) :nom W::flow)))
	   (senses
	    ((lf-parent ont::fluidic-motion)
	     (example "The water flowed [from the source]")
	     (TEMPL AFFECTED-SOURCE-XP-OPTIONAL-TEMPL)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	    
	    ))	
))

