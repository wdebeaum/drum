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
	     (templ affected-source-optional-templ)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	    
	    ))	
))

