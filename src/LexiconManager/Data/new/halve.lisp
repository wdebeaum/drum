;;;;
;;;; w::halve
;;;;

(define-words :pos W::V 
  :templ agent-affected-xp-templ
 :words (
	  (w::halve
	   (wordfeats (W::morph (:forms (-vb) :ing w::halving)))
	   (senses
	    ((lf-parent ont::cut)
	     (example "Halve the bread")
	     (templ agent-affected-xp-templ)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	    
	    ))
))

