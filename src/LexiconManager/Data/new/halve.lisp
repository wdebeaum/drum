;;;;
;;;; w::halve
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
	  (w::halve
	   (wordfeats (W::morph (:forms (-vb) :ing w::halving)))
	   (senses
	    ((lf-parent ont::cut)
	     (example "Halve the bread")
	     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
	     (meta-data :origin bee :entry-date 20040614 :change-date nil :comments portability-experiment)
	     )	    
	    ))
))

