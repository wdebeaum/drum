;;;;
;;;; w::signify
;;;;

(define-words :pos W::V 
  :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
	  (w::signify
	   (wordfeats (W::morph (:forms (-vb) :nom w::significance)))
	   (senses 
	    ((lf-parent ont::correlation) 
	     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
	     (example "the evidence signifies trobule")
	     (meta-data :origin lam :entry-date 20050422 :change-date 20090604 :comments lam-initial)
	     )	    
	    ((lf-parent ont::correlation)
	     (TEMPL NEUTRAL-FORMAL-XP-NP-2-TEMPL (xp (% w::cp (w::ctype w::s-that))))
	     (meta-data :origin lam :entry-date 20050422 :change-date 20090604 :comments lam-initial)
	     (example "the exmpalation signifies that ...")
	     )	    	    
	    )
	   )
))

