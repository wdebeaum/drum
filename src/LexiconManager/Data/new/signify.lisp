;;;;
;;;; w::signify
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
	  (w::signify
	   (wordfeats (W::morph (:forms (-vb) :nom w::significance)))
	   (senses 
	    ((lf-parent ont::correlation) 
	     (templ neutral-neutral-templ)
	     (example "the evidence signifies trobule")
	     (meta-data :origin lam :entry-date 20050422 :change-date 20090604 :comments lam-initial)
	     )	    
	    ((lf-parent ont::correlation)
	     (templ neutral-theme-xp-templ (xp (% w::cp (w::ctype w::s-that))))
	     (meta-data :origin lam :entry-date 20050422 :change-date 20090604 :comments lam-initial)
	     (example "the exmpalation signifies that ...")
	     )	    	    
	    )
	   )
))

